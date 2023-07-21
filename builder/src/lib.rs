use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Pair, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Field, Fields, GenericArgument, Ident, Meta, Path, PathArguments, PathSegment,
    Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());

    // Fields of struct.
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(ref fields_named),
        ..
    }) = ast.data
    {
        fields_named.named.iter()
    } else {
        return syn::Error::new(
            Span::call_site(),
            "Only structs with named fields are supported",
        )
        .to_compile_error()
        .into();
    };

    // Field declarations of the builder type.
    let builder_fields = fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        // If the field is already an Option, do not wrap another Option around it.
        if wrapper("Option", ty).is_some() {
            quote!(#name: #ty)
        } else {
            quote!(#name: std::option::Option<#ty>)
        }
    });

    // Initial field values of builder.
    let builder_init_values = fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote!(#name: std::option::Option::None)
    });

    // Setter methods of builder.
    let methods = fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let method = quote!(pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = std::option::Option::Some(#name);
            self
        });

        // If the field is an Option, its inner type is parameter to its setter.
        if let Some(inner_ty) = wrapper("Option", ty) {
            quote!(pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            })
        } else {
            let (conflict, repeated_field_method) = builder_of(field);
            // If there is a conflict between the field name and the annotated `each` repeated field method name,
            // only generate the repeated field method.
            if conflict {
                repeated_field_method
            } else {
                quote!(#repeated_field_method
                #method)
                .to_token_stream()
                .into()
            }
        }
    });

    // Build the field values.
    let build_fields = fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let name_str = name.to_string();

        if wrapper("Option", ty).is_some() {
            quote!(
                #name: self.#name.clone()
            )
        } else if wrapper("Vec", ty).is_some() {
            quote!(
                #name: self.#name.clone().unwrap_or_default()
            )
        } else {
            quote!(
                #name: self.#name.clone().ok_or(format!("field `{}` not set", #name_str))?
            )
        }
    });

    let expanded = quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }
        impl #builder_name {
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_fields),*
                })
            }
            #(#methods)*
        }
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init_values),*
                }
            }
        }
    };
    expanded.into()
}

/// If `ty` is a wrapper of type `wrapper_ty`, return the inner type.
/// For example, Option<T> returns T.
fn wrapper(wrapper_ty: &str, ty: &Type) -> Option<Type> {
    let segments = match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
        }) => segments,
        _ => return None,
    };

    let generic_args = match segments.pairs().next() {
        Some(Pair::End(PathSegment {
            ident: i,
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        })) if i.to_string() == wrapper_ty => args,
        _ => return None,
    };

    match generic_args.pairs().next() {
        Some(Pair::End(GenericArgument::Type(ty))) => Some(ty.clone()),
        _ => None,
    }
}

/// Check if the given field is annotated with the builder attribute
/// and return the repeated field method name if so.
fn builder_of(field: &Field) -> (bool, proc_macro2::TokenStream) {
    if field.attrs.len() != 1 {
        return (false, quote!());
    }
    let attr = &field.attrs[0];

    // Error to return when the attribute is in the wrong format.
    let expected_builder_list_err =
        syn::Error::new_spanned(attr.meta.clone(), "expected `builder(each = \"...\")`")
            .to_compile_error();

    if attr.meta.path().get_ident().unwrap() != "builder" {
        return (false, expected_builder_list_err);
    }

    // Inside the attribute, we expect `builder(each = ...)`, which is a meta list.
    let meta_list = match attr.meta {
        Meta::List(ref meta_list) => meta_list,
        _ => return (false, expected_builder_list_err),
    };

    let tokens: Vec<TokenTree> = meta_list.tokens.clone().into_iter().collect();

    if tokens.len() != 3 {
        return (false, expected_builder_list_err);
    }

    // Check for `each` and `=`.
    let each = &tokens[0];
    if each.to_string() != "each" {
        return (false, expected_builder_list_err);
    }
    let eq = &tokens[1];
    if eq.to_string() != "=" {
        return (false, expected_builder_list_err);
    }

    let repeated_field_name = Ident::new(
        (&tokens[2]).to_string().trim_matches('"'),
        Span::call_site(),
    );

    let name = field.ident.as_ref().unwrap();
    let ty = &field.ty;
    let inner_ty = wrapper("Vec", ty).unwrap();
    let repeated_field_method = quote!(pub fn #repeated_field_name(&mut self, #repeated_field_name: #inner_ty) -> &mut Self {
        match self.#name {
            std::option::Option::Some(ref mut v) => v.push(#repeated_field_name),
            std::option::Option::None => self.#name = Some(vec![#repeated_field_name]),
        }
        self
    });

    (&repeated_field_name == name, repeated_field_method)
}
