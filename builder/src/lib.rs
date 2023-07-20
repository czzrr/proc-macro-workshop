use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Pair, AngleBracketedGenericArguments, Data, DeriveInput,
    GenericArgument, Ident, Meta, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;
    let builder_struct_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());

    // Fields of struct.
    let fields = match input.data {
        Data::Struct(s) => s.fields,
        _ => {
            return syn::Error::new(Span::call_site(), "Only structs are supported")
                .to_compile_error()
                .into()
        }
    };

    // Field declarations of the builder type.
    let builder_field_decls = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;

        // If the field is already an Option, do not wrap another Option around it.
        if is_wrapper_of("Option", field_ty).is_some() {
            quote!(#field_ident: #field_ty)
        } else {
            quote!(#field_ident: Option<#field_ty>)
        }
    });

    // Initial field values of builder.
    let builder_field_inits = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        quote!(#field_ident: None)
    });

    // Setter methods of builder.
    let builder_setters = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;

        // If the field is an Option, its inner type is parameter to its setter.
        match is_wrapper_of("Option", field_ty) {
            Some(ty) => quote!(pub fn #field_ident(&mut self, #field_ident: #ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }),
            _ => quote!(pub fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }),
        }
    });

    // Methods for specifying values one at a time.
    let repeated_field_decls = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;

        // Parse attributes of field.
        if let Some(attr) = field.attrs.iter().next() {
            // Error to return when the attribute is in the wrong format.
            let expected_builder_list_err =
                syn::Error::new_spanned(attr.meta.clone(), "expected `builder(each = \"...\")`")
                    .to_compile_error();

            // Inside the attribute, we expect `builder(each = ...)`, which is a meta list.
            let meta_list = match attr.meta {
                Meta::List(ref meta_list) => meta_list,
                _ => return expected_builder_list_err,
            };

            // The path of the meta list is the `builder` of `builder(each = ...)`.
            let builder_path = meta_list.path.segments.first();

            // I think this is guaranteed to be true, as we can't refer to any attributes not specified in the proc macro declaration.
            assert!(matches![builder_path, Some(ps) if ps.ident == "builder"]);

            // Fields annotated with the attribute is of type Vec.
            let inner_ty = is_wrapper_of("Vec", field_ty).expect("field type should be Vec<_>");

            let tokens: Vec<TokenTree> =
                meta_list.tokens.clone().into_iter().collect();
            if tokens.len() != 3 {
                return expected_builder_list_err;
            }

            // Check for `each` and `=`.
            let each = &tokens[0];
            if each.to_string() != "each" {
                return expected_builder_list_err;
            }
            let eq = &tokens[1];
            if eq.to_string() != "=" {
                return expected_builder_list_err;
            }
            
            // If the repeated method name equals the field name, do nothing.
            let repeated_method_name = Ident::new((&tokens[2]).to_string().trim_matches('"'), Span::call_site());
            if repeated_method_name != field_ident.to_string() {
                quote!(pub fn #repeated_method_name(&mut self, #repeated_method_name: #inner_ty) -> &mut Self {
                    match self.#field_ident {
                        Some(ref mut v) => v.push(#repeated_method_name),
                        None => self.#field_ident = Some(vec![#repeated_method_name]),
                    }
                    self
                })
            } else {
                quote!()
            }
        } else {
            quote!()
        }
    });

    // Build the field values.
    let build_checks = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        let field_ident_str = field_ident.to_string();

        if is_wrapper_of("Option", field_ty).is_some() {
            quote!(
                let #field_ident = self.#field_ident.clone();
            )
        } else if is_wrapper_of("Vec", field_ty).is_some() {
            quote!(
                let #field_ident = self.#field_ident.clone().unwrap_or_default();
            )
        } else {
            quote!(
                let #field_ident = self.#field_ident.clone().ok_or(format!("field `{}` not set", #field_ident_str))?;
            )
        }
    });

    let builder_instantiation_values = fields.iter().map(|field| field.ident.as_ref().unwrap());

    let expanded = quote! {
        pub struct #builder_struct_name {
            #(#builder_field_decls),*
        }
        impl #builder_struct_name {
            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(#build_checks)*
                Ok(#struct_name {
                    #(#builder_instantiation_values),*
                })
            }
            #(#builder_setters)*
            #(#repeated_field_decls)*
        }
        impl #struct_name {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #(#builder_field_inits),*
                }
            }
        }
    };
    expanded.into()
}

/// Test if `field_ty` is of wrapper type `wrapper_ty`, e.g., Option or Vec.
fn is_wrapper_of(wrapper_ty: &str, field_ty: &Type) -> Option<Type> {
    let segments = match field_ty {
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
