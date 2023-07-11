use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{parse_macro_input, DeriveInput, Ident, Data, GenericArgument, AngleBracketedGenericArguments, PathArguments, PathSegment, Path, TypePath, Type, punctuated::Pair};
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let fields = match input.data {
        Data::Struct(s) => s.fields,
        _ => panic!("Only structs are supported"),
    };
    // let fields_named = match fields {
    //     Fields::Named(f) => f,
    //     _ => panic!("Only structs with named fields are supported"),
    // };

    let builder_field_decls = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        if is_option_type(field_ty).is_some() {
            quote!(#field_ident: #field_ty)
        } else {
            quote!(#field_ident: Option<#field_ty>)
        }
    });

    let builder_fields_init = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        quote!(#field_ident: None)
    });

    let setters = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        
        match is_option_type(field_ty) {
            Some(ty) => quote!(pub fn #field_ident(&mut self, #field_ident: #ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }),
            _ => quote!(pub fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            })
        }
    });

    let build_checks = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        let field_ident_str = field_ident.to_string();

        if is_option_type(field_ty).is_some() {
            quote!(
                let #field_ident = self.#field_ident.clone();
            )
        } else {
            quote!(
                let #field_ident = self.#field_ident.clone().ok_or(format!("field `{}` not set", #field_ident_str))?;
            )
        }
    });

    let build = fields.iter().map(|field| field.ident.as_ref().unwrap());

    let expanded = quote! {
        pub struct #builder_name {
            #(#builder_field_decls),*
        }
        impl #builder_name {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(#build_checks)*
                Ok(#name {
                    #(#build),*
                })
            }
            #(#setters)*
        }
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_fields_init),*
                }
            }
        }
    };
    expanded.into()
    //unimplemented!()
}

fn is_option_type(field_ty: &Type) -> Option<Type> {
    let segments = match field_ty {
        Type::Path(
            TypePath {
                qself: None,
                path: Path {
                    segments, ..
                },
            },
        ) => segments,
        _ => return None,
    };
    
    let generic_args = match segments.pairs().next() {
        Some(Pair::End(
            PathSegment {
                ident: i,
                arguments: PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments {
                        args, ..
                    },
                ),
            })) if i.to_string() == "Option" => args,
        _ => return None,
    };

    match generic_args.pairs().next() {
        Some(Pair::End(GenericArgument::Type(ty))) => Some(ty.clone()),
        _ => None
    }
}
