use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input,
    punctuated::Pair,
    AngleBracketedGenericArguments, Data, DeriveInput, GenericArgument, Ident, Meta,
    Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    //panic!("{:#?}", input);
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
        if is_wrapper_of("Option", field_ty).is_some() {
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

    let repeated_field_decls = fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        //panic!("{:#?}", field_ty);

        if let Some(attr) = field.attrs.iter().next() {
            let meta_list = match attr.meta {
                Meta::List(ref meta_list) => meta_list,
                _ => return quote!()
            };
            //panic!("{:#?}", field_ident.to_string());
            let builder_path = meta_list.path.segments.first();
            let is_builder_inert_attr =
                matches![builder_path, Some(ps) if ps.ident == "builder"];
            if is_builder_inert_attr {
                let inner_ty = is_wrapper_of("Vec", field_ty).expect("expected type Vec<_>");

                //panic!("{:#?}", attr);
                let mut tokens: proc_macro2::token_stream::IntoIter = meta_list.tokens.clone().into_iter();
                //let b1 = matches![tokens.next(), Some(TokenTree::Ident(i)) if i.to_string() == "each"];
                let tkntree = tokens.next().unwrap();
                match tkntree {
                    TokenTree::Ident(ref i) if i.to_string() != "each" => {
                        return syn::Error::new_spanned(attr.meta.clone(), "expected `builder(each = \"...\")`").to_compile_error();
                    },
                    _ => (),
                }
                if !false {
                    //compile_error!("expected #[builder(each = ...)");
                }
                //let b2 = matches![tokens.next(), Some(TokenTree::Punct(p)) if p.as_char() == '='];
                let tkntree = tokens.next().unwrap();
                match tkntree {
                    TokenTree::Punct(ref p) if p.as_char() != '=' => panic!("{:?}, {}", tkntree, p),
                    _ => (),
                }
                let lit = match tokens.next() {
                    Some(lit @ TokenTree::Literal(_)) => format!("{}", lit),
                     _ => panic!(),
                };
                let each = lit.trim_matches('"');
                let each_ident = Ident::new(&format!("{}", each), Span::call_site());
                if each != field_ident.to_string() {
                    let x: TokenStream = quote!(pub fn #each_ident(&mut self, #each_ident: #inner_ty) -> &mut Self {
                        match self.#field_ident {
                            Some(ref mut v) => v.push(#each_ident),
                            None => self.#field_ident = Some(vec![#each_ident]),
                        }
                        self
                    }).into();
                    //panic!("{}", x.to_string());
                    x.into()
                } else {
                    quote!()
                }
            } else {
                panic!()
            }
        } else {
            quote!()
        }
    });

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
            #(#repeated_field_decls)*
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
}

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

