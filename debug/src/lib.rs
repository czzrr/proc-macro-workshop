use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, DataStruct, FieldsNamed, Fields, Field, Expr, Lit, ExprLit, Generics, GenericParam, parse_quote, punctuated::Iter, TypeParam, Type, TypePath, Path, PathArguments, AngleBracketedGenericArguments, GenericArgument};
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    //eprintln!("{:#?}", ast);
    
    let name = &ast.ident;

    let fields = match ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed {
                ref named, ..
            }), ..
        }) => named.iter(),
        _ => unimplemented!()
    };

    let add_debug_fields = fields.clone().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let name_str = name.to_string();

        let format = debug_attr_format(f);
        let formatted_value = match format {
            Some(fmt) => {
                fmt
            }
            None => quote!(&self.#name)
        };

        quote!(
            .field(#name_str, #formatted_value)
        )
    });

    let generics = add_trait_bounds(ast.generics, fields.clone());
    //eprintln!("{:#?}", generics);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let name_str = name.to_string();
    let expanded = quote!(
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                fmt.debug_struct(#name_str)
                #(#add_debug_fields)*
                .finish()
            }
        }
    );

    expanded.into()
}

fn debug_attr_format(f: &Field) -> Option<proc_macro2::TokenStream> {
    if f.attrs.is_empty() {
        return None;
    }
    let attr = &f.attrs[0];
    if f.attrs.len() != 1 {
        return Some(syn::Error::new_spanned(attr, "expected at most one attribute of type `#[debug = \"...\"]`")
            .to_compile_error());
    }
    let err = Some(syn::Error::new_spanned(attr, "expected `#[debug = \"...\"]`")
    .to_compile_error());

    let nv = if let Ok(nv) = attr.meta.require_name_value() {
        nv
    } else {
        return err;
    };

    if !nv.path.is_ident("debug") {
        return err;
    }

    let format = match nv.value {
        Expr::Lit(ExprLit { lit: Lit::Str(ref lit_str), .. }) => lit_str.value(),
        _ => return err
    };

    let name = f.ident.as_ref().unwrap();
    let format_ts = quote!(&format_args!(#format, &self.#name));

    Some(format_ts)
}

fn add_trait_bounds(mut generics: Generics, fields: Iter<'_, Field>) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut ty_param) = param {
            if !only_in_phantom_data(ty_param, &fields) {
                ty_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}

fn only_in_phantom_data(ty_param: &TypeParam, fields: &Iter<'_, Field>) -> bool {
    fields.clone().filter(|f| match f.ty {
        Type::Path(TypePath { path: Path { ref segments, ..}, ..}) => {
            let ps = segments.first().unwrap();
            let x = match ps.arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, ..}) => {
                    match args.first().unwrap() {
                        GenericArgument::Type(Type::Path(TypePath {
                            path: Path {
                                segments, ..
                            }, ..
                        })) => segments.first().unwrap().ident.to_string() == ty_param.ident.to_string(),
                        _ => unimplemented!()
                    }
                },
                _ => false,
            };
            ps.ident == "PhantomData" && x
        },
        _ => unimplemented!()
    }).count() == 1
}