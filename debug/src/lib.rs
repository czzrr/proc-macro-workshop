use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, DataStruct, FieldsNamed, Fields, Field, Expr, Lit, ExprLit};
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
                quote!(&format_args!(#fmt, &self.#name))
            }
            None => quote!(&self.#name)
        };

        quote!(
            .field(#name_str, #formatted_value)
        )
    });

    let name_str = name.to_string();
    let expanded = quote!(
        impl std::fmt::Debug for #name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                fmt.debug_struct(#name_str)
                #(#add_debug_fields)*
                .finish()
            }
        }
    );

    expanded.into()
}

fn debug_attr_format(f: &Field) -> Option<String> {
    if f.attrs.is_empty() {
        return None;
    }
    if f.attrs.len() != 1 {
        panic!();
    }
    let attr = &f.attrs[0];
    let nv = attr.meta.require_name_value().unwrap();
    if !nv.path.is_ident("debug") {
        panic!();
    }
    let format = match nv.value {
        Expr::Lit(ExprLit { lit: Lit::Str(ref lit_str), .. }) => lit_str.value(),
        _ => panic!()
    };
    Some(format)
}