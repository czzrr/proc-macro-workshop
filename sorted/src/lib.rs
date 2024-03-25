use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Item;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    //println!("args: {:?}", args);
    let _ = args;
    let _ = input;
    //println!("input: {:#?}", input);
    let x = input.clone();
    let item = parse_macro_input!(x as Item);
    //sorted_helper(item).unwrap_or_else(syn::Error::into_compile_error).into()
    match sorted_helper(item) {
        Ok(_) => input,
        Err(err) => syn::Error::into_compile_error(err).into()
    }
}

fn sorted_helper(item: Item) -> syn::Result<TokenStream2> {
    match item {
        Item::Enum(item) => {
            let variants: Vec<_> = item.variants.into_iter().collect();
            for i in 0..variants.len() {
                for v in &variants[..i] {
                    if let std::cmp::Ordering::Less = variants[i].ident.cmp(&v.ident) {
                        return Err(syn::Error::new(
                            variants[i].span(),
                            format!("{} should sort before {}", variants[i].ident, v.ident),
                        )) 
                    }
                }
            }
        },
        _ => {
            return Err(syn::Error::new(
                proc_macro::Span::call_site().into(),
                "expected enum or match expression",
            ))
        }
    }

    Ok(TokenStream::new().into())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    TokenStream::new()
}