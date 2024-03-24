use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Item;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;
    println!("{:#?}", input);
    let x = input.clone();
    let item = parse_macro_input!(x as Item);

    let _ = sorted_helper(item).unwrap_or_else(syn::Error::into_compile_error);

    input
}

fn sorted_helper(item: Item) -> syn::Result<TokenStream2> {
    match item {
        Item::Enum(item) => (),
        _ => {
            return Err(syn::Error::new(
                item.span(),
                "#[sorted] cannot be applied to things other than enum",
            ))
        }
    }

    Ok(TokenStream::new().into())
}
