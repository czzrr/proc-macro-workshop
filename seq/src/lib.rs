use proc_macro::{Group, Literal, TokenStream, TokenTree};
use syn::{parse::Parse, parse_macro_input, Ident, Token};

struct Seq {
    var: syn::Ident,
    start: syn::LitInt,
    end: syn::LitInt,
    body: TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start: syn::LitInt = input.parse()?;
        let _: Token![..] = input.parse()?;
        let end: syn::LitInt = input.parse()?;
        let group: proc_macro2::Group = input.parse()?;
        let body = group.stream().into();

        Ok(Seq {var, start, end, body})
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);
    let start = seq.start.base10_parse::<i64>().unwrap();
    let end = seq.end.base10_parse::<i64>().unwrap();
    let mut repeated_body = TokenStream::new();
    for i in start..end {
        let body = seq.body.clone();
        let body = body.into_iter();
        let body: TokenStream = body.map(|token| replace_ident_with_num(token, &seq.var, i)).collect(); 
        repeated_body.extend(body);
    }

    repeated_body
}

fn replace_ident_with_num(token_tree: TokenTree, ident: &Ident, n: i64) -> TokenTree {
    match token_tree {
        TokenTree::Ident(i) if i.to_string() == ident.to_string() => TokenTree::Literal(Literal::i64_unsuffixed(n)),
        TokenTree::Group(token_tree) => {
            let mapped: TokenStream = token_tree.stream().into_iter().map(|token| replace_ident_with_num(token, ident, n)).collect();
            TokenTree::Group(Group::new(token_tree.delimiter(), mapped))
        },
        tt => tt
    }
}
