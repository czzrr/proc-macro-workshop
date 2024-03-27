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
    //eprintln!("INPUT: {:?}", input);
    let seq = parse_macro_input!(input as Seq);
    //eprintln!("BODY: {:?}", seq.body);
    let start = seq.start.base10_parse::<i64>().unwrap();
    let end = seq.end.base10_parse::<i64>().unwrap();
    let mut repeated_body = TokenStream::new();
    let  body = seq.body.clone();
    for i in start..end {
        let body = body.clone().into_iter();
        let body: TokenStream = body.map(|token| replace_ident_with_num(token, &seq.var, i)).collect(); 
        let body = collapse_indices(body, seq.var.clone());
        //eprintln!("NEW BODY: {:?}", body);
        repeated_body.extend(body);
    }

    repeated_body
}

fn collapse_indices(token_stream: TokenStream, index: Ident) -> TokenStream {
    let mut ts = TokenStream::new();
    let token_stream: Vec<TokenTree>  = token_stream.into_iter().collect();
    let mut i = 0;
    while i < std::cmp::max(0, token_stream.len() as i32 - 2) as usize {
        match token_stream[i].clone() {
            TokenTree::Ident(i1) => {
                match token_stream[i+1].clone() {
                    TokenTree::Punct(punct) if punct.to_string() == "~" => {
                        match token_stream[i+2].clone() {
                            TokenTree::Literal(i2) => {
                                let tt = TokenTree::Ident(proc_macro::Ident::new(&format!("{}{}", i1.to_string(), i2.to_string()), i1.span()));
                                ts.extend(<TokenTree as Into<TokenStream>>::into(tt));
                                i += 3;
                                continue;
                            },
                            _ => ()
                        }
                    },
                    _ => ts.extend(<TokenTree as Into<TokenStream>>::into(TokenTree::Ident(i1))),
                }
            },
            TokenTree::Group(group) => {
                let tss = collapse_indices(group.stream(), index.clone());
                ts.extend(<TokenTree as Into<TokenStream>>::into(TokenTree::Group(Group::new(group.delimiter(), tss))));
            }
            
            tt => ts.extend(<TokenTree as Into<TokenStream>>::into(tt)),
        }
        i += 1;
    }
    if token_stream.len() as i64 - 2 >= 0 {
        ts.extend(<TokenTree as Into<TokenStream>>::into(token_stream[token_stream.len() - 2].clone()));
    }
    if token_stream.len() as i64 - 1 >= 0 {
        ts.extend(<TokenTree as Into<TokenStream>>::into(token_stream[token_stream.len() - 1].clone()));
    }
    

    ts
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
