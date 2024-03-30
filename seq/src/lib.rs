use proc_macro::{Delimiter, Group, Literal, TokenStream, TokenTree};
use syn::{parse::Parse, parse_macro_input, Token};

struct Seq {
    var: syn::Ident,
    start: usize,
    end: usize,
    body: TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start = input
            .parse::<syn::LitInt>()?
            .base10_parse::<usize>()
            .unwrap();
        let _: Token![..] = input.parse()?;
        let end = if let Ok(_) = input.parse::<Token![=]>() {
            input
                .parse::<syn::LitInt>()?
                .base10_parse::<usize>()
                .unwrap()
                + 1
        } else {
            input
                .parse::<syn::LitInt>()?
                .base10_parse::<usize>()
                .unwrap()
        };

        let group: proc_macro2::Group = input.parse()?;
        let body = group.stream().into();

        Ok(Seq {
            var,
            start,
            end,
            body,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    //eprintln!("INPUT: {:?}", input);

    let cloned_input = input.clone();
    let seq = parse_macro_input!(cloned_input as Seq);

    //eprintln!("BODY: {:?}", seq.body);

    // 1. Try to find a repeat section and repeat it the correct number of times.
    // 2. If no repeat sections were found, repeat the whole `seq.body` the correct number of times.
    let mut repeated = false;
    let ts_repeated_sections = seq.repeat_section(seq.body.clone(), &mut repeated);
    if repeated {
        //eprintln!("REPEATED_SECTIONS: {:?}", ts_repeated_sections);
        ts_repeated_sections
    } else {
        // Repeat the whole body.
        let mut repeated_body = TokenStream::new();
        for i in seq.start..seq.end {
            let body = seq.body.clone();
            let body: TokenStream = body
                .into_iter()
                .map(|token| replace_ident_with_num(token, &seq.var, i))
                .collect();
            let body = seq.collapse_indices(&body);
            //eprintln!("NEW BODY: {:?}", body);
            repeated_body.extend(body);
        }
        //eprintln!("REPEATED_BODY: {:?}", repeated_body);
        repeated_body
    }
}

impl Seq {
    fn repeat_section(&self, ts: TokenStream, repeated: &mut bool) -> TokenStream {
        let ts: Vec<TokenTree> = ts.clone().into_iter().collect();
        let mut token_trees: Vec<TokenTree> = Vec::new();
        let mut i = 0;
        while i < ts.len() {
            // Look for '#', a parenthesized group, and '*'.
            match &ts[i] {
                punct @ TokenTree::Punct(prefix_punct)
                    if prefix_punct.as_char() == '#' && i + 2 < ts.len() =>
                {
                    match &ts[i + 1] {
                        TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                            match &ts[i + 2] {
                                TokenTree::Punct(suffix_punct) if suffix_punct.as_char() == '*' => {
                                    // We found the token trees for a repeat section.
                                    // Now we repeat the section the desired number of times.
                                    let mut repeated_sections = TokenStream::new();
                                    for n in self.start..self.end {
                                        let repeated_section: TokenStream = group
                                            .stream()
                                            .into_iter()
                                            .map(|tt| replace_ident_with_num(tt, &self.var, n))
                                            .collect();
                                        let repeated_section =
                                            self.collapse_indices(&repeated_section);
                                        repeated_sections.extend(repeated_section);
                                    }
                                    token_trees.extend(
                                        repeated_sections.into_iter().collect::<Vec<TokenTree>>(),
                                    );
                                    *repeated = true;
                                    i += 2;
                                }
                                _ => token_trees.push(punct.clone()),
                            }
                        }
                        _ => token_trees.push(punct.clone()),
                    }
                }
                TokenTree::Group(group) => {
                    let ts = self.repeat_section(group.stream(), repeated);
                    token_trees.push(TokenTree::Group(Group::new(group.delimiter(), ts)));
                }
                tt => token_trees.push(tt.clone()),
            }
            i += 1;
        }
        let ts = token_trees.into_iter().collect();
        ts
    }

    fn collapse_indices(&self, token_stream: &TokenStream) -> TokenStream {
        let mut augmented_token_stream = TokenStream::new();
        let token_stream: Vec<TokenTree> = token_stream.clone().into_iter().collect();
        let mut i = 0;
        while i < token_stream.len() {
            match &token_stream[i] {
                TokenTree::Ident(ident) if i + 2 < token_stream.len() => {
                    if let Some(augmented_ident) =
                        collapse_ident(&ident, &token_stream[i + 1], &token_stream[i + 2])
                    {
                        augmented_token_stream.extend(<TokenTree as Into<TokenStream>>::into(
                            TokenTree::Ident(augmented_ident),
                        ));
                        i += 2;
                    } else {
                        augmented_token_stream.extend(<TokenTree as Into<TokenStream>>::into(
                            TokenTree::Ident(ident.clone()),
                        ));
                    }
                }
                TokenTree::Group(group) => {
                    let tss = self.collapse_indices(&group.stream());
                    augmented_token_stream.extend(<TokenTree as Into<TokenStream>>::into(
                        TokenTree::Group(Group::new(group.delimiter(), tss)),
                    ));
                }
                tt => augmented_token_stream
                    .extend(<TokenTree as Into<TokenStream>>::into(tt.clone())),
            }
            i += 1;
        }

        augmented_token_stream
    }
}

fn collapse_ident(
    ident: &proc_macro::Ident,
    maybe_tilde: &TokenTree,
    maybe_seq_var: &TokenTree,
) -> Option<proc_macro::Ident> {
    match maybe_tilde {
        TokenTree::Punct(punct) if punct.to_string() == "~" => match maybe_seq_var {
            TokenTree::Literal(lit) => {
                let augmented_ident = proc_macro::Ident::new(
                    &format!("{}{}", ident.to_string(), lit.to_string()),
                    ident.span(),
                );

                Some(augmented_ident)
            }
            _ => None,
        },
        _ => None,
    }
}

fn replace_ident_with_num(token_tree: TokenTree, ident: &syn::Ident, n: usize) -> TokenTree {
    match token_tree {
        TokenTree::Ident(i) if i.to_string() == ident.to_string() => {
            TokenTree::Literal(Literal::usize_unsuffixed(n))
        }
        TokenTree::Group(token_tree) => {
            let mapped: TokenStream = token_tree
                .stream()
                .into_iter()
                .map(|token| replace_ident_with_num(token, ident, n))
                .collect();
            TokenTree::Group(Group::new(token_tree.delimiter(), mapped))
        }
        tt => tt,
    }
}
