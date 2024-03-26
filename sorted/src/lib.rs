use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, ItemFn};
use syn::{Item, Pat};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;
    let x = input.clone();
    let item = parse_macro_input!(x as Item);
    match sorted_helper(item) {
        Ok(_) => input,
        Err(err) => {
            let err = syn::Error::into_compile_error(err);
            let mut input = input;
            input.extend::<TokenStream>(err.into());
            input
        }
    }
}

fn sorted_helper(item: Item) -> syn::Result<()> {
    match item {
        Item::Enum(item) => {
            let variants: Vec<_> = item.variants.into_iter().collect();
            for i in 0..variants.len() {
                for v in &variants[..i] {
                    if let std::cmp::Ordering::Less = variants[i].ident.cmp(&v.ident) {
                        return Err(syn::Error::new(
                            variants[i].span(),
                            format!("{} should sort before {}", variants[i].ident, v.ident),
                        ));
                    }
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                proc_macro::Span::call_site().into(),
                "expected enum or match expression",
            ))
        }
    }

    Ok(())
}

struct FnMatchSortedCheck {
    violator: Option<TokenStream2>,
}

impl FnMatchSortedCheck {
    pub fn new() -> Self {
        FnMatchSortedCheck { violator: None }
    }
}

impl VisitMut for FnMatchSortedCheck {
    fn visit_expr_match_mut(&mut self, item: &mut syn::ExprMatch) {
        let found = item
            .attrs
            .iter()
            .position(|a| a.path().get_ident().unwrap() == "sorted");
        if let Some(idx) = found {
            item.attrs.remove(idx);
        }
        let mut v = Vec::new();
        for arm in &item.arms {
            match &arm.pat {
                Pat::TupleStruct(pat) => {
                    let mut segments = Vec::new();
                    for segment in pat.path.segments.iter() {
                        segments.push(segment.ident.to_string());
                    }
                    let s = segments.join("::");
                    v.push((s, pat.path.clone()))
                }
                _ => (),
            }
            for i in 0..v.len() {
                for (pat1, _) in &v[..i] {
                    let (pat2, span2) = &v[i];
                    if let std::cmp::Ordering::Less = pat2.cmp(pat1) {
                        self.violator = Some(
                            syn::Error::new_spanned(
                                span2,
                                format!("{} should sort before {}", pat2, pat1),
                            )
                            .into_compile_error(),
                        );
                    }
                }
            }
        }
    }
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;
    let mut item_fn = parse_macro_input!(input as ItemFn);
    let mut check = FnMatchSortedCheck::new();
    check.visit_item_fn_mut(&mut item_fn);
    if let Some(err) = check.violator {
        quote!(
            #item_fn
            #err)
        .into()
    } else {
        quote!(#item_fn).into()
    }
}
