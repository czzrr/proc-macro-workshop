use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;
    //eprintln!("INPUT: {:#?}", input);

    let item = parse_macro_input!(input as syn::Item);
    //eprintln!("ITEM: {:?}", input);
    match item {
        syn::Item::Struct(item) => {
            let struct_def = struct_def_ts(&item);
            let getters = getters_ts(&item);
            let setters = setters_ts(&item);
            let constructor = constructor_ts(&item);
            let struct_ident = &item.ident;
            let impl_block = quote!(
                impl #struct_ident {
                    #constructor
                    #getters
                    #setters
                }
            );

            quote!(
                #struct_def
                #impl_block
            )
            .into()
        }
        _ => syn::Error::into_compile_error(syn::Error::new(item.span(), "expected struct")).into(),
    }
}

fn constructor_ts(item: &syn::ItemStruct) -> TokenStream2 {
    let struct_ident = &item.ident;
    let size = size_ts(item);

    quote!(
        pub fn new() -> Self {
            #struct_ident {
                data: [0; #size],
            }
        }
    )
}

fn getters_ts(item: &syn::ItemStruct) -> TokenStream2 {
    let mut getters = TokenStream2::new();
    let mut bit_offset = quote!(0);
    for field in &item.fields {
        let ty = &field.ty;
        let bit_len = quote!(<#ty as Specifier>::BITS);

        // Create getter.
        let get_ident = syn::Ident::new(
            &format!("get_{}", field.ident.as_ref().unwrap()),
            field.span(),
        );
        let getter = quote!(
            pub fn #get_ident(&self) -> u64 {
                // nice to have:
                // get_bits_at_byte(byte_offset, bit_offset, bit_len)
                let mut bits: u64 = 0;
                let mut byte_offset = (#bit_offset) / 8;
                let local_bit_offset = (#bit_offset) % 8;
                // If bits flow over to the next byte, grab the bits in this byte
                // and process the remaining bytes.
                if local_bit_offset + #bit_len > 8 {
                    let mut rem_len = #bit_len;
                    let mut shift = 0;
                    // Grab the relevant bits.
                    bits |= self.data[byte_offset] as u64 >> local_bit_offset;
                    // Adjust shift and remaining number of bits.
                    shift += 8 - local_bit_offset;
                    rem_len -= 8 - local_bit_offset;

                    // Grab a byte at a time while we can.
                    byte_offset += 1;
                    while rem_len > 8 {
                        bits |= (self.data[byte_offset] as u64) << shift;
                        rem_len -= 8;
                        shift += 8;
                        byte_offset += 1;
                    }
                    // Now there are less than 8 bits left,
                    // and they appear at the start of the last relevant byte.
                    let mask = !(u64::MAX << rem_len);
                    bits |= (self.data[byte_offset] as u64 & mask) << shift;

                    bits
                } else {
                    // All relevant bits are located within a byte.
                    let byte = self.data[byte_offset] as u64;
                    let mask = !(u64::MAX << (#bit_len)) << local_bit_offset;
                    let bits = (byte & mask) >> local_bit_offset;

                    bits as u64
                }
            }
        );
        getters.extend(getter);

        // Update bit offset.
        bit_offset.extend(quote!(+));
        bit_offset.extend(bit_len.clone());
    }

    getters
}

fn setters_ts(item: &syn::ItemStruct) -> TokenStream2 {
    let mut setters = TokenStream2::new();
    let mut bit_offset = quote!(0);
    for field in &item.fields {
        let ty = &field.ty;
        let bit_len = quote!(<#ty as Specifier>::BITS);

        // Create setter.
        let set_ident = syn::Ident::new(
            &format!("set_{}", field.ident.as_ref().unwrap()),
            field.span(),
        );
        let setter = quote!(
            pub fn #set_ident(&mut self, mut n: u64) {
                // nice to have:
                // set_bits_at_byte(byte_offset, bit_offset, bit_len, value))
                let mut byte_offset = (#bit_offset) / 8;
                let local_bit_offset = (#bit_offset) % 8;
                // If bits flow over to the next byte, set the bits in this byte
                // and process the remaining bytes.
                if local_bit_offset + #bit_len > 8 {
                    let mut rem_len = #bit_len;
                    self.data[byte_offset] &= !(u8::MAX << local_bit_offset);
                    self.data[byte_offset] |= (n as u8) << local_bit_offset;
                    n = n >> (8 - local_bit_offset);
                    rem_len -= 8 - local_bit_offset;
                    byte_offset += 1;
                    // Set a byte at a time while we can.
                    while rem_len > 8 {
                        self.data[byte_offset] = n as u8;
                        rem_len -= 8;
                        n = n >> 8;
                        byte_offset += 1;
                    }
                    // All relevant bits are located within a byte.
                    if rem_len == 8 {
                        self.data[byte_offset] = n as u8;
                    } else {
                        self.data[byte_offset] |= !(u8::MAX << rem_len) & (n as u8);
                    }
                } else {
                    // All relevant bits are located within a byte.
                    let bitmask = !(u8::MAX << #bit_len) << local_bit_offset;
                    // Clear.
                    self.data[byte_offset] &= !bitmask as u8;
                    // Set.
                    self.data[byte_offset] |= bitmask & (n << local_bit_offset) as u8;
                }
            }
        );
        setters.extend(setter);

        // Update bit offset.
        bit_offset.extend(quote!(+));
        bit_offset.extend(bit_len.clone());
    }

    setters
}

fn struct_def_ts(item: &syn::ItemStruct) -> TokenStream2 {
    let size = size_ts(&item);
    let struct_ident = &item.ident;
    let struct_def = quote!(
        pub struct #struct_ident {
            data: [u8; #size],
        }
    );

    struct_def
}

fn size_ts(item: &syn::ItemStruct) -> TokenStream2 {
    let mut size = TokenStream2::new();
    size.extend(quote!(0usize));
    for field in &item.fields {
        size.extend(quote!(+));
        let ty = &field.ty;
        let a = quote!(<#ty as Specifier>::BITS);
        size.extend(a);
    }
    let size = quote!((#size) / 8);
    size
}

#[proc_macro]
pub fn define_specifier_impls(_input: TokenStream) -> TokenStream {
    let mut ts = TokenStream::new();
    for i in 1..=64 as usize {
        let b_ident = syn::Ident::new(&format!("B{}", i), proc_macro2::Span::call_site());
        let imp: TokenStream = quote!(
            pub struct #b_ident;
            impl Specifier for #b_ident {
                const BITS: usize = #i;
            }
        )
        .into();
        ts.extend(imp);
    }
    ts
}
