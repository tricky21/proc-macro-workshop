extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Data, DataStruct, DeriveInput, Fields, FieldsNamed,
    GenericArgument, Ident, PathArguments, PathSegment, Token, Type,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{}Builder", ident), ident.span());

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        panic!("This macro is only applicable to a struct");
    };

    let builder_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option(ty) || is_vector(ty) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let builder_fields_init = fields.iter().map(|field| {
        let name = &field.ident;
        if is_vector(&field.ty) {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: None }
        }
    });

    let builder_fields_build = fields.iter().map(|field| {
        let name = &field.ident;
        if is_option(&field.ty) || is_vector(&field.ty) {
            quote! { #name: self.#name.clone() }
        } else {
            quote! { #name: self.#name.clone().ok_or(" is not set")? }
        }
    });

    let builder_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_vector(&field.ty) {
            quote! {
                fn #name(&mut self, value: #ty) -> &mut Self {
                    self.#name = value;
                    self
                }
            }
        } else {
            let value_type = if is_option(&field.ty) {
                get_inner_type(ty).unwrap()
            } else {
                &field.ty
            };
            quote! {
                fn #name(&mut self, value: #value_type) -> &mut Self {
                    self.#name = std::option::Option::Some(value);
                    self
                }
            }
        }
    });

    let expanded = quote! {
        impl #ident {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields_init,)*
                }
            }
        }

        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #builder_ident {
            #(#builder_methods)*

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#builder_fields_build,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}

fn is_option(ty: &Type) -> bool {
    match get_first_type(&get_types(ty)) {
        Some(type_value) => type_value == "Option",
        None => false,
    }
}

fn is_vector(ty: &Type) -> bool {
    match get_first_type(&get_types(ty)) {
        Some(type_value) => type_value == "Vec",
        None => false,
    }
}

fn get_first_type(types: &Vec<Vec<String>>) -> Option<&String> {
    if types.len() >= 1 {
        types.first().unwrap().last()
    } else {
        None
    }
}

fn get_types(ty: &Type) -> Vec<Vec<String>> {
    let mut types = vec![];
    if let Type::Path(type_path) = ty {
        let segments = get_segments_ident(&type_path.path.segments);
        types.push(segments);
        if let PathArguments::AngleBracketed(ref args) =
            type_path.path.segments.last().unwrap().arguments
        {
            if args.args.first().is_some() {
                if let GenericArgument::Type(ref inner_type) = args.args.first().unwrap() {
                    types = [types, get_types(inner_type)].concat();
                }
            }
        }
    }
    types
}

fn get_segments_ident(segments: &Punctuated<PathSegment, Token![::]>) -> Vec<String> {
    segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>()
        .clone()
}

fn get_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let PathArguments::AngleBracketed(ref args) =
            type_path.path.segments.last().unwrap().arguments
        {
            if args.args.first().is_some() {
                if let GenericArgument::Type(ref inner_type) = args.args.first().unwrap() {
                    return Some(inner_type);
                }
            }
        }
    }
    None
}
