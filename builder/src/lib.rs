extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, Data, DataStruct, DeriveInput, Fields,
    FieldsNamed, GenericArgument, Ident, Lit, Meta, NestedMeta, PathArguments, PathSegment, Token,
    Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
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
            let error_message = format!("{} is not set", name.clone().unwrap().to_string());
            quote! { #name: self.#name.clone().ok_or(#error_message)? }
        }
    });

    let builder_methods = fields.iter().flat_map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let mut methods = vec![];

        if is_vector(&field.ty) {
            let mut create_original = true;
            let inner_type = get_inner_type(ty).unwrap();
            let name_string = name.clone().unwrap().to_string();
            get_each_values(&field.attrs)
                .iter()
                .for_each(|(each_name, each_span)| {
                    if each_name.clone() == name_string {
                        create_original = false;
                    }
                    let each_name_ident = Ident::new(each_name, each_span.clone());
                    methods.push(quote! {
                        fn #each_name_ident(&mut self, value: #inner_type) -> &mut Self {
                            self.#name.push(value);
                            self
                        }
                    });
                });
            if create_original {
                methods.push(quote! {
                    fn #name(&mut self, value: #ty) -> &mut Self {
                        self.#name = value;
                        self
                    }
                });
            }
        } else {
            let value_type = if is_option(&field.ty) {
                get_inner_type(ty).unwrap()
            } else {
                &field.ty
            };
            methods.push(quote! {
                fn #name(&mut self, value: #value_type) -> &mut Self {
                    self.#name = std::option::Option::Some(value);
                    self
                }
            });
        }
        methods
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

fn get_each_values(attrs: &Vec<Attribute>) -> Vec<(String, Span)> {
    attrs
        .iter()
        .filter_map(|attr| {
            let is_builder = match attr.path.get_ident() {
                Some(ident) => ident.to_string() == "builder",
                _ => false,
            };
            if is_builder {
                let meta = attr.parse_meta();
                if let Ok(Meta::List(meta_list)) = meta {
                    return Some(
                        meta_list
                            .nested
                            .iter()
                            .filter_map(|nested_value| {
                                if let NestedMeta::Meta(Meta::NameValue(meta_name_value)) =
                                    nested_value.clone()
                                {
                                    if meta_name_value.path.get_ident().is_some()
                                        && meta_name_value.path.get_ident().unwrap().to_string()
                                            == "each"
                                    {
                                        if let Lit::Str(lit_str) = meta_name_value.lit {
                                            return Some((lit_str.value(), lit_str.span()));
                                        }
                                    }
                                }
                                None
                            })
                            .collect::<Vec<(String, Span)>>(),
                    );
                }
            }
            None
        })
        .flatten()
        .collect::<Vec<(String, Span)>>()
}
