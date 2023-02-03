fn parse_generic_type<'a>(tp: &'a syn::TypePath, expected_type: &str) -> Option<&'a syn::Type> {
    if tp.path.segments[0].ident == expected_type {
        if let syn::PathArguments::AngleBracketed(generic_args) = &tp.path.segments[0].arguments {
            if let syn::GenericArgument::Type(ty) = &generic_args.args[0] {
                return Some(ty);
            }
        }
    }
    None
}

fn get_attr_modifier(
    attrs: &Vec<syn::Attribute>,
) -> Result<Option<syn::Ident>, proc_macro::TokenStream> {
    for attr in attrs {
        if let Ok(syn::Meta::List(meta)) = attr.parse_meta() {
            if meta.path.get_ident().is_none() {
                continue;
            }
            if meta.path.get_ident().unwrap() != "builder" {
                continue;
            }

            for nested in meta.nested {
                if let syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) = nested {
                    if let Some(ident) = name_value.path.get_ident() {
                        if ident == "each" {
                            if let syn::Lit::Str(lit_str) = name_value.lit {
                                return Ok(Some(syn::Ident::new(
                                    &lit_str.value(),
                                    proc_macro2::Span::call_site(),
                                )));
                            }
                        }
                        let mut tokens = proc_macro2::TokenStream::new();
                        quote::ToTokens::to_tokens(&attr.path, &mut tokens);
                        quote::ToTokens::to_tokens(&attr.tokens, &mut tokens);
                        return Err(syn::Error::new_spanned(tokens, "expected `builder(each = \"...\")`")
                            .to_compile_error()
                            .into());
                    }
                }
            }
        }
    }
    Ok(None)
}

fn macro_error(reason: &str) -> proc_macro::TokenStream {
    quote::quote! {
        compile_error!(#reason);
    }
    .into()
}

struct Fields {
    vis: Vec<syn::Visibility>,
    names: Vec<syn::Ident>,
    types: Vec<syn::Type>,
}

impl Fields {
    pub fn new() -> Self {
        Self {
            vis: vec![],
            names: vec![],
            types: vec![],
        }
    }

    pub fn push(&mut self, vis: syn::Visibility, name: syn::Ident, ty: syn::Type) {
        self.vis.push(vis);
        self.names.push(name);
        self.types.push(ty);
    }

    pub fn release(self) -> (Vec<syn::Visibility>, Vec<syn::Ident>, Vec<syn::Type>) {
        (self.vis, self.names, self.types)
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // main identifiers
    let ident = input.ident;
    let builder_ident = quote::format_ident!("{}Builder", ident);

    // retrieve DataStruct
    let data = if let syn::Data::Struct(d) = input.data {
        d
    } else {
        return macro_error("Only structs supported");
    };

    // retrieve Fields
    let fields = &data.fields;
    if let syn::Fields::Named(_) = fields {
    } else {
        return macro_error("Unsupported fields");
    };

    // this is where to store parsed info
    let mut scalars = Fields::new();
    let mut vectors = Fields::new();
    let mut helpers = Fields::new();
    let mut optionals = Fields::new();
    let mut others = Fields::new();

    // parse fields of the struct
    for field in fields.iter() {
        if field.ident.is_none() {
            return macro_error("Fields without names not allowed");
        }

        let attrs = &field.attrs;
        let name = field.ident.as_ref().unwrap();
        let vis = field.vis.clone();
        let ty = field.ty.clone();

        if let syn::Type::Path(tp) = &ty {
            // is #[builder(each = "arg")] provided?
            let attr_modifier = match get_attr_modifier(attrs) {
                Ok(opt) => opt,
                Err(token_stream) => {
                    return token_stream;
                }
            };

            // is attribute applied to Vec<T>?
            let vec_managed = if let Some(vec_type) = parse_generic_type(tp, "Vec") {
                if let Some(ident_for_scalars) = attr_modifier {
                    scalars.push(vis.clone(), ident_for_scalars.clone(), vec_type.clone());
                    vectors.push(vis.clone(), name.clone(), ty.clone());

                    if name != &ident_for_scalars {
                        helpers.push(vis.clone(), name.clone(), ty.clone());
                    }
                    true
                } else {
                    false
                }
            } else {
                false
            };

            // Optional fields or others required
            if !vec_managed {
                if let Some(opt_type) = parse_generic_type(tp, "Option") {
                    optionals.push(vis, name.clone(), opt_type.clone());
                } else {
                    others.push(vis, name.clone(), ty);
                }
            }
        } else {
            return macro_error("syn::Type::Path only allowed as a type of a field");
        }
    }

    // prepare data for code generating
    let (scalars_vis, scalars_names, scalars_types) = scalars.release();
    let (vectors_vis, vectors_names, vectors_types) = vectors.release();
    let (helpers_vis, helpers_names, helpers_types) = helpers.release();
    let (optionals_vis, optionals_names, optionals_types) = optionals.release();
    let (others_vis, others_names, others_types) = others.release();

    let names = vectors_names
        .iter()
        .chain(optionals_names.iter())
        .chain(others_names.iter());

    // code generating
    quote::quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#vectors_names: vec![],)*
                    #(#optionals_names: None,)*
                    #(#others_names: None),*
                }
            }
        }

    pub struct #builder_ident {
            #(#vectors_vis #vectors_names: #vectors_types,)*
            #(#optionals_vis #optionals_names: std::option::Option<#optionals_types>,)*
            #(#others_vis #others_names: std::option::Option<#others_types>),*
        }

        impl #builder_ident {
            // scalars
            #(#scalars_vis fn #scalars_names(&mut self, #scalars_names: #scalars_types) -> &mut Self {
                self.#vectors_names.push(#scalars_names);
                self
            })*

            // vectors
            #(#helpers_vis fn #helpers_names(&mut self, mut #helpers_names: #helpers_types) -> &mut Self {
                self.#helpers_names.append(&mut #helpers_names);
                self
            })*

            // optionals
            #(#optionals_vis fn #optionals_names(&mut self, #optionals_names: #optionals_types) -> &mut Self {
                self.#optionals_names = std::option::Option::Some(#optionals_names);
                self
            })*

            // others
            #(#others_vis fn #others_names(&mut self, #others_names: #others_types) -> &mut Self {
                self.#others_names = std::option::Option::Some(#others_names);
                self
            })*

            pub fn build(&mut self) -> std::result::Result<#ident, &str> {
                if (
                        #(self.#others_names.is_some())&*
                ) {
                    #(let mut #vectors_names = vec![];)*
                    #(#vectors_names.append(&mut self.#vectors_names);)*
                    #(let #optionals_names = if (self.#optionals_names.is_some()) {
                        self.#optionals_names.take()
                    } else {
                        std::option::Option::None
                    };)*
                    #(let #others_names = self.#others_names.take().unwrap();)*

                    std::result::Result::Ok(#ident {
                        #(#names),*
                    })
                } else {
                    std::result::Result::Err("construction failure")
                }
            }
        }
    }
    .into()
}
