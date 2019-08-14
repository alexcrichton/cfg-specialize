extern crate proc_macro;

use proc_macro2::{Delimiter, Ident, Span, TokenStream, TokenTree};
use quote::ToTokens;
use quote::{quote, quote_spanned};
use syn::fold::Fold;
use syn::*;

#[proc_macro_attribute]
pub fn cfg_specialize(
    attribute: proc_macro::TokenStream,
    function: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let cfgs = match get_cfgs(attribute) {
        Some(cfgs) => cfgs,
        None => {
            panic!(
                "the `cfg_specialize` attribute must be of the form: \
                 #[cfg_specialize(...)]"
            );
        }
    };

    let ItemFn {
        vis,
        block,
        sig,
        attrs,
        ..
    } = match syn::parse::<syn::Item>(function).unwrap() {
        Item::Fn(item) => item,
        _ => panic!("#[cfg_specialize] can only be applied to functions"),
    };

    let Signature {
        abi,
        ident,
        unsafety,
        constness,
        inputs,
        output,
        variadic,
        generics,
        ..
    } = sig;
    let ref inputs = inputs;
    let where_clause = &generics.where_clause;

    if unsafety.is_none() {
        // TODO: shouldn't require this
        panic!("can only be applied to unsafe functions for now");
    }

    if variadic.is_some() {
        panic!(
            "the #[cfg_specialize] attribute cannot be applied to \
             variadic functions"
        );
    }

    if constness.is_some() {
        panic!(
            "the #[cfg_specialize] attribute cannot be applied to \
             const functions"
        );
    }

    let ref inputs = inputs;
    let output = match output {
        ReturnType::Type(_, t) => t,
        ReturnType::Default => Box::new(
            TypeTuple {
                elems: Default::default(),
                paren_token: Default::default(),
            }
            .into(),
        ),
    };

    // We've got to get a bit creative with our handling of arguments. For a
    // number of reasons we translate this:
    //
    //      fn foo(ref a: u32) -> u32 {
    //          // ...
    //      }
    //
    // into roughly:
    //
    //      fn foo(__arg_0: u32) -> u32 {
    //          // ..
    //
    //          fn foo_sub(ref a: u32) -> u32 {
    //              // ...
    //          }
    //      }
    //
    // The intention here is to ensure that all local function variables get
    // transferred correctly to the actual definitions.
    //
    // Note that due to the fact that we're generating new functions here we
    // don't support methods.
    let mut inputs_no_patterns = Vec::new();
    let mut patterns = Vec::new();
    let mut temp_bindings = Vec::new();
    let mut fnty = TypeBareFn {
        lifetimes: None,
        unsafety: unsafety.clone(),
        abi: None,
        fn_token: Default::default(),
        paren_token: Default::default(),
        inputs: Default::default(),
        variadic: None,
        output: ReturnType::Type(Default::default(), output.clone()),
    };
    for (i, input) in inputs.iter().enumerate() {
        match input {
            // `a: B`
            syn::FnArg::Typed(PatType { pat, ty, .. }) => {
                patterns.push(pat);
                let ident = Ident::new(&format!("__arg_{}", i), Span::call_site());
                temp_bindings.push(ident.clone());
                let pat = PatIdent {
                    attrs: Vec::new(),
                    by_ref: None,
                    mutability: None,
                    ident: ident,
                    subpat: None,
                };
                inputs_no_patterns.push(FnArg::Typed(PatType {
                    attrs: Vec::new(),
                    pat: Box::new(pat.into()),
                    ty: ty.clone(),
                    colon_token: Default::default(),
                }));
                fnty.inputs.push(BareFnArg {
                    attrs: Vec::new(),
                    name: None,
                    ty: *ty.clone(),
                });
            }

            _ => {
                panic!(
                    "the #[cfg_specialize] attribute cannot be applied \
                     to methods, only free functions"
                );
            }
        }
    }

    // We use a `static` below for dispatching this function, and there's only
    // one static per function, so we can't support generics.
    if generics.params.len() > 0 {}
    let mut lifetimes = punctuated::Punctuated::new();
    for param in generics.params.iter() {
        match *param {
            GenericParam::Type(_) => {
                panic!(
                    "the #[cfg_specialize] attribute cannot be applied to \
                     generic functions"
                );
            }
            GenericParam::Lifetime(ref l) => {
                lifetimes.push(l.clone());
            }
            GenericParam::Const(_) => {
                panic!(
                    "the #[cfg_specialize] attribute cannot be applied to \
                     functions with constant parameters"
                );
            }
        }
    }
    if lifetimes.len() > 0 {
        fnty.lifetimes = Some(BoundLifetimes {
            for_token: Default::default(),
            lt_token: Default::default(),
            lifetimes,
            gt_token: Default::default(),
        });
    }

    // For each of the #[cfg] listed inside of the `#[cfg_specialize]`
    // attribute we want to generate a unique function.
    //
    // Each function will have its respective `#[target_feature]` enabled and
    // will have `cfg!(target_feature = "..")` replaced with essentially "true"
    // to ensure that it works. Each of these functions will then be selected
    // amongst at runtime.
    let mut functions = Vec::new();
    let mut checker = Vec::new();
    let mut checker_impl = Vec::new();
    for cfg in cfgs {
        // First up let's figure out what `Feature` this corresponds to.
        let feature = parse_cfg(cfg);

        // This'll be the name of the function, which is the same as the name
        // of the original function but suffixed with our feature name, e.g.
        // `foo_avx`.
        let feature_ident = feature.replace(".", "_");
        let ident = Ident::new(&format!("__{}_{}", ident, feature_ident), Span::call_site());

        // Get rid of `cfg!(target_feature = ...)` by replacing it with `true`
        // where we can. Maybe one day this'll be fixed in the compiler and we
        // won't have to do it!.
        let body = fold_body(*block.clone(), &feature);

        // Package all that up in a function, and save off some of this
        // information.
        functions.push(quote! {
            #[target_feature(enable = #feature)]
            #unsafety
            fn #ident #generics(#inputs) -> #output #where_clause
                #body
        });
        checker.push(feature);
        checker_impl.push(ident);
    }

    // Generate the default fallback which is the same as above, but without
    // any `#[target_feature]` attribute. We'll dispatch to this one if nothing
    // else hits.
    let default = Ident::new(&format!("__{}_default", ident), Span::call_site());
    functions.push(quote! {
        #unsafety
        fn #default #generics(#inputs) -> #output #where_clause
            #block
    });

    let fnty = AlterLifetimes.fold_type(Type::BareFn(fnty));
    let functions = ToTokensAll(&functions);
    let temp_bindings = &temp_bindings;
    let attrs = ToTokensAll(&attrs);
    let inputs_no_patterns = &inputs_no_patterns;
    let function = quote_spanned! { Span::call_site() =>
        #attrs
        #vis #unsafety #abi
        fn #ident #generics(#(#inputs_no_patterns),*) -> #output
            #where_clause
        {
            use std::mem;
            use std::sync::atomic::{AtomicUsize, Ordering};

            // We initially resolve to a dispatching function, and this'll get
            // filled in later with the actual function we resolve to.
            static mut WHICH: #fnty = __dispatch;

            return {
                let slot = &*(&WHICH as *const _ as *const AtomicUsize);
                let function = slot.load(Ordering::Relaxed);
                mem::transmute::<usize, #fnty>(function)(#(#temp_bindings),*)
            };

            fn __dispatch #generics (#(#inputs_no_patterns),*) -> #output {
                unsafe {
                    // Start out selecting our default fallback, and then
                    // after that go through each of the cfg directives we had
                    // and ask our runtime support if that one should be
                    // selected instead.
                    //
                    // Whatever hits last is the final resolved function, so
                    // we then store that in our dispatch function location
                    // and delegate to it as well for our own function call.
                    let mut _dest = #default as usize;
                    #(
                        if is_x86_feature_detected!(#checker) {
                            _dest = #checker_impl as usize;
                        }
                    )*

                    let slot = &*(&WHICH as *const _ as *const AtomicUsize);
                    slot.store(_dest, Ordering::Relaxed);
                    mem::transmute::<usize, #fnty>(_dest)(#(#temp_bindings),*)
                }
            }

            #functions
        }
    };

    // println!("{}", function);
    let function: TokenStream = function.into();
    function.into()
}

fn get_cfgs(attribute: proc_macro::TokenStream) -> Option<Vec<TokenStream>> {
    let mut ret = Vec::new();
    let mut cur = Vec::new();
    for token in TokenStream::from(attribute) {
        match token {
            TokenTree::Punct(ref p) if p.as_char() == ',' => {
                ret.push(TokenStream::from(cur.into_iter().collect::<TokenStream>()));
                cur = Vec::new();
            }
            _ => cur.push(token),
        }
    }
    ret.push(TokenStream::from(cur.into_iter().collect::<TokenStream>()));

    Some(ret)
}

fn parse_cfg(cfg: TokenStream) -> String {
    match feature_from_stream(cfg.into()) {
        Some(feature) => feature,
        None => {
            panic!(
                "cfg directives inside `#[cfg_specialize]` must be of the \
                 form `target_feature = \"foo\"`"
            );
        }
    }
}

fn fold_body(block: Block, feature: &str) -> Block {
    FulfillFeature(feature).fold_block(block)
}

struct ToTokensAll<'a, T: 'a>(&'a [T]);

impl<'a, T: ToTokens> ToTokens for ToTokensAll<'a, T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for item in self.0 {
            item.to_tokens(tokens);
        }
    }
}

struct FulfillFeature<'a>(&'a str);

impl<'a> Fold for FulfillFeature<'a> {
    fn fold_macro(&mut self, mut mac: Macro) -> Macro {
        if mac.path.leading_colon.is_some() {
            return mac;
        }
        if mac.path.segments.len() != 1 {
            return mac;
        }
        if mac.path.segments.first().unwrap().ident != "cfg" {
            return mac;
        }

        let mut iter = mac.tokens.clone().into_iter();
        let stream = match iter.next() {
            Some(TokenTree::Group(g)) => {
                if g.delimiter() == Delimiter::Parenthesis {
                    g.stream()
                } else {
                    return mac;
                }
            }
            _ => return mac,
        };
        if iter.next().is_some() {
            return mac;
        }
        match feature_from_stream(stream) {
            Some(ref s) if s == self.0 => {}
            _ => return mac,
        }

        let tokens = quote! { (not(this_will_never_be_pased)) };
        mac.tokens = TokenStream::from(tokens);
        return mac;
    }
}

struct AlterLifetimes;

impl Fold for AlterLifetimes {
    fn fold_lifetime(&mut self, mut lt: Lifetime) -> Lifetime {
        lt.ident = Ident::new(&format!("{}_guh", lt.ident), lt.ident.span());
        lt
    }
}

fn feature_from_stream(stream: TokenStream) -> Option<String> {
    let tokens = stream.into_iter().collect::<Vec<_>>();
    if tokens.len() != 3 {
        return None;
    }

    match &tokens[0] {
        TokenTree::Ident(s) if s == "target_feature" => {}
        _ => return None,
    }
    match &tokens[1] {
        TokenTree::Punct(p) if p.as_char() == '=' => {}
        _ => return None,
    }
    let mut literal = match &tokens[2] {
        TokenTree::Literal(ref l) => l.to_string(),
        _ => return None,
    };
    // remove quotes
    literal.remove(0);
    literal.pop();
    Some(literal)
}
