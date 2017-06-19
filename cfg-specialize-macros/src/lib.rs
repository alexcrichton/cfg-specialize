#![feature(proc_macro)]
#![recursion_limit = "128"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, TokenKind, Literal};
use syn::*;
use syn::fold::Folder;
use quote::{Tokens, ToTokens};

struct Feature {
    name: &'static str,
    arch: &'static [&'static str],
}

#[proc_macro_attribute]
pub fn cfg_specialize(attribute: TokenStream, function: TokenStream) -> TokenStream {
    let cfgs = match get_cfgs(attribute) {
        Some(cfgs) => cfgs,
        None => {
            panic!("the `cfg_specialize` attribute must be of the form: \
                    #[cfg_specialize(...)]");
        }
    };

    let function = proc_macro2::TokenStream::from(function);
    let Item { attrs, node } = function.clone().into();
    let ItemFn {
        ident,
        vis,
        unsafety,
        constness,
        abi,
        block,
        decl,
        ..
    } = match node {
        ItemKind::Fn(item) => item,
        _ => panic!("#[cfg_specialize] can only be applied to functions"),
    };

    let FnDecl { inputs, output, variadic, generics, .. } = { *decl };
    let ref inputs = inputs;
    let where_clause = &generics.where_clause;

    if variadic {
        panic!("the #[cfg_specialize] attribute cannot be applied to \
                variadic functions");
    }

    // We use a `static` below for dispatching this function, and there's only
    // one static per function, so we can't support generics.
    if generics.ty_params.len() > 0 {
        panic!("the #[cfg_specialize] attribute cannot be applied to \
                generic functions");
    }

    let ref inputs = inputs;
    let output = match output {
        FunctionRetTy::Ty(t, _) => t,
        FunctionRetTy::Default => {
            TyTup {
                tys: Default::default(),
                lone_comma: Default::default(),
                paren_token: Default::default(),
            }.into()
        }
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
    let mut fnty = BareFnTy {
        lifetimes: None,
        unsafety: unsafety.clone(),
        abi: None,
        fn_token: Default::default(),
        paren_token: Default::default(),
        inputs: Default::default(),
        variadic: None,
        output: FunctionRetTy::Ty(output.clone(), Default::default()),
    };
    for (i, input) in inputs.iter().enumerate() {
        let input = *input.item();

        if let FnArg::Captured(ref arg) = *input {
            if let Pat::Ident(PatIdent { ref ident, ..}) = arg.pat {
                if ident == "self" {
                    panic!("the #[cfg_specialize] attribute cannot be applied \
                            to methods, only free functions");
                }
            }
        }

        match *input {
            // `a: B`
            FnArg::Captured(ArgCaptured { ref pat, ref ty, .. }) => {
                patterns.push(pat);
                let ident = Ident::from(format!("__arg_{}", i));
                temp_bindings.push(ident.clone());
                let pat = PatIdent {
                    mode: BindingMode::ByValue(Mutability::Immutable),
                    ident: ident,
                    at_token: None,
                    subpat: None,
                };
                inputs_no_patterns.push(FnArg::Captured(ArgCaptured {
                    pat: pat.into(),
                    ty: ty.clone(),
                    colon_token: Default::default(),
                }));
                fnty.inputs.push_default(BareFnArg {
                    name: None,
                    ty: ty.clone(),
                });
            }

            _ => {
                panic!("the #[cfg_specialize] attribute cannot be applied \
                        to methods, only free functions");
            }
        }
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
    let mut checker_cfg = Vec::new();
    for cfg in cfgs {
        // First up let's figure out what `Feature` this corresponds to.
        let feature = parse_cfg(cfg);

        // This'll be the name of the function, which is the same as the name
        // of the original function but suffixed with our feature name, e.g.
        // `foo_avx`.
        let ident = Ident::from(&format!("{}_{}", ident, feature.name)[..]);

        // This is the string we'll pass to the `target_feature` attribute,
        // which enables the feature via `target_feature = "+feature"`
        let feature_enable = Lit {
            value: LitKind::Other(Literal::from(&format!("+{}", feature.name)[..])),
            span: Span::default(),
        };

        // Get rid of `cfg!(target_feature = ...)` by replacing it with `true`
        // where we can. Maybe one day this'll be fixed in the compiler and we
        // won't have to do it!.
        let body = fold_body(*block.clone(), &feature);

        // This #[cfg] directive will guard both the function and the dispatch
        // function we generate below. That way we don't have to worry as much
        // about cross compilation hopefully.
        let arch = &feature.arch;
        let cfg = quote! {
            #[cfg(any(
                #(target_arch = #arch),*
            ))]
        };

        // Package all that up in a function, and save off some of this
        // information.
        functions.push(quote! {
            #[target_feature = #feature_enable]
            #cfg
            #unsafety #constness
            fn #ident #generics(#inputs) -> #output #where_clause
                #body
        });
        checker.push(Ident::from(&format!("check_{}", feature.name)[..]));
        checker_impl.push(ident);
        checker_cfg.push(cfg);
    }

    // Generate the default fallback which is the same as above, but without
    // any `#[target_feature]` attribute. We'll dispatch to this one if nothing
    // else hits.
    let default = Ident::from(&format!("{}_default", ident)[..]);
    functions.push(quote! {
        #unsafety #constness
        fn #default #generics(#inputs) -> #output #where_clause
            #block
    });

    let fnty = Ty::BareFn(TyBareFn { ty: Box::new(fnty) });
    let functions = ToTokensAll(&functions);
    let temp_bindings = &temp_bindings;
    let attrs = ToTokensAll(&attrs);
    let inputs_no_patterns = &inputs_no_patterns;
    let function = quote! {
        #attrs
        #vis #unsafety #abi #constness
        fn #ident #generics(#(#inputs_no_patterns),*) -> #output
            #where_clause
        {
            use std::mem;
            use std::sync::atomic::{AtomicUsize, Ordering};

            // We initially resolve to a dispatching function, and this'll get
            // filled in later with the actual function we resolve to.
            static mut WHICH: #fnty = dispatch;

            return unsafe {
                let slot = &*(&WHICH as *const _ as *const AtomicUsize);
                let function = slot.load(Ordering::Relaxed);
                mem::transmute::<usize, #fnty>(function)(#(#temp_bindings),*)
            };

            fn dispatch(#(#inputs_no_patterns),*) -> #output {
                extern crate cfg_specialize;
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
                        #checker_cfg
                        {
                            if cfg_specialize::#checker() {
                                _dest = #checker_impl as usize;
                            }
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
    let function: proc_macro2::TokenStream = function.into();
    function.into()
}

fn get_cfgs(attribute: TokenStream) -> Option<Vec<TokenStream>> {
    let attribute = proc_macro2::TokenStream::from(attribute);
    let mut tokens = attribute.into_iter().collect::<Vec<_>>();
    if tokens.len() != 1 {
        return None
    }
    let stream = match tokens.remove(0).kind {
        TokenKind::Sequence(Delimiter::Parenthesis, stream) => stream,
        _ => return None
    };

    let mut ret = Vec::new();
    let mut cur = Vec::new();
    for token in stream {
        match token.kind {
            TokenKind::Op(',', _) => {
                ret.push(TokenStream::from(cur.into_iter().collect::<proc_macro2::TokenStream>()));
                cur = Vec::new();
            }
            _ => cur.push(token),
        }
    }
    ret.push(TokenStream::from(cur.into_iter().collect::<proc_macro2::TokenStream>()));

    Some(ret)
}

fn parse_cfg(cfg: TokenStream) -> &'static Feature {
    // TODO: implement this
    static X86: [&str; 2] = ["x86", "x86_64"];
    static FEATURES: &[Feature] = &[
        Feature { name: "avx", arch: &X86 },
        Feature { name: "ssse3", arch: &X86 },
    ];
    let name = match feature_from_stream(cfg.into()) {
        Some(feature) => feature,
        None => {
            panic!("cfg directives inside `#[cfg_specialize]` must be of the \
                    form `target_feature = \"foo\"`");
        }
    };
    for feature in FEATURES {
        if feature.name == name {
            return feature
        }
    }

    panic!("unknown target feature `{}` in `#[cfg_specialize]` directive", name);
}

fn fold_body(block: Block, feature: &Feature) -> Block {
    FulfillFeature(feature).fold_block(block)
}

struct ToTokensAll<'a, T: 'a>(&'a [T]);

impl<'a, T: ToTokens> ToTokens for ToTokensAll<'a, T> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        for item in self.0 {
            item.to_tokens(tokens);
        }
    }
}

struct FulfillFeature<'a>(&'a Feature);

impl<'a> Folder for FulfillFeature<'a> {
    fn fold_mac(&mut self, mut mac: Mac) -> Mac {
        if mac.path.global() {
            return mac
        }
        if mac.path.segments.len() != 1 {
            return mac
        }
        if mac.path.segments.get(0).item().ident != "cfg" {
            return mac
        }

        if mac.tokens.len() != 1 {
            return mac
        }

        let stream = match mac.tokens[0].0.kind {
            TokenKind::Sequence(Delimiter::Parenthesis, ref stream) => stream.clone(),
            _ => return mac.clone(),
        };
        match feature_from_stream(stream) {
            Some(ref s) if s == self.0.name => {}
            _ => return mac,
        }

        let tokens = quote! { (not(this_will_never_be_pased)) };
        let stream = proc_macro2::TokenStream::from(tokens);
        mac.tokens = stream.into_iter().map(|token| {
            TokenTree(token)
        }).collect();
        return mac
    }
}

fn feature_from_stream(stream: proc_macro2::TokenStream) -> Option<String> {
    let tokens = stream.into_iter().collect::<Vec<_>>();
    if tokens.len() != 3 {
        return None
    }

    match tokens[0].kind {
        TokenKind::Word(s) if s.as_str() == "target_feature" => {}
        _ => return None,
    }
    match tokens[1].kind {
        TokenKind::Op('=', _) => {}
        _ => return None,
    }
    let mut literal = match tokens[2].kind {
        TokenKind::Literal(ref l) => l.to_string(),
        _ => return None,
    };
    // remove quotes
    literal.remove(0);
    literal.pop();
    Some(literal)
}
