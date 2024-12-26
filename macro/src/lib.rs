//! Macros to bundle and use Scheme programs.

use cfg_exif::feature;
use core::error::Error;
use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{quote, ToTokens};
use stak_compiler::CompileError;
use stak_macro_util::{convert_result, read_source_file};
use std::path::{Path, MAIN_SEPARATOR_STR};
use syn::{parse::Parse, parse_macro_input, Ident, LitStr, Token};

struct IncludeModuleInput {
    path: LitStr,
    module: Option<Ident>,
}

impl Parse for IncludeModuleInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        let module = if input.parse::<Option<Token![,]>>()?.is_some() {
            if let Some(module) = input.parse()? {
                input.parse::<Option<Token![,]>>()?;
                Some(module)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Self { path, module })
    }
}

/// Includes bytecodes of a R7RS Scheme module built by the
/// [`stak_build`](https://docs.rs/stak-build) crate.
///
/// See the [`stak`](https://docs.rs/stak) crate's documentation for full examples.
#[proc_macro]
pub fn include_module(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as IncludeModuleInput);

    convert_result(include_result(&input)).into()
}

fn include_result(input: &IncludeModuleInput) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    let path = format!("{}", Path::new("src").join(input.path.value()).display());
    let full_path = quote!(concat!(env!("OUT_DIR"), #MAIN_SEPARATOR_STR, #path));
    let module = input
        .module
        .as_ref()
        .map(|r#type| r#type.to_token_stream())
        .unwrap_or_else(|| quote!(stak::module));

    Ok(feature!(if ("hot-reload") {
        quote!(#module::UniversalModule::from_hot_reload_path(#full_path))
    } else {
        quote!(#module::UniversalModule::from_bytecode(
            include_bytes!(#full_path)
        ))
    }))
}

/// Compiles a module in R7RS Scheme into bytecodes.
///
/// # Examples
///
/// ```rust
/// const BYTECODE: &[u8] = stak_macro::compile_r7rs!("(define x 42)");
/// ```
#[proc_macro]
pub fn compile_r7rs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_r7rs(&input.value())).into()
}

/// Includes a module in R7RS Scheme as bytecodes.
///
/// # Examples
///
/// ```rust
/// const BYTECODE: &[u8] = stak_macro::include_r7rs!("foo.scm");
/// ```
#[proc_macro]
pub fn include_r7rs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| generate_r7rs(&read_source_file(input)?))()).into()
}

fn generate_r7rs(source: &str) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    generate_scheme(source, |source, target| {
        stak_compiler::compile_r7rs(source, target)
    })
}

/// Compiles a module in Scheme into bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// const BYTECODE: &[u8] = stak_macro::compile_bare!("($$define x 42)");
/// ```
#[proc_macro]
pub fn compile_bare(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result(generate_bare(&input.value())).into()
}

/// Includes a module in Scheme as bytecodes with only built-ins.
///
/// # Examples
///
/// ```rust
/// const BYTECODE: &[u8] = stak_macro::include_bare!("foo.scm");
/// ```
#[proc_macro]
pub fn include_bare(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LitStr);

    convert_result((|| generate_bare(&read_source_file(input)?))()).into()
}

fn generate_bare(source: &str) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    generate_scheme(source, |source, target| {
        stak_compiler::compile_bare(source, target)
    })
}

fn generate_scheme(
    source: &str,
    compile: fn(&[u8], &mut Vec<u8>) -> Result<(), CompileError>,
) -> Result<proc_macro2::TokenStream, Box<dyn Error>> {
    let mut target = vec![];

    compile(source.as_bytes(), &mut target)?;

    let target = Literal::byte_string(&target);

    Ok(quote! { #target })
}
