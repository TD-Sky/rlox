pub mod cli;
mod eval;
mod expr;
mod parse;
mod scan;
mod value;

use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, Source};
use eval::eval;
use parse::Parser;
use scan::Scanner;
pub use value::Value;

pub fn run(path: Option<&Path>, source: &str) {
    let scanner = Scanner::new(source);
    let lexemes = match scanner.scan() {
        Ok(lexemes) => lexemes,
        Err(e) => {
            if let Some(path) = path.map(Path::to_string_lossy) {
                let path: &str = path.as_ref();

                Report::build(ReportKind::Error, path, e.offset)
                    .with_label(
                        Label::new((path, e.span))
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((path, Source::from(source)))
                    .unwrap()
            } else {
                Report::build(ReportKind::Error, (), e.offset)
                    .with_label(
                        Label::new(e.span)
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(source))
                    .unwrap()
            }

            return;
        }
    };

    let mut parser = Parser::new(&lexemes);
    let expr = match parser.expression() {
        Ok(expr) => expr,
        Err(e) => {
            if let Some(path) = path.map(Path::to_string_lossy) {
                let path: &str = path.as_ref();

                Report::build(ReportKind::Error, path, e.span.start)
                    .with_label(
                        Label::new((path, e.span))
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((path, Source::from(source)))
                    .unwrap()
            } else {
                Report::build(ReportKind::Error, (), e.span.start)
                    .with_label(
                        Label::new(e.span)
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(source))
                    .unwrap()
            }

            return;
        }
    };

    match eval(&expr) {
        Ok(value) => {
            println!("{value:#?}")
        }
        Err(e) => {
            if let Some(path) = path.map(Path::to_string_lossy) {
                let path: &str = path.as_ref();

                Report::build(ReportKind::Error, path, e.span.start)
                    .with_label(
                        Label::new((path, e.span))
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((path, Source::from(source)))
                    .unwrap()
            } else {
                Report::build(ReportKind::Error, (), e.span.start)
                    .with_label(
                        Label::new(e.span)
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(source))
                    .unwrap()
            }
        }
    }
}
