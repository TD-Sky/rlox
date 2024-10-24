pub mod cli;
mod exec;
mod parse;
mod scan;

pub use exec::{ExecError, Interpreter, Value};
pub use parse::{ParseError, Parser};
pub use scan::{Lexeme, ScanError, Scanner};

use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, Source};

pub fn run(path: &Path, source: &str) {
    let scanner = Scanner::new(source);
    let lexemes = match scanner.scan() {
        Ok(lexemes) => lexemes,
        Err(e) => {
            let path = path.to_string_lossy();
            let path: &str = path.as_ref();

            Report::build(ReportKind::Error, path, e.offset)
                .with_label(
                    Label::new((path, e.span))
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((path, Source::from(source)))
                .unwrap();

            return;
        }
    };

    let mut parser = Parser::new(&lexemes);
    let stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(e) => {
            let path = path.to_string_lossy();
            let path: &str = path.as_ref();

            Report::build(ReportKind::Error, path, e.span.start)
                .with_label(
                    Label::new((path, e.span))
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((path, Source::from(source)))
                .unwrap();

            return;
        }
    };

    if let Err(e) = Interpreter::default().interpret(&stmts) {
        let path = path.to_string_lossy();
        let path: &str = path.as_ref();

        Report::build(ReportKind::Error, path, e.span.start)
            .with_label(
                Label::new((path, e.span))
                    .with_message(e.msg)
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((path, Source::from(source)))
            .unwrap();
    }
}

pub fn rep(source: &str) {
    let scanner = Scanner::new(source);
    let lexemes = match scanner.scan() {
        Ok(lexemes) => lexemes,
        Err(e) => {
            Report::build(ReportKind::Error, (), e.offset)
                .with_label(
                    Label::new(e.span)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(source))
                .unwrap();

            return;
        }
    };

    let mut parser = Parser::new(&lexemes);
    let expr = match parser.expression() {
        Ok(expr) => expr,
        Err(e) => {
            Report::build(ReportKind::Error, (), e.span.start)
                .with_label(
                    Label::new(e.span)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(source))
                .unwrap();

            return;
        }
    };

    match Interpreter::default().eval(&expr) {
        Ok(value) => {
            println!("{value:#?}")
        }
        Err(e) => Report::build(ReportKind::Error, (), e.span.start)
            .with_label(
                Label::new(e.span)
                    .with_message(e.msg)
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(Source::from(source))
            .unwrap(),
    }
}
