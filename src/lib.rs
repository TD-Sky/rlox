pub mod cli;
mod exec;
mod parse;
mod scan;
mod span;
mod utils;

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

            Report::build(ReportKind::Error, (path, e.span.clone()))
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

            Report::build(ReportKind::Error, (path, e.span.range.clone()))
                .with_label(
                    Label::new((path, e.span.range))
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

        Report::build(ReportKind::Error, (path, e.span.range.clone()))
            .with_label(
                Label::new((path, e.span.range))
                    .with_message(e.msg)
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((path, Source::from(source)))
            .unwrap();
    }
}
