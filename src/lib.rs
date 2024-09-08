pub mod cli;
mod expr;
mod scan;

use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, Source};
use scan::Scanner;

pub fn run(path: Option<&Path>, source: &str) {
    let scanner = Scanner::new(source);
    match scanner.scan() {
        Ok(tokens) => {
            println!("{tokens:#?}");
        }
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
        }
    }
}
