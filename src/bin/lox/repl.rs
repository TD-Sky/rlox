use ariadne::{Color, Label, Report, ReportKind, Source};
use rlox::{ExecError, Interpreter, ParseError, Parser, Scanner};

#[derive(Debug, Default)]
pub struct Repl {
    inner: Interpreter,
}

impl Repl {
    pub fn rep(&mut self, input: &str) {
        let tokens = match Scanner::new(input).scan() {
            Ok(tokens) => tokens,
            Err(e) => {
                Report::build(ReportKind::Error, e.span.clone())
                    .with_label(
                        Label::new(e.span)
                            .with_message(e.msg)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(input))
                    .unwrap();

                return;
            }
        };

        let report_parse_error = |e: ParseError| {
            Report::build(ReportKind::Error, e.span.clone())
                .with_label(
                    Label::new(e.span)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(input))
                .unwrap()
        };

        let report_exec_error = |e: ExecError| {
            Report::build(ReportKind::Error, e.span.clone())
                .with_label(
                    Label::new(e.span)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(input))
                .unwrap();
        };

        let mut parser = Parser::new(&tokens);
        let (stmts, nrest) = match parser.parse_ass() {
            Ok(v) => v,
            Err(e) => {
                report_parse_error(e);
                return;
            }
        };
        if let Err(e) = self.inner.interpret(&stmts) {
            report_exec_error(e);
            return;
        }

        if nrest > 0 {
            let expr = match parser.expression() {
                Ok(expr) => expr,
                Err(e) => {
                    report_parse_error(e);
                    return;
                }
            };
            match self.inner.eval(&expr) {
                Ok(value) => println!("{value}"),
                Err(e) => report_exec_error(e),
            }
        }
    }
}
