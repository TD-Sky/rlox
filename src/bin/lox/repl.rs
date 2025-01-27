use ariadne::{Color, Label, Report, ReportKind, Source};
use ouroboros::self_referencing;
use rlox::{ExecError, Interpreter, ParseError, Parser, ResolveError, Resolver, Scanner};

#[self_referencing]
pub struct Repl {
    intp: Interpreter,
    #[borrows(mut intp)]
    #[covariant]
    resolver: Resolver<'this>,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new(Interpreter::default(), |intp| Resolver::new(intp))
    }
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
            Report::build(ReportKind::Error, e.span.range.clone())
                .with_label(
                    Label::new(e.span.range)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(input))
                .unwrap()
        };

        let report_resolve_error = |e: ResolveError| {
            Report::build(ReportKind::Error, e.span.range.clone())
                .with_label(
                    Label::new(e.span.range)
                        .with_message(e.msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(Source::from(input))
                .unwrap();
        };

        let report_exec_error = |e: ExecError| {
            Report::build(ReportKind::Error, e.span.range.clone())
                .with_label(
                    Label::new(e.span.range)
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

        if let Err(e) = self.with_resolver_mut(|r| r.resolve_stmts(&stmts)) {
            report_resolve_error(e);
            return;
        }

        if let Err(e) = self.with_resolver_mut(|r| r.interpreter().interpret(&stmts)) {
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

            if let Err(e) = self.with_resolver_mut(|r| r.resolve_expr(&expr)) {
                report_resolve_error(e);
                return;
            }

            match self.with_resolver_mut(|r| r.interpreter().eval(&expr)) {
                Ok(value) => println!("{value}"),
                Err(e) => report_exec_error(e),
            }
        }
    }
}
