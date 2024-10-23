use rlox::{Interpreter, Parser, Scanner};

#[derive(Debug, Default)]
pub struct Repl {
    inner: Interpreter,
}

impl Repl {
    pub fn rep(&mut self, input: &str) {
        let tokens = Scanner::new(input).scan().unwrap();

        let mut parser = Parser::new(&tokens);
        let (stmts, nrest) = parser.parse_ass();
        self.inner.interpret(&stmts).unwrap();

        if nrest > 0 {
            let expr = parser.expression().unwrap();
            let value = self.inner.eval(&expr).unwrap();
            println!("{value}");
        }
    }
}
