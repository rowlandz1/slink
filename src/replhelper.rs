use rustyline::completion::Completer;
use rustyline::hint::Hinter;
use rustyline::highlight::Highlighter;
use rustyline::validate::Validator;
use rustyline::validate::ValidationContext;
use rustyline::validate::ValidationResult;
use rustyline::Helper;

pub struct MyHelper {

}

impl MyHelper {
    pub fn new() -> MyHelper {
        MyHelper {}
    }
}

impl Completer for MyHelper {
    type Candidate = String;

}

impl Hinter for MyHelper {
    type Hint = String;
}

impl Highlighter for MyHelper {

}

impl Validator for MyHelper {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        if ctx.input().trim_end().ends_with(";") {
            Ok(ValidationResult::Valid(None))
        } else {
            Ok(ValidationResult::Incomplete)
        }
    }
}

impl Helper for MyHelper {

}
