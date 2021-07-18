/* replhelper.rs
 *
 * Code used by rustyline to control reading input from the user
 */

use rustyline::completion::Completer;
use rustyline::hint::Hinter;
use rustyline::highlight::Highlighter;
use rustyline::validate::{MatchingBracketValidator, Validator, ValidationContext, ValidationResult};
use rustyline::Helper;

pub struct MyHelper {
    mbv: MatchingBracketValidator
}

impl MyHelper {
    pub fn new() -> MyHelper {
        MyHelper {mbv: MatchingBracketValidator::new()}
    }
}

impl Completer for MyHelper {
    type Candidate = String;
}

impl Hinter for MyHelper {
    type Hint = String;
}

impl Highlighter for MyHelper {}

impl Validator for MyHelper {
    fn validate(&self, ctx: &mut ValidationContext<'_>) -> rustyline::Result<ValidationResult> {
        if !ctx.input().trim_end().ends_with(';') { return Ok(ValidationResult::Incomplete) }
        self.mbv.validate(ctx)
    }
}

impl Helper for MyHelper {}
