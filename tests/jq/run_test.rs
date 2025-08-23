use std::sync::Arc;

use crate::common::{parse_tests, JqTest};
use jqrs::{
    filter::{self, Filter},
    json::Json,
};

fn test_run(test: JqTest) {
    let test_line = test.line;
    let test_filter = test.filter;
    let test_input = test.input;

    let filter: Arc<_> = test
        .filter
        .parse::<Filter>()
        .unwrap_or_else(|err| panic!("error parsing filter: {test_filter}: {err}"))
        .into();

    let input: Arc<_> = test
        .input
        .parse::<Json>()
        .unwrap_or_else(|err| panic!("error parsing input json: {test_input}: {err}"))
        .into();

    let expected_result = test
        .result
        .into_iter()
        .map(|json| {
            json.parse::<Json>()
                .unwrap_or_else(|err| panic!("error parsing filter: {test_filter}: {err}"))
                .into()
        })
        .collect::<Vec<_>>();

    let result = filter.run(input, Default::default()).collect::<Result<Vec<_>, _>>();
    match result {
        Ok(result) => {
            assert_eq!(
                result, expected_result,
                "Line {test_line}; echo '{test_input}' | jqrs '{test_filter}'"
            )
        }
        Err(end) => {
            match end {
                filter::run_vm::RunEndValue::Error(err) => panic!("Line {test_line}; error {err}"),
                filter::run_vm::RunEndValue::Break(label) => panic!("Line {test_line}; break '{label}'"),
                filter::run_vm::RunEndValue::Halt { code, err } => panic!("Line {test_line}; halt '{err:?} ({code})'"),
            }
        }
    }
}

fn test_run_file(tests: &str) {
    for test in parse_tests(tests) {
        test_run(test);
    }
}

#[test]
fn base_test() {
    test_run_file(include_str!("assets/jq/base.jq.test"))
}

#[test]
fn manual_test() {
    test_run_file(include_str!("assets/jq/manual.jq.test"))
}

#[test]
fn utf8_truncate_test() {
    test_run_file(include_str!("assets/jq/utf8-truncate.jq.test"))
}

#[test]
fn regex_test() {
    test_run_file(include_str!("assets/jq/regex.jq.test"))
}
