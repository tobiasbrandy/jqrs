mod common;

use common::{parse_tests, JqTest};
use jqrs::filter::Filter;

fn test_filter_parse(test: JqTest) {
    let line = test.line;
    let filter = test.filter.parse::<Filter>();
    assert!(
        filter.is_ok(),
        "filter parse error (line {line}): {}",
        filter.err().unwrap()
    )
}

fn test_filter_parse_file(tests: &str) {
    for test in parse_tests(tests) {
        test_filter_parse(test);
    }
}

#[test]
fn base_test() {
    test_filter_parse_file(include_str!("assets/jq/base.jq.test"))
}

#[test]
fn manual_test() {
    test_filter_parse_file(include_str!("assets/jq/manual.jq.test"))
}

#[test]
fn utf8_truncate_test() {
    test_filter_parse_file(include_str!("assets/jq/utf8-truncate.jq.test"))
}

#[test]
fn regex_test() {
    test_filter_parse_file(include_str!("assets/jq/regex.jq.test"))
}
