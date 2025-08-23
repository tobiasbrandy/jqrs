pub struct JqTest<'a> {
    pub line: usize,
    pub filter: &'a str,
    #[allow(dead_code)]
    pub input: &'a str,
    #[allow(dead_code)]
    pub result: Vec<&'a str>,
}

pub fn parse_tests(tests: &str) -> Vec<JqTest<'_>> {
    let mut lines = tests
        .lines()
        .enumerate()
        .map(|(i, line)| (i, line.trim()))
        .filter(|(_, line)| line.is_empty() || !line.starts_with("#"));

    let mut tests = Vec::new();
    loop {
        let (line, filter) = loop {
            match lines.next() {
                None => return tests,
                Some((_, "")) => continue,
                Some((i, filter)) => break (i + 1, filter),
            }
        };

        let input = match lines.next() {
            None => panic!(),
            Some((_, "")) => panic!("error: malformed test: missing input for test on line {line}"),
            Some((_, input)) => input,
        };

        let result = lines
            .by_ref()
            .map(|(_, line)| line)
            .take_while(|line| !line.is_empty())
            .collect();

        tests.push(JqTest {
            line,
            filter,
            input,
            result,
        });
    }
}
