#![feature(box_syntax, box_patterns)]

#[macro_use] extern crate pretty_assertions;
extern crate combine;

extern crate roc;

#[cfg(test)]
mod tests {
    use roc::expr::Expr::*;
    use roc::expr::Operator::*;
    use roc::parse;
    use combine::{Parser};
    use combine::easy;

    // STRING LITERALS

    fn expect_parsed_str<'a>(expected_str: &'a str, actual_str: &'a str) {
        assert_eq!(
            Ok((String(expected_str.to_string()), "")),
            parse::expr().parse(actual_str)
        );
    }

    fn expect_parsed_str_error<'a>(actual_str: &'a str) {
        assert!(
            parse::string_literal().parse(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn parse_empty_string() {
        expect_parsed_str("", "\"\"");
    }

    #[test]
    fn parse_string_without_escape() {
        expect_parsed_str("a", "\"a\"");
        expect_parsed_str("ab", "\"ab\"");
        expect_parsed_str("abc", "\"abc\"");
        expect_parsed_str("123", "\"123\"");
        expect_parsed_str("abc123", "\"abc123\"");
        expect_parsed_str("123abc", "\"123abc\"");
        expect_parsed_str("123 abc 456 def", "\"123 abc 456 def\"");
    }

    #[test]
    fn parse_string_with_special_escapes() {
        expect_parsed_str("x\\x", "\"x\\\\x\"");
        expect_parsed_str("x\"x", "\"x\\\"x\"");
        expect_parsed_str("x\tx", "\"x\\tx\"");
        expect_parsed_str("x\rx", "\"x\\rx\"");
        expect_parsed_str("x\nx", "\"x\\nx\"");
    }

    #[test]
    fn parse_string_with_single_qoute() {
        // This shoud NOT be escaped in a string.
        expect_parsed_str("x'x", "\"x'x\"");
    }

    #[test]
    fn parse_string_with_valid_unicode_escapes() {
        expect_parsed_str("x\u{00A0}x", "\"x\\u{00A0}x\"");
        expect_parsed_str("x\u{101010}x", "\"x\\u{101010}x\"");
    }

    #[test]
    fn parse_string_with_invalid_unicode_escapes() {
        // Should be too big - max size should be 10FFFF.
        // (Rust has this restriction. I assume it's a good idea.)
        expect_parsed_str_error("\"x\\u{110000}x\"");

        // No digits specified
        expect_parsed_str_error("\"x\\u{}x\"");

        // No opening curly brace
        expect_parsed_str_error("\"x\\u}x\"");

        // No closing curly brace
        expect_parsed_str_error("\"x\\u{x\"");

        // No curly braces
        expect_parsed_str_error("\"x\\ux\"");
    }

    // CHAR LITERALS

    fn expect_parsed_char<'a>(expected: char, actual_str: &'a str) {
        assert_eq!(Ok((Char(expected), "")), parse::char_literal().parse(actual_str));
    }

    fn expect_parsed_char_error<'a>(actual_str: &'a str) {
        assert!(
            parse::char_literal().parse(actual_str).is_err(),
            "Expected parsing error"
        );
    }
    

    #[test]
    fn parse_empty_char() {
        // expect_parsed_char_error("''");

        match parse::char_literal().easy_parse("''") {
            Ok(_) => panic!("Expected parse error"),
            Err(err) => {
                let errors = err.errors;

                assert_eq!(errors,
                    vec![
                        easy::Error::Unexpected('\''.into()),
                        easy::Error::Expected(parse::ERR_EMPTY_CHAR.into()),
                    ]
                );
            }
        };
    }

    #[test]
    fn parse_char_without_escape() {
        expect_parsed_char('a', "'a'");
        expect_parsed_char('1', "'1'");
        expect_parsed_char(' ', "' '");
    }

    #[test]
    fn parse_char_with_special_escapes() {
        expect_parsed_char('\\', "'\\\\'");
        expect_parsed_char('\'', "'\\''");
        expect_parsed_char('\t', "'\\t'");
        expect_parsed_char('\r', "'\\r'");
        expect_parsed_char('\n', "'\\n'");
    }

    #[test]
    fn parse_char_with_double_qoute() {
        // This shoud NOT be escaped in a char.
        expect_parsed_char('"', "'\"'");
    }

    #[test]
    fn parse_char_with_unicode_escapes() {
        expect_parsed_char('\u{00A0}', "'\\u{00A0}'");
        expect_parsed_char('\u{101010}', "'\\u{101010}'");
    }

    #[test]
    fn parse_char_with_invalid_unicode_escapes() {
        // Should be too big - max size should be 10FFFF.
        // (Rust has this rechariction. I assume it's a good idea.)
        expect_parsed_char_error("\"x\\u{110000}x\"");

        // No digits specified
        expect_parsed_char_error("\"x\\u{}x\"");

        // No opening curly brace
        expect_parsed_char_error("\"x\\u}x\"");

        // No closing curly brace
        expect_parsed_char_error("\"x\\u{x\"");

        // No curly braces
        expect_parsed_char_error("\"x\\ux\"");
    }

    // NUMBER LITERALS
    
    fn expect_parsed_int<'a>(expected: i64, actual: &str) {
        assert_eq!(Ok((Int(expected), "")), parse::number_literal().parse(actual));
    }

    fn expect_parsed_ratio<'a>(expected_numerator: i64, expected_denominator: u64, actual: &str) {
        assert_eq!(Ok((Frac(expected_numerator, expected_denominator), "")), parse::number_literal().parse(actual));
    }

    #[test]
    fn parse_positive_int() {
        expect_parsed_int(1234, "1234");
    }

    #[test]
    fn parse_negative_int() {
        expect_parsed_int(-1234, "-1234");
    }

    #[test]
    fn parse_positive_ratio() {
        expect_parsed_ratio(12345, 100, "123.45");
        expect_parsed_ratio(4200, 100, "42.00");
    }

    #[test]
    fn parse_negative_ratio() {
        expect_parsed_ratio(-1234567, 1000, "-1234.567");
        expect_parsed_ratio(-1920, 10, "-192.0");
    }

    #[test]
    fn parse_ints_with_spaces() {
        expect_parsed_int(987654321, "987 6 5 432 1");
        expect_parsed_int(-1234567890, "-1 234 567 890");
    }

    #[test]
    fn parse_ratios_with_spaces() {
        expect_parsed_ratio(-1234567, 1000, "-1 23 4.567");
        expect_parsed_ratio(-1920, 10, "-19 2.0");
        expect_parsed_ratio(12345, 100, "1 2 3.45");
        expect_parsed_ratio(4200, 100, "4 2.00");
    }

    #[test]
    fn parse_single_operator() {
        match parse::expr().parse("1234 + 567") {
            Ok((Operator(v1, op, v2), "")) => {
                assert_eq!(*v1, Int(1234));
                assert_eq!(op, Plus);
                assert_eq!(*v2, Int(567));
            },
            _ => panic!("Expression didn't parse"),
        }
    }

    #[test]
    fn parse_multiple_operators() {
        match parse::expr().parse("1 + 2 * 3") {
            Ok((Operator(box v1, op1, box Operator(box v2, op2, box v3)), "")) => {
                assert_eq!(v1, Int(1));
                assert_eq!(op1, Plus);
                assert_eq!(v2, Int(2));
                assert_eq!(op2, Star);
                assert_eq!(v3, Int(3));
            },
            _ => panic!("Expression didn't parse"),
        }
    }

    // VAR

    fn expect_parsed_var<'a>(expected_str: &'a str) {
        let expected = expected_str.to_string();

        assert_eq!(Ok((Var(expected), "")), parse::expr().parse(expected_str));
    }

    fn expect_parsed_var_error<'a>(actual_str: &'a str) {
        // TODO someday, this should work! It should parse as a variant.
        assert!(
            parse::expr().parse(actual_str).is_err(),
            "Expected parsing error"
        );
    }

    #[test]
    fn parse_var() {
        expect_parsed_var("x");
        expect_parsed_var("x2");
        expect_parsed_var("foo");
        expect_parsed_var("foo2furious");
    }

    #[test]
    fn parse_invalid_var() {
        expect_parsed_var_error("5x");
        expect_parsed_var_error("2foo2furious");
        expect_parsed_var_error("2Foo2Furious");

        // TODO someday, capitalized vars should parse successfully as variants.
        // At that point, turn these into variant tests!
        expect_parsed_var_error("X");
        expect_parsed_var_error("X2");
        expect_parsed_var_error("Foo");
        expect_parsed_var_error("Foo2Furious");
    }

    // APPLY
    
    // TODO write a bunch of parenthetical expression tests - try to repeat
    // all of the above tests except with parens too!
    // Also, verify them all with variable paren counts; ((foo)) should work.

    // FUNC


    // TODO try it with operators, e.g. foo bar + baz qux

    // fn expect_parsed_func<'a>(expected_str: &'a str) {
    //     let expected = expected_str.to_string();

    //     assert_eq!(Ok((Var(expected), "")), parse::expr().parse(expected_str));
    // }

    // fn expect_parsed_func_error<'a>(actual_str: &'a str) {
    //     // TODO someday, this should work! It should parse as a variant.
    //     assert!(
    //         parse::expr().parse(actual_str).is_err(),
    //         "Expected parsing error"
    //     );
    // }

    // #[test]
    // fn parse_var() {
    //     expect_parsed_var("x");
    //     expect_parsed_var("x2");
    //     expect_parsed_var("foo");
    //     expect_parsed_var("foo2furious");
    // }

    // #[test]
    // fn parse_invalid_var() {
    //     expect_parsed_var_error("5x");
    //     expect_parsed_var_error("2foo2furious");
    //     expect_parsed_var_error("2Foo2Furious");

    //     // TODO someday, capitalized vars should parse successfully as variants.
    //     // At that point, turn these into variant tests!
    //     expect_parsed_var_error("X");
    //     expect_parsed_var_error("X2");
    //     expect_parsed_var_error("Foo");
    //     expect_parsed_var_error("Foo2Furious");
    // }
}
