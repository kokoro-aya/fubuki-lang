module Tests.TestExpressions (testExpressionCases) where

simpleExpressions = zip [1..] [
    "1 + 2 - 3 + 4 - 5",
    "1 * (2 + 3)",
    "1 + 2 ^ 3 ^ 4 % 5",
    "1 << 2 + 3 * 4 >> 2 + 1",
    "\"hello\" ++ \"world\"",
    "a ++ b ++ c",
    "1 + 2 ... 3 * 4",
    "1 + 2 ..< 3 * 4",
    "1 + 2 <= 3 * 4 == 5 + 6 || false && true",
    "1 + 2 > 3 ^^ 4 < 5 && 6 + 7 == 8",
    "foo += 1 + 2 * (3 - 4)",
    "bar /= 1 * 2 + 3 / 4",
    "baz = 1 * (2 + 3) % 4"]

customOperatorExpressions = zip [1..] []

compoundExpressions = zip [1..] []

grammarCorrectButTypeIncorrectExpressions = zip [1..] []

grammarWrongExpressions = zip [1..] []

testExpressionCases :: [(String, [(Int, String)])]
testExpressionCases = [
    ("SimpleExpressions", simpleExpressions),
    ("Custom Operator Expressions", customOperatorExpressions),
    ("Compound Expressions", compoundExpressions),
    ("Grammar Correct But Type Incorrect Expressions", grammarCorrectButTypeIncorrectExpressions),
    ("Grammar Wrong Expressions", grammarWrongExpressions)]