module Tests.TestPatterns (testPatternCases) where

simplePatterns = zip [1..] [
    "_",
    "foo",
    "fo_oz",
    "_bar: int"
    ]

tuplePatterns = zip [1..] [
    "(c12, b: int)",
    "(a: real, b[4], _)",
    "(_, _)"
    ]

subscriptPatterns = zip [1..] [
    "a[b]",
    "c[12]",
    "d[6 + 7]",
    "foo[-1]",
    "foo[1..]",
    "foo[..-1]",
    "foo[1..-4]",
    "foo[-6..]",
    "foo[a + b .. 12 / 4 + 3 * 3]"
    ]

wrongPatterns = zip [1..] [
    "(\"foo\", b: int)",
    "(a: real, 13, \"foo\": int)"
    ]

testPatternCases :: [(String, [(Int, String)])]
testPatternCases = [
    ("Simple Patterns", simplePatterns),
    ("Tuple Patterns", tuplePatterns),
    ("Subscript Patterns", subscriptPatterns),
    ("Wrong Patterns", wrongPatterns)
    ]