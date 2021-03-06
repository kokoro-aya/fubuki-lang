module Tests.TestPrimaries (testPrimariesCases) where

simplePrimaries = zip [1..] [
    "27.5",
    "14",
    "false",
    "'c'",
    "\"hello\"",
    "(1, 2, 2 / (3 + 4))",
    "(1 + (3, 4))",
    "(1, 'b', (\"foo\", 4))",
    "[1,2,3,4,5]",
    "['a', 'b', c]",
    "foo",
    "bar: int",
    "fst[0]",
    "snd[1][2]",
    "_",
    "list[2..3]",
    "collatz(n: 3)",
    "concat(\"hello\", \"world\")",
    "one()",
    "zip(list, [1,2,3,4,5])"]


testPrimariesCases :: [(String, [(Int, String)])]
testPrimariesCases = [
    ("SimpleExpressions", simplePrimaries)]