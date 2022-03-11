module Tests.TestTypes (testTypeCases) where

funcTypes = zip [1..] [
    "(int) -> unit",
    "() -> (real, int)",
    "(bool, real) -> [real]",
    "(a, (a) -> b) -> b",
    "([a], (a) -> b) -> [b]"
    ]

arrTypes = zip [1..] [
    "[int]",
    "[a]",
    "[(a, b)]",
    "[(int) -> unit]"
    ]

tupleTypes = zip [1..] [
    "(a, b)",
    "(int, real)",
    "(a, (a) -> b)",
    "((a, b), (a) -> b)"
    ]

testTypeCases :: [(String, [(Int, String)])]
testTypeCases = [
    ("Simple Types", zip [1..] ["int", "real", "bool", "unit", "a1", "a", "a_b"]),
    ("Function Types", funcTypes),
    ("Array Types", arrTypes),
    ("Tuple Types", tupleTypes)
    ]