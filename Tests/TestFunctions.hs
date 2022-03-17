module Tests.TestFunctions (testFunctionCases) where

simpleFunctions = zip [1..] [
    "fn concat_str(head s1: str, tail s2: str): str {\n    s1 ++ s2\n}\n",
    "fn one() => 1",
    "fn length<a>(_ xs: [a]): int {\n    var i = 0\n    for x in xs { i += 1 }\n    return i\n}\n",
    "fn length<a>(xs: [a]): int {\n    return xs.reduce(0, fn (acc: int, _: a) => acc + 1)\n    // ? means the value is ignored\n}\n",
    "fn `++`(a: [a], b: [a]): [a] \n    => a ++ b\n"]

testFunctionCases :: [(String, [(Int, String)])]
testFunctionCases = [
    ("Simple Functions", simpleFunctions)]