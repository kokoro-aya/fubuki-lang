module Tests.TestAmbiguous (testAmbiguousCases) where

shouldCompile = zip [1..] [
    "val lesser = sort<int>(array: [1,2,3,4,5])",
    "val greater = sort<>([1,2,3,4,5], ascending: false)",
    "foo = plus<int, int>(1, 2)"]

testAmbiguousCases :: [(String, [(Int, String)])]
testAmbiguousCases = [
    ("Should not create ambiguity", shouldCompile)]