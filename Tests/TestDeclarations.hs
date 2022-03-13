module Tests.TestDeclarations (testDeclarationCases) where

simpleDeclarations = zip [1..] [
    "val a: real = 3.5",
    "val b = 3",
    "var (b, c) = (1, 2)",
    "var (_, x) = foo()",
    "var a = 4, b: bool = false, c: char = 'c', d = \"foo\"",
    "val inc = fn (x: int): int => x + 1"]

testDeclarationCases :: [(String, [(Int, String)])]
testDeclarationCases = [
    ("Simple Declarations", simpleDeclarations)]