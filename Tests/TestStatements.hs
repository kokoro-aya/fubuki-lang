module Tests.TestStatements (testStatementCases) where

simpleStatements = zip [1..] [
    "val a = fn (a: int) => a + 1",

    "1 + 2 * 3 - 4 / 5",
    "foo.bar(a: 1, 2, 3).baz()",

    "for i in 1 ... 3 {\n    print(i)\n}\n",

    "var cnt = 3",

    "while cnt > 0 {\n    print(cnt)\n    cnt -= 1\n}\n",

    "repeat {\n    print(cnt)\n    cnt -= 1\n} while cnt > 0\n",

    "val x = true",
    "val y = 4.5",
    "val z = \"foo\"",

    "if x, y > 3.0, z.length() < 5 {\n    print(\"foo\")\n} else if z == \"bar\" {\n    print(\"bar\")\n} else {\n    print(\"baz\")\n}\n",

    "switch x {\n    case true:\n        print(\"true\")\n    case false:\n        print(\"false\")\n}\n",

    "switch x {\n    case 1:\n        print(\"one\")\n    case 2:\n        print(\"two\")\n    default:\n        print(\"otherwise\")\n}\n",

    "switch x {\n    case 1, 2:\n        print(\"one or two\")\n    default:\n        print(\"otherwise\")\n}\n",

    "do {\n    val f = 2\n    print(f)\n}"]

testStatementCases :: [(String, [(Int, String)])]
testStatementCases = [
    ("SimpleExpressions", simpleStatements)]