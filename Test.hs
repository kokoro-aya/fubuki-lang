import ParseTopLevel (testParse, testParseShow)
import Utils (putLine)
import Tests.TestExpressions (testExpressionCases)
import Tests.TestTypes (testTypeCases)
import Tests.TestPatterns (testPatternCases)
import FubukiParser (expr, type_, pattern_, statement, declaration, functionDeclaration, primary)
import Tests.TestAmbiguous (testAmbiguousCases)
import Tests.TestDeclarations (testDeclarationCases)
import Tests.TestFunctions (testFunctionCases)
import Tests.TestPrimaries (testPrimariesCases)
import Tests.TestStatements (testStatementCases)


testSuite name p test = do
    putStrLn $ "-- Test " ++ name ++ " Begin --"
    putLine

    mapM_ (\(name, cases) -> do putStrLn "--------------------------------------------------------"
                                putStrLn name
                                putStrLn "--------------------------------------------------------"

                                mapM_ (\(caseNum, code) -> do putStrLn ("No: " ++ show caseNum)
                                                              putStrLn "Code: "
                                                              putStrLn code
                                                              putStrLn "Generated ADT: "
                                                              putStrLn . testParseShow p $ code
                                                              putStrLn "Pretty print: "
                                                              putStrLn . testParse p $ code
                                                              putLine
                                                              putLine
                                                              pure ()) cases

                                putLine
                                pure ()
                                ) . filter (\(_, xs) -> not $ null xs) $ test
    putStrLn $ "-- Test " ++ name ++ " End --"

main = do
        -- testSuite "Ambiguous Clauses" statement testAmbiguousCases

        -- putStrLn ""

        -- testSuite "Declarations" declaration testDeclarationCases -- OK

        -- putStrLn ""

        -- testSuite "Functions" functionDeclaration testFunctionCases -- OK

        -- putStrLn ""

        testSuite "Expressions" expr testExpressionCases -- OK, assignment expressions should be lifted out

        putStrLn ""

        -- testSuite "Primaries" primary testPrimariesCases -- OK

        -- putStrLn ""

        -- testSuite "Statement" statement testStatementCases -- OK

        -- putStrLn ""

        -- testSuite "Types" type_ testTypeCases -- OK

        -- putStrLn ""

        -- testSuite "Patterns" pattern_ testPatternCases -- OK

        -- putStrLn ""