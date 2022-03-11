import ParseTopLevel (testParse)
import Utils (putLine)
import Tests.TestExpressions (testExpressionCases)
import Tests.TestTypes (testTypeCases)
import Tests.TestPatterns (testPatternCases)
import FubukiParser (expr, type_, pattern_)


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
                                                              print . testParse p $ code
                                                              putLine
                                                              putLine
                                                              pure ()) cases

                                putLine
                                pure ()
                                ) . filter (\(_, xs) -> not $ null xs) $ test
    putStrLn $ "-- Test " ++ name ++ " End --"

main = do
        -- testSuite "Expressions" expr testExpressionCases
        testSuite "Types" type_ testTypeCases
        testSuite "Patterns" pattern_ testPatternCases