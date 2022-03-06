import Tests.TestExpressions (testExpressionCases)
import ParseTopLevel (testParseTopLevel)
import Utils (putLine)

testExpressions = do
    putStrLn "-- Test Expressions Begin --"
    putLine

    mapM_ (\(name, cases) -> do putStrLn "--------------------------------------------------------"
                                putStrLn name
                                putStrLn "--------------------------------------------------------"

                                mapM_ (\(caseNum, code) -> do putStrLn ("No: " ++ show caseNum)
                                                              putStrLn "Code: "
                                                              putStrLn code
                                                              putStrLn "Generated ADT: "
                                                              print . testParseTopLevel $ code
                                                              putLine
                                                              putLine
                                                              pure ()) cases

                                putLine
                                pure ()
                                ) . filter (\(_, xs) -> not $ null xs) $ testExpressionCases
    putStrLn "-- Test Expressions End --"

main = do
        testExpressions