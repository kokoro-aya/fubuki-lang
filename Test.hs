import Tests.TestExpressions (testExpressionCases)
import ParseTopLevel (parseTopLevel)
import Utils (putLine)

testExpressions = do
    putStrLn "-- Test Expressions --"
    putLine

    mapM_ (\(name, cases) -> do putStrLn "--------------------------------------------------------"
                                putStrLn name
                                putStrLn "--------------------------------------------------------"

                                mapM_ (\(caseNum, code) -> do putStrLn ("No: " ++ show caseNum)
                                                              putStrLn "Code: "
                                                              putStrLn code
                                                              putStrLn "Generated ADT: "
                                                              print . parseTopLevel $ code
                                                              putLine
                                                              putLine
                                                              pure ()) cases

                                putLine
                                pure ()
                                ) . filter (\(_, xs) -> not $ null xs) $ testExpressionCases

main = do
        testExpressions