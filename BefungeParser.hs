-- @+leo-ver=5-thin
-- @+node:kajsa.20130215180649.1361: * @file BefungeParser.hs
-- @+others
-- @+node:kajsa.20130215183429.1369: ** Interpreter
runBefungeProgram :: String -> String
runBefungeProgram program = executeProgram program []
                            
executeProgram :: String -> String -> String
executeProgram ('"':c:'"':xs) stack     = executeProgram xs (c:stack)
executeProgram (',':xs)       (s:stack) = s:(executeProgram xs stack)
executeProgram ('@':xs)       stack     = ""
executeProgram   (x:xs)       stack     = executeProgram xs stack
-- @+node:kajsa.20130228131825.1375: ** Run program
assertGivesOutput :: String -> String -> IO ()
assertGivesOutput program expectedOutput =
    case expectedOutput == runBefungeProgram program of
        True -> putStrLn $ "Pass: " ++ program 
        False -> putStrLn $ "Fail: " ++ program

main = do
    assertGivesOutput "\"!\",@" "!"
-- @-others
-- @-leo
