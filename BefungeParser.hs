-- @+leo-ver=5-thin
-- @+node:kajsa.20130215180649.1361: * @file BefungeParser.hs
-- @+others
-- @+node:kajsa.20130215183429.1369: ** Interpreter
runBefungeProgram :: String -> String
runBefungeProgram program = interpretProgram program []

interpretProgram :: String -> String -> String
interpretProgram ('"':xs)       stack     = interpretString xs stack
interpretProgram (',':xs)       (s:stack) = s:(interpretProgram xs stack)
interpretProgram ('@':xs)       stack     = ""
interpretProgram   (x:xs)       stack     = interpretProgram xs stack

interpretString :: String -> String -> String
interpretString ('"':xs) stack = interpretProgram xs stack
interpretString (c:xs) stack = interpretString xs (c:stack)
-- @+node:kajsa.20130228131825.1375: ** Run program
assertGivesOutput :: String -> String -> IO ()
assertGivesOutput program expectedOutput =
    case expectedOutput == actualOutput of
        True -> putStrLn $ "Pass: " ++ program ++ " -> " ++ actualOutput
        False -> putStrLn $ "Fail: " ++ program ++ " -> " ++ actualOutput
    where
        actualOutput = runBefungeProgram program

main = do
    assertGivesOutput "\"!\",@" "!"
    assertGivesOutput "\"!dlroW olleH\",,,,,,,,,,,,@" "Hello World!"
-- @-others
-- @-leo
