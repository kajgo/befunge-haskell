-- @+leo-ver=5-thin
-- @+node:kajsa.20130215180649.1361: * @file BefungeParser.hs
-- @+others
-- @+node:kajsa.20130215183429.1369: ** Main run function
runBefungeProgram :: String -> IO ()
runBefungeProgram program = do let result = executeProgram program []
                               putStrLn result
                            
executeProgram :: String -> String -> String
executeProgram ('"':c:'"':xs) stack     = executeProgram xs (c:stack)
executeProgram (',':xs)       (s:stack) = s:(executeProgram xs stack)
executeProgram ('@':xs)       stack     = ""
executeProgram   (x:xs)       stack     = executeProgram xs stack
-- @-others
main = runBefungeProgram "\"!\",@"
-- @-leo
