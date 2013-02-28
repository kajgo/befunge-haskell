-- @+leo-ver=5-thin
-- @+node:kajsa.20130215180649.1361: * @file BefungeParser.hs
-- @+others
-- @+node:kajsa.20130228155956.1376: ** Program abstraction
data Program = Program String

data ProgramCounter = ProgramCounter Int

instructionAt :: Program -> ProgramCounter -> Char
instructionAt (Program p) (ProgramCounter pc) = p !! pc

calculateNextPc :: Program -> ProgramCounter -> ProgramCounter
calculateNextPc (Program p) (ProgramCounter pc) = ProgramCounter (pc + 1)
-- @+node:kajsa.20130215183429.1369: ** Interpreter
runBefungeProgram :: String -> String
runBefungeProgram program = interpretProgram (Program program) (ProgramCounter 0) []

interpretProgram :: Program -> ProgramCounter -> String -> String
interpretProgram p pc stack =
    case instructionAt p pc of
        '"' -> interpretString p nextPc stack
        ',' -> (head stack):(interpretProgram p nextPc (tail stack))
        '@' -> ""
        _ -> interpretProgram p nextPc stack
    where
        nextPc = calculateNextPc p pc

interpretString :: Program -> ProgramCounter -> String -> String
interpretString p pc stack =
    case instructionAt p pc of
        '"' -> interpretProgram p nextPc stack
        c -> interpretString p nextPc (c:stack)
    where
        nextPc = calculateNextPc p pc
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
--    assertGivesOutput (">                            v\n" ++
--                       "@ ,,,,,,,,,,,,\"Hello World!\" <")
--                      "Hello World!"
-- @-others
-- @-leo
