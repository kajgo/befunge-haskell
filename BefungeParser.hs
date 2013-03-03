-- @+leo-ver=5-thin
-- @+node:kajsa.20130215180649.1361: * @file BefungeParser.hs
import Prelude hiding (Left, Right)
-- @+others
-- @+node:kajsa.20130228155956.1376: ** Program abstraction
data Program = Program [String]

data ProgramCounter = ProgramCounter Int Int Direction

data Direction = Up | Down | Left | Right

instructionAt :: Program -> ProgramCounter -> Char
instructionAt (Program p) (ProgramCounter x y dir) =
    (p !! y) !! x

calculateNextPc :: Program -> ProgramCounter -> ProgramCounter
calculateNextPc p pc@(ProgramCounter x y dir) =
    updatePc x y (calculateNextDirection dir (instructionAt p pc))

calculateNextDirection :: Direction -> Char -> Direction
calculateNextDirection _   '^' = Up
calculateNextDirection _   'v' = Down
calculateNextDirection _   '<' = Left
calculateNextDirection _   '>' = Right
calculateNextDirection dir  _  = dir

updatePc :: Int -> Int -> Direction -> ProgramCounter
updatePc x y Up    = ProgramCounter   x   (y-1) Up
updatePc x y Down  = ProgramCounter   x   (y+1) Down
updatePc x y Left  = ProgramCounter (x-1)   y   Left
updatePc x y Right = ProgramCounter (x+1)   y   Right
-- @+node:kajsa.20130215183429.1369: ** Interpreter
runBefungeProgram :: String -> String
runBefungeProgram program = interpretProgram (Program (lines program)) (ProgramCounter 0 0 Right) []

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
    assertGivesOutput (">                            v\n" ++
                       "@ ,,,,,,,,,,,,\"Hello World!\" <")
                       "Hello World!"
-- @-others
-- @-leo
