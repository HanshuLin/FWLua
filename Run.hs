import ParserTD
import Executer
import SymTab
import System.IO.Unsafe
import Data.IORef

test f = do
  putStrLn $ "***Testing: " ++ f ++ " ***"
  putStrLn $ ""
  printParseTree f
  t <- parseExp f
  excuteFile t
  putStrLn ""

run :: IO ()
run = do
  test "test.imp"