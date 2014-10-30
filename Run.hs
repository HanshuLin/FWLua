import Desugaring
import ParserTD
import Executer
import SymTab
import System.IO.Unsafe
import Data.IORef
import Data.Either

code = "rawcode.imp"

luaTest f = do
  putStrLn $ "***Testing: " ++ f ++ " ***"
  putStrLn $ ""
  printRawExp f
  Right f' <- desugarExp f
  writeFile code f'
  printParseTree code
  t <- parseExp code
  excuteFile t
  putStrLn ""
  
test code = do
  putStrLn $ "***Testing: " ++ code ++ " ***"
  putStrLn $ ""
  printParseTree code
  t <- parseExp code
  excuteFile t
  putStrLn ""

lua :: IO ()
lua = do
  luaTest "testlua.imp"
  
  
run :: IO ()
run = do
  test "test.imp"