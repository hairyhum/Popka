import System.Environment
import System.Exit
import Data.MarkovChain
import Data.List.Split
import System.Random
import Data.List
import Data.Char
import System.Console.GetOpt
import ParseMidi

data Options = Options { in_file :: FileName, pattern_files :: [FileName], depth :: Int } deriving (Read, Show, Eq)

defaultOptions = 
  Options { in_file = "", pattern_files = [], depth = 5 } 

flags = 
  [ Option ['f'] [] (ReqArg (\arg opt -> return opt { in_file = arg }) "FILE") "Base FILE"
  , Option ['p'] [] (ReqArg (\arg opt -> return opt { pattern_files = splitOn "," arg }) "FILES") "Pattern FILES"
  , Option ['d'] [] (ReqArg (\arg opt -> return opt { depth = read arg })  "NUMBER") "Depth of search in Markov chain"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder flags args
  Options { in_file = file, pattern_files = files, depth = seek_depth } <- foldl (>>=) (return defaultOptions) actions
  case (file, files) of
    ("", _) -> putStrLn "Error! No input file."
    (_, []) -> putStrLn "Error! No pattern files."
    _ -> do
      name <- shuffle file files seek_depth
      putStrLn ("New file saved as "++name)



