import System.Directory
import Data.Char ( isLetter )
import Data.Char ( isDigit )

-- Main

createMass :: FilePath -> FilePath -> String -> IO ()
createMass srcFile targetFile text = 
  do s <- readFile srcFile
     writeFile targetFile (addText (removeNonLetters s) text) 
	 
-- Oneliner

createMass2 :: FilePath -> FilePath -> String -> IO ()
createMass2 srcFile targetFile text = 
  do s <- readFile srcFile
     writeFile targetFile 
	 (concat (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") 
	             (map (\x -> filter(\y -> isLetter y || isDigit y) x) (words s))))

-- Interface

createMassApp :: IO()
createMassApp = 
  do putStrLn ("Please specify the filepath to file containing the character names> ")
     src <- getLine
     putStrLn ("Please specify the filepath where you want your script to go> ")	 
     target <- getLine
     putStrLn ("Please specify the text you want to send> ")
     text <- getLine
     createMass src target text
	 
-- API
	 
removeNonLetters :: String -> [String]
removeNonLetters s = map (\x -> filter(\y -> isLetter y || isDigit y) x) (words s)

addText :: [String] -> String -> String
addText s text = 
        concat (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") s)
		



       