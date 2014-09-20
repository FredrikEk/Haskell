import System.Directory
import Data.Char ( isLetter )

--"Readable" Code

createMass :: FilePath -> String -> IO ()
createMass file text = 
  do s <- readFile file
     writeFile "MassTell.txt" (addText (removeNonLetters s) text) 

removeNonLetters :: String -> [String]
removeNonLetters s = map (\x -> filter(isLetter) x) (words s)

addText :: [String] -> String -> String
addText s text = 
        concat (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") s)

--Oneliner

createMass2 :: FilePath -> String -> IO ()
createMass2 file text = 
  do s <- readFile file
     writeFile "MassTell.txt" 
	 (concat (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") 
	             (map (\x -> filter(isLetter) x) (words s))))
       