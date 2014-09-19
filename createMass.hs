import System.Directory

--"Readable" Code

createMass :: FilePath -> String -> IO ()
createMass file text = 
  do s <- readFile file
     writeFile "MassTell.txt" (addText (removeCommas s) text) 
	 
removeCommas :: String -> [String]
removeCommas s = map (reverse . tail . reverse) (words s)

addText :: [String] -> String -> String
addText s text = 
        (textAdder (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") s))

textAdder :: [String] -> String
textAdder [] = ""
textAdder (x:xs) = x ++ (textAdder xs)

--Oneliner

createMass2 :: FilePath -> String -> IO ()
createMass2 file text = 
  do s <- readFile file
     writeFile "MassTell.txt" 
	 (textAdder2 (map (\x -> "/tell " ++ x ++ " " ++ text ++ "\n" ++ "/delay 5000\n") 
	             (map (reverse . tail . reverse) (words s)))) where
       textAdder2 [] = ""
       textAdder2 (x:xs) = x ++ (textAdder xs) 