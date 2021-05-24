import System.IO ()
import Data.Char ()
import Data.List ( foldl' )

getTags :: String -> String
getTags [] = []
getTags (x : xs) | x == '<' = (x : getCore xs) ++ getTags xs
                 | otherwise = getTags xs

getCore :: String -> String
getCore [] = []
getCore (x : xs) | x == ' ' = getEnd xs
                 | x == '>' = x : [' ']
                 | otherwise = x : getCore xs

getEnd :: String -> String
getEnd [] = []
getEnd (x : xs) | x == '>' = x : [' ']
                | otherwise = getEnd xs

isBalanced :: Foldable t => t [Char] -> Bool
isBalanced xs = null $ foldl' op [] xs
  where
    op ("<html>":xs) "</html>" = xs
    op ("<head>":xs) "</head>" = xs
    op ("<body>":xs) "</body>" = xs
    op ("<title>":xs) "</title>" = xs
    op ("<h>":xs) "</h>" = xs
    op ("<p>":xs) "</p>" = xs
    op ("<ul>":xs) "</ul>" = xs
    op ("<li>":xs) "</li>" = xs
    op ("<a>":xs) "</a>" = xs
    op ("<div>":xs) "</div>" = xs
    op xs x = x:xs

checkBalanced :: [String] -> Bool
checkBalanced xs | isBalanced xs = True
                 | otherwise = False

checkValid :: [String] -> Bool 
checkValid [] = True
checkValid (x : xs) | x == "<html>" || x == "</html>" = checkValid xs
                    | x == "<head>" || x == "</head>" = checkValid xs
                    | x == "<body>" || x == "</body>" = checkValid xs
                    | x == "<title>" || x == "</title>" = checkValid xs
                    | x == "<h>" || x == "</h>" = checkValid xs
                    | x == "<p>" || x == "</p>" = checkValid xs
                    | x == "<ul>" || x == "</ul>" = checkValid xs
                    | x == "<li>" || x == "</li>" = checkValid xs
                    | x == "<a>" || x == "</a>" = checkValid xs
                    | x == "<div>" || x == "</div>" = checkValid xs
                    | otherwise = False 

removeSpecial :: [String] -> [String]
removeSpecial [] = []
removeSpecial (x : xs) | x == "<br>" || x == "<hr>" = removeSpecial xs
                       | x == "<h1>" || x == "<h2>" || x == "<h3>" = "<h>" : removeSpecial xs
                       | x == "</h1>" || x == "</h2>" || x == "</h3>" = "</h>" : removeSpecial xs
                       | otherwise = x : removeSpecial xs

checkSemantics :: [String] -> IO()
checkSemantics xs | not (checkValid xs) = putStrLn "Invalid tags"
                  | not (checkBalanced xs) = putStrLn "Unbalanced tags"
                  | not (checkHtml xs) = putStrLn "MUST start and end with SINGLE pair of html tags"
                  | not (checkHead xs) = putStrLn "Html tags MUST contain SINGLE PAIR of head tag"
                  | not (checkTitle xs) = putStrLn "Head tags MUST contain SINGLE PAIR of title tags\n Title tags CANNOT contain ANY other tags"
                  | not (checkBody xs) = putStrLn "Html tags MUST contain SINGLE PAIR of body tag"
                  | not (checkDiv xs) = putStrLn "Div tags CANNOT be nested inside p tags"
                  | not (checkP xs) = putStrLn "P tags CANNOT be nested inside other p tags"
                  | otherwise = putStrLn "No parse errors"

checkHtml :: [String] -> Bool
checkHtml xs | head xs == "<html>" && last xs == "</html>" && tagOccurences "<html>" xs == 1 && tagOccurences "</html>" xs == 1 = True
             | otherwise = False

checkHead :: [String] -> Bool
checkHead xs | checkAdjacent "<html>" "<head>" xs && checkAdjacent "</title>" "</head>" xs && tagOccurences "<head>" xs == 1 && tagOccurences "</head>" xs == 1 = True
             | otherwise = False

checkTitle :: [String] -> Bool
checkTitle xs | checkAdjacent "<head>" "<title>" xs && checkAdjacent "<title>" "</title>" xs && tagOccurences "<title>" xs == 1 && tagOccurences "</title>" xs == 1 = True
              | otherwise = False

checkBody :: [String] -> Bool
checkBody xs | checkAdjacent "</head>" "<body>" xs && checkAdjacent "</body>" "</html>" xs && tagOccurences "<body>" xs == 1 && tagOccurences "</body>" xs == 1 = True
             | otherwise = False

checkDiv :: [String] -> Bool
checkDiv xs | not (checkNested "<p>" "</p>" "<div>" xs) = True
            | otherwise = False

checkP :: [String] -> Bool
checkP xs | not (checkNested "<p>" "</p>" "<p>" xs) = True
          | otherwise = False

checkNested :: String -> String -> String -> [String] -> Bool
checkNested a b y [] = False
checkNested a b y (x : xs) | x == a = findFirst b y xs || checkNested a b y xs
                           | otherwise = checkNested a b y xs

--FIND 2nd INPUT BEFORE 1st
findFirst :: String -> String -> [String] -> Bool
findFirst a b [] = False
findFirst a b (x : xs) | x == a = False
                       | x == b = True
                       | otherwise = findFirst a b xs

checkAdjacent :: String -> String -> [String] -> Bool
checkAdjacent a b [] = True
checkAdjacent a b (x : xs) | x == a && head xs == b = True 
                           | x == a && head xs /= b = False
                           | otherwise = checkAdjacent a b xs

tagOccurences :: String -> [String] -> Int
tagOccurences t [] = 0
tagOccurences t (x : xs) | x == t = 1 + tagOccurences t xs
                         | otherwise = tagOccurences t xs

main :: IO ()
main = do  
   let file = "file.html" 
   contents <- readFile file 
   let tags = words (getTags contents)
   checkSemantics (removeSpecial tags)
   