module Lib
    ( someFunc
    ) where

import Types

tabsNum :: Int
tabsNum = 2

{-
	object
	array
	string
	number
	boolean
-}

someFunc :: IO ()
someFunc = do
    putStrLn "== TEST 1 =="
    putStrLn (parseObj 0 [("array", (NodeArr [(StrValue "a"), (StrValue "b"), (StrValue "c")]))])
    putStrLn "== TEST 2 =="
    putStrLn (parseObj 0 [("string", (StrValue "abc")), ("number", (NumValue 1)), ("boolean", (BoolValue True))])
    putStrLn "== TEST 3 =="
    putStrLn (parseObj 0 [("a", (NodeObj [("bool", (BoolValue True)), ("array", (NodeArr [(StrValue "abc"), (NumValue 2)]))])), ("b", (StrValue "2"))])

parseObj :: Int -> Object -> String
parseObj offset obj = "{\n" ++ (parseObjItems (offset + tabsNum) obj) ++ (spaces offset) ++ "}"

parseObjItems :: Int -> Object -> String
parseObjItems _ [] = ""
parseObjItems offset ((x, (NodeObj y)) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\": "
    ++ (parseObj offset y) ++ ",\n" ++ (parseObjItems offset t)
parseObjItems offset ((x, (NodeArr y)) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\": "
    ++ (parseArr offset y) ++ ",\n" ++ (parseObjItems offset t)
parseObjItems offset ((x, (StrValue y)) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\": \"" 
    ++ y ++ "\",\n" ++ (parseObjItems offset t)
parseObjItems offset ((x, (NumValue y)) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\": " ++ (show y) ++ ",\n"
    ++ (parseObjItems offset t)
parseObjItems offset ((x, (BoolValue y)) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\": " ++ (showBool y) ++ ",\n"
    ++ (parseObjItems offset t)


parseArr :: Int -> Array -> String
parseArr offset arr = "[\n" ++ (parseArrItems (offset + tabsNum) arr) ++ (spaces offset) ++ "]"

parseArrItems :: Int -> Array -> String
parseArrItems _ [] = ""
parseArrItems offset ((NodeObj x) : (t)) = (spaces offset) ++ (parseObj (offset + tabsNum) x) ++ ",\n" 
    ++ (parseArrItems offset t)
parseArrItems offset ((NodeArr x) : (t)) = (spaces offset) ++ (parseArr (offset + tabsNum) x) ++ ",\n"
    ++ (parseArrItems offset t)
parseArrItems offset ((StrValue x) : (t)) = (spaces offset) ++ "\"" ++ x ++ "\",\n"
    ++ (parseArrItems offset t)
parseArrItems offset ((NumValue x) : (t)) = (spaces offset) ++ (show x) ++ ",\n"
    ++ (parseArrItems offset t)
parseArrItems offset ((BoolValue x) : (t)) = (spaces offset) ++ (showBool x) ++ ",\n"
    ++ (parseArrItems offset t)

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

spaces :: Int -> String
spaces offset = [' ' | _ <- [1..offset]]

