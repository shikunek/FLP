-- Nazev projektu: PLG-2-NKA
-- Jmeno: Petr Polansky
-- Login: xpolan07

import Data.Char
import System.IO
import Control.Monad
import Data.List
import Data.Function
import System.Environment
import Gram
import KonAut


main = do  
		args <- getArgs
    		procArgs args
			

-- parse list of arguments into couple
procArgs [] = error "mozne pouze 1 nebo 2 parametry"  -- not really needed in this program but you can do something similar,
procArgs [x] 
    | x=="-1" = readStdin x
    | x=="-2" = readStdin x
    | x=="-i" = readStdin x
    | otherwise = error "spatna volba parametru"
procArgs [x,y]
    | x=="-1" = cteni x y
    | x=="-2" = cteni x y
    | x=="-i" = cteni x y
    | otherwise = error "unknown argument"
procArgs _ = error "mozne pouze 1 nebo 2 parametry"



-- zpracovani standardniho vstupu
readStdin arg = do
                pole <- retezec ""                
                putStrLn $ reverse $ dropWhile isSpace $ reverse $ unlines $ vypis arg pole
            
                where
                    retezec str = do
                        l <- getLine
                        if length l == 0
                            then return str
                            else
                                retezec (str ++  l++ "\n") 

-- pro cteni ze souboru
cteni arg file = do
    withFile file ReadMode (\handle -> do 
        
    	l <- hGetContents handle  

    	-- KONECNA VERZE
    	let celkovy = vypis arg l
    	putStrLn $ reverse $ dropWhile isSpace $ reverse $ unlines celkovy

     	)
        

getFirst (x:_) = x
getSecond (x:y:_) = y
getThird (x:y:z:_) = z

vypis arg l
	| arg == "-i" = vypisVstupu l
	| arg == "-1" = vypisGram l 
	| arg == "-2" = vypisKA l


-- vypise vstup
-- spojit seznam neterminalu, seznam terminalu, pocatecni stav a pravidla
vypisVstupu l = seznamNON:seznamTERM:pocStav:[pravidla]
	where
		seznamNON = intercalate "," $ parseFst $ getFirst $ osetrene 
		seznamTERM = intercalate "," $ parseFst $ getSecond $ osetrene
		pocStav = getThird $ osetrene
		pravidla = unlines $ printPra $ parsePra $ unlines $ drop 3 $ osetrene
                osetrene = odstPrazdne $ lines l 

vypisKA l = stavy:pocStav:konStavy:[prechody]
	where
		aktual = update $ parsePra $ unlines $ drop 3 $ osetrene
                osetrene = odstPrazdne $ lines l
    		netermy = intercalate "," $ nub $ novyNeterm aktual
    		konecny = unlines $ printPra $ odstJed aktual aktual	
    		konAut = gramNaKA (nub $ novyNeterm aktual) (dvaNaTri (odstJed aktual aktual)) [1..]
    		stavy = intercalate "," $ nub $ seznamVsechKA konAut
    		pocStav = pocStavKA konAut
    		konStavy = intercalate "," $ nub $ seznamKonc konAut
    		prechody = unlines $ printKA konAut
                   


vypisGram l = netermy:b:c:[konecny]
	where
                osetrene = odstPrazdne $ lines l
		b = intercalate "," $ parseFst $ getSecond $ osetrene
		c = getThird $ osetrene
		aktual = update $ parsePra $ unlines $ drop 3 $ osetrene
    		netermy = intercalate "," $ nub $ novyNeterm aktual
    		konecny = unlines $ printPra $ odstJed aktual aktual	

odstPrazdne [] = []
odstPrazdne (x:xs) 
    | length x == 0 = odstPrazdne xs
    | otherwise = x : odstPrazdne xs

