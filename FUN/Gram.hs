-- Nazev projektu: PLG-2-NKA
-- Jmeno: Petr Polansky
-- Login: xpolan07

module Gram
( bezSi   
  ,parsePra
  ,parseFst
  ,printPra
  ,bezCar
  ,malaCount
  ,update
  ,stringAInt
  ,novyNeterm
  ,odstJed
  ,seznamPra
) where  
  
import Data.Char
import System.IO
import Control.Monad
import Data.List
import Data.Function
import System.Environment

-- slouzi k rozdeleni pravidla na levou a pravou stranu bez sipek
bezSi xs =  [(a,d)]
        where
            (a,b) = splitAt 1 xs
            (c,d) = splitAt 2 b

-- rozdeli pravidla podle sipek a ulozi
parsePra line = bez 
	where
	all = lines line
	bez = foldl (\acc x -> acc ++ bezSi x ) [] all	

-- projde seznam n-tic a pospojuje je sipkou a ulozi do seznamu 
printPra pra = foldr (\(x,y) acc -> (x++ "->" ++y) : acc) [] pra


-- vypis bez car
-- funguje pro jeden znak
parseFst fst = bezCar $ show fst 

-- vyfiltrujeme cary
bezCar xs = filter (not. any isPunctuation) . groupBy ((==) `on` isPunctuation) $ xs	

-- urceni poctu malych pismen v retezci
malaCount xs = foldl (\acc x -> if isLower x then acc + 1 else acc) 0 xs 


-- projde seznam n-tic a rozepise pravidla s vice TERMY
update pra = celkoveZprac pra 1


-- pricte k retezci cislo
stringAInt retezec cislo = retezec ++ show cislo


-- vytvari novou pravou stranu pravidla
-- retezec, znak, cislo 
novaPra2 retezec1 retezec2 znak cislo = bezpos 
	where 
    dale = stringAInt retezec1 cislo -- A1
    jeste = [znak] ++ dale -- aA1
    bezpos = [(retezec2,jeste)]


-- vrati ocislovany seznam neterminalu 
ocislovSez retezec seznam rozsah = [retezec] ++ zbytekA retezec rozsah


-- pricita k netermu cislo
zbytekA a [] = []
zbytekA a (x:xs) = stringAInt a x : zbytekA a xs


-- levy NETERM nebo posledni znak retezce, ocislovany seznam NETERMU, pravidlo, cislo
-- bere posledni znaky retezce, cislo. seznam, pravidlo, cislo a preda novaPra2 pro vytvoreni noveho pravidla (A1, "aA2")
-- nakonec vezme levy NETERM, cislo. seznam, pravidlo, cislo a preda novaPra3, ktery vytvori ("A1", "bB")
splits3 h (v:vs) (s:x:[]) _ = novaPra3 ([x]) v s
splits3 h (v:vs) (x:xs) (y:ys) = novaPra2 h v x y ++ splits3 h vs xs ys

-- variata pro posledni pravidlo, kde pouze spoji terminal s puvodnim netermem 
-- posledni znak retezce, ocisl. retezec, znak
novaPra3 retezec1 retezec2 znak = bezpos
    where
    jeste = [znak] ++ retezec1 -- aA
    bezpos = [(retezec2,jeste)]


-- aktualizovani seznamu netermu
novyNeterm xs = foldr (\(x,y) acc-> x :acc) [] xs


-- pro aaa bere i posledni znak retezce
splits4 h (v:vs) [] _ = novaPra4 [h] v
splits4 h (v:vs) (x:xs) (y:ys) = novaPra2 h v x y ++ splits4 h vs xs ys

-- posledni pravidlo pro variantu aaa
-- ocisl. retezec a znak #
novaPra4 retezec1 retezec2 = [(retezec2,['#'])]



-- prochazi seznam n-tic, ktere jsou s vice TERMY tak zpracuje, ostatni pouze pripoji
-- jako rozsah pro split3, splits4 bereme cislo, ktere zacina 1 az po cislo + pocet malych pismen v retezci 
celkoveZprac [] _ = []
celkoveZprac ((x,y):seznam) cislo 
    | malaCount y == 1 && all isLower y = splits4 x (ocislovSez x y [cislo .. (cislo + malaCount y)]) y [cislo .. (cislo + malaCount y)] ++ celkoveZprac seznam ((cislo) + malaCount y)      
    | malaCount y == 1 = [(x,y)] ++ celkoveZprac seznam (cislo)
    | malaCount y > 1 && all isLower y = splits4 x (ocislovSez x y [cislo .. (cislo + malaCount y)]) y [cislo .. (cislo + malaCount y)] ++ celkoveZprac seznam ((cislo) + malaCount y)      
    | malaCount y < 1 = [(x,y)] ++ celkoveZprac seznam ((cislo) + malaCount y)                 
    | otherwise = splits3 x (ocislovSez x y [cislo .. (cislo + malaCount y)]) y [cislo .. (cislo + malaCount y)] ++ celkoveZprac seznam ((cislo - 1) + malaCount y)         


-- funkce pro pretvoreni seznamu pravidel bez jednoduchych pravidel
-- seznam ntic, seznam ntic
-- kazdou pravou stranu overuje, zda to neni jednoduche pravidlo
-- pokud je, tak pro kazde jednoduche pravidlo pretvori seznam ntic
odstJed [] _ = []
odstJed ((x,y):zbytek) all
    | malaCount y < 1 && elem '#' y = [(x,y)] ++ odstJed zbytek all    
    | malaCount y < 1 = foldl (\acc prvek -> [(x, prvek)] ++ acc ) [] (seznamPra y all all) ++ odstJed zbytek all
    | otherwise = [(x,y)] ++ odstJed zbytek all

-- pro kazdy neterminal najde vrati seznam jejich pravych stran pravidel
-- hledany retezec, zbytek pravidel, celkovy seznam pravidel
-- porovna retezec s levou stranou, pokud se rovnaji, tak vrati pravou stranu

seznamPra _ [] _ = []
seznamPra hledany ((x,y):zbytek) cely 
    | hledany == x && malaCount y < 1 && elem '#' y  = [y] ++ seznamPra hledany zbytek cely 
    | hledany == x && malaCount y < 1 = seznamPra y cely cely ++ seznamPra hledany zbytek cely 
    | hledany == x && malaCount y >= 1 = [y] ++ seznamPra hledany zbytek cely                               
    | otherwise = seznamPra hledany zbytek cely   
