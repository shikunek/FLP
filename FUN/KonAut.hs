-- Nazev projektu: PLG-2-NKA
-- Jmeno: Petr Polansky
-- Login: xpolan07

module KonAut
(seznamKonc,
	dvaNaTri,
	oddelTerm,
	gramNaKA,
	netermCislo,
	printKA,
	seznamVsechKA,
	pocStavKA
	)
where


-- vytvori seznam koncovych stavu
seznamKonc [] = []
seznamKonc ((x,y,z):xs) 
                | y == "#" = x : seznamKonc xs
                | otherwise = seznamKonc xs                                

-- seznam dvojic prevede na trojice
dvaNaTri xs = foldr (\x acc -> oddelTerm x : acc ) [] xs 

-- oddeli term od netermu a vytvori trojici
oddelTerm (x,y) = fx x y
            where 
                (a,b) = splitAt 1 y
                fx p q = (p,a,b)  


-- seznam NETERMU, seznam n-tic pravidel, seznam cisel
-- kazdym krokem zmeni seznam n-tic, ktery se pouzije v dalsim kroce
gramNaKA [] seznam _ = seznam
gramNaKA (x:xs) seznam (y:ys) = gramNaKA xs (netermCislo x seznam y) ys 

-- znak, seznam n-tic, cislo
-- pokud se znak shoduje s NETERMEM, tak je NETERM nahrazen cislem
netermCislo _ [] _ = []
netermCislo znak ((x,y,z):xs) cislo =
            if x == znak 
                then 
                    if z == znak
                        then 
                            (show cislo, y, show cislo) : netermCislo znak xs cislo
                        else 
                            (show cislo, y, z) : netermCislo znak xs cislo
                else
                    if z == znak
                        then 
                            (x, y, show cislo) : netermCislo znak xs cislo
                        else
                            (x,y,z) : netermCislo znak xs cislo        


-- Pretvoreni prechodu do finalni podoby pro tisk a odstraneni koncovych pravidel
printKA [] = []
printKA ((a,b,c):xs) 
			| b == "#" = printKA xs
			| otherwise = (a ++ "," ++ b ++ "," ++ c) : printKA xs                             

-- seznam vsech stavu konecneho automatu
seznamVsechKA [] = []
seznamVsechKA ((a,b,c):xs) = a: seznamVsechKA xs

-- pocatecni stav automatu
pocStavKA [] = []
pocStavKA ((a,b,c):_) = a 	