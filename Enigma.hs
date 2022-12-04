{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  -- add extra imports if needed, but only standard library functions!

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String,Int) 
  type Reflector = [(Char,Char)] 
  type Offsets = (Int,Int,Int) 
  type Stecker = [(Char,Char)] 

  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  advance :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  advance lr mr rr (lrOffset, mrOffset, rrOffset) = (newlrOffset, newmrOffset, newrrOffset)
    where
      newrrOffset = if rrOffset >= snd rr then (rrOffset + 1) `mod` 26 else rrOffset + 1
      newmrOffset = if newrrOffset == snd rr then (mrOffset + 1) `mod` 26 else mrOffset
      newlrOffset = if newmrOffset == snd mr && newrrOffset == snd rr then (lrOffset + 1) `mod` 26 else lrOffset
                                                    

  encodeRToL :: Rotor -> Int -> Char -> Char
  encodeRToL rotor rotorOffset key = 
    posAlpha((alphaPos (fst rotor !! ((alphaPos key + rotorOffset) `mod` 26)) - rotorOffset) `mod` 26)

  reflect :: Reflector -> Char -> Char
  reflect [] c = c
  reflect ((a,b):xs) x 
    | x == a = b 
    | x == b = a 
    |otherwise = reflect xs x 

  encodeLToR :: Rotor -> Int -> Char -> Char
  encodeLToR rotor rotorOffset key = 
    posAlpha (((findChar (fst rotor) (posAlpha((alphaPos key + rotorOffset) `mod` 26))) - rotorOffset) `mod` 26)

  findChar :: String -> Char -> Int
  findChar rotor c = fromJust (findIndex (isPrefixOf [c]) (tails rotor))

  stecker :: Stecker -> Char -> Char
  stecker plugBoard c = reflect plugBoard c

  encodeChar :: Char -> Enigma  -> Char
  encodeChar c (SimpleEnigma lr mr rr reflector (ol, om, or)) = 
    encodeLToR rr advor (encodeLToR mr advom (encodeLToR lr advol 
      (reflect reflector 
        (encodeRToL lr advol (encodeRToL mr advom (encodeRToL rr advor c))))))
    where
        (advol, advom, advor) = advance lr mr rr (ol, om, or)
        
  encodeChar c (SteckeredEnigma lr mr rr reflector (ol, om, or) steck) = 
    stecker steck (encodeChar (stecker steck c) (SimpleEnigma lr mr rr reflector (ol, om, or)))

  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = [] 
  encodeMessage (x:xs) (SimpleEnigma lr mr rr reflector (ol, om, or)) =  
    encodeChar x simEnigma : encodeMessage xs simEnigmaAdv
    where
      simEnigma = (SimpleEnigma lr mr rr reflector (ol, om, or))
      simEnigmaAdv = (SimpleEnigma lr mr rr reflector (advance lr mr rr (ol, om, or)))

  encodeMessage (x:xs) (SteckeredEnigma lr mr rr reflector (ol, om, or) steck) = 
    encodeChar x steckEnigma : encodeMessage xs steckEnigmaAdv
    where
      steckEnigma = (SteckeredEnigma lr mr rr reflector (ol, om, or) steck)
      steckEnigmaAdv = (SteckeredEnigma lr mr rr reflector (advance lr mr rr (ol, om, or)) steck)


  {- You will need to add many more functions. Remember, design it carefully
   - and keep it simple! If things are feeling complicated, step back from your
   - code and think about the design again.
   -}


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] 
  type Crib = [(Char,Char)] 
  type IndexedCribItem = (Int, (Char, Char))
  type IndexedCrib = [IndexedCribItem]

  indexCrib :: Crib -> [IndexedCribItem]
  indexCrib crib = zip [0..] (crib)

  findItem :: Char -> IndexedCrib -> [IndexedCribItem]
  findItem char [] = []
  findItem char ((i,(p,c)):xs) = if p == char then (i,(p,c)) : findItem char xs else findItem char xs
  
  findMenus :: IndexedCrib -> [IndexedCribItem] -> [Menu]
  findMenus cribi [] = []
  findMenus cribi ((i,(p,c)):xs)  | findItem c cribi /= [] = 
                                    (map (i:)) (findMenus newCribi (findItem c newCribi)) ++ (findMenus cribi xs)
                                  | otherwise = [[i]] ++ findMenus cribi xs
                                 where
                                   newCribi = [(ci,(cp,cc)) | (ci,(cp,cc)) <- cribi , ci /= i]
  
  findLongestMenu :: [Menu] -> Menu
  findLongestMenu menu = head (sortBy (\xs ys -> compare (length ys) (length xs)) menu)

  longestMenu :: Crib -> Menu
  longestMenu crib | length (cribi) == 0 = []
                   | otherwise = findLongestMenu(findMenus cribi cribi)
                  where 
                    cribi = indexCrib crib

{- Part 3: Simulating the Bombe -}
  
  advanceByInt :: Offsets -> Int -> Int -> Offsets
  advanceByInt (ol, om, or) amount count | count == amount = (ol, om, or)
                                         | otherwise = advanceByInt (newOl, newOm, newOr) amount (count+1)
                                        where
                                          (newOl, newOm, newOr) = advance rotor1 rotor2 rotor3 (ol, om, or)

  addSteckerPair :: Stecker -> Stecker -> Maybe Stecker
  addSteckerPair [(a,b)] board | ((a,b) `elem` board) || ((b,a) `elem` board) = Just board
                               | (charInStecker a board) || (charInStecker b board) = Nothing
                               | otherwise = Just ((a,b):board)

  charInStecker :: Char -> Stecker -> Bool
  charInStecker char ((x,y):xs) | length steck == 1 = if x == char || y == char then True else False
                                | otherwise =  if x == char || y == char then True else charInStecker char xs
                                where 
                                  steck = ((x,y):xs)

  followMenu :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  followMenu crib (x:xs) board (ol, om, or) 
    | length (x:xs) == 0 = Just board
    | (addSteckerPair [(encodeC, (snd (crib !! x)))] board) == Nothing = Nothing
    | length (x:xs) == 1 = addSteckerPair [(encodeC, (snd (crib !! x)))] board
    | otherwise = followMenu crib (xs) (fromJust (addSteckerPair [(encodeC, (snd (crib !! x)))] board)) (ol, om, or)
    where
      encodeC = encodeChar (stecker board (fst (crib !! x))) (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (advanceByInt (ol, om, or) (x+1) 0))

  nextSteckerPair :: Stecker -> Stecker
  nextSteckerPair [(a,b)] = [(a, (['A'..'Z'] !! (mod ((alphaPos b) + 1) 26)))]

  findStecker :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  findStecker crib menu [(a,b)] (ol, om, or) | followMenu crib menu [(a,b)] (ol, om, or) == Nothing =
                                               if alphaPos b == (alphaPos a - 1) `mod` 26 then Nothing else findNextStecker
                                             | otherwise = followMenu crib menu [(a,b)] (ol, om, or)
                                            where 
                                              findNextStecker = findStecker crib menu (nextSteckerPair [(a,b)]) (ol, om, or)

  breakingEnigma :: Crib -> Menu -> Stecker -> Offsets -> Maybe (Offsets, Stecker)
  breakingEnigma crib menu steck (ol, om, or)  
    | (findStecker crib menu steck (ol, om, or)) == Nothing =
      if (ol, om, or) == (25,25,25) then Nothing 
      else breakingEnigma crib menu steck (advanceByInt (ol, om, or) 1 0)
    | otherwise =  Just ((ol, om, or) , fromJust (findStecker crib menu steck (ol, om, or)))

  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib = breakingEnigma crib (longestMenu crib) initialSteckerBoard (0,0,0)
    where
      initialSteckerBoard = [(fst (crib !! (head (longestMenu crib))), fst (crib !! (head (longestMenu crib))))]


{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  posAlpha :: Int -> Char
  posAlpha i = chr (ord 'A' + i) 