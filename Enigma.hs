{-- 
  Enigma.hs- A Haskell implementation of the Enigma machine
  by- Mohammad Tahir Khan
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String,Int) 
  type Reflector = [(Char,Char)] 
  type Offsets = (Int,Int,Int) 
  type Stecker = [(Char,Char)] 
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  {- advance: advances the rotors by one position. The first rotor advances
     every time, the second rotor advances every time the first rotor
     advances to its knock-on position, and the third rotor advances every
     time the second and first rotor advances to its knock-on positions. 
     args: lr, mr, rr: the left, middle, and right rotors
           (lrOffset, mrOffset, rrOffset): the offsets of the rotors
     returns: (newlrOffset, newmrOffset, newrrOffset): the new offsets of
              the rotors
   -}
  advance :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  advance lr mr rr (lrOffset, mrOffset, rrOffset) = (newlrOffset, newmrOffset, newrrOffset)
    where
      newrrOffset = if rrOffset >= snd rr then (rrOffset + 1) `mod` 26 else rrOffset + 1
      newmrOffset = if newrrOffset == snd rr then (mrOffset + 1) `mod` 26 else mrOffset
      newlrOffset = if newmrOffset == snd mr && newrrOffset == snd rr then (lrOffset + 1) `mod` 26 else lrOffset
                                                    
  {- encodeRToL: encodeRToL encodes a key that is passed through the rotor 
     from right to left with a specific offset
     args: rotor: the rotor
           rotorOffset: the offset of the rotor
           key: the key
     returns: the encoded key
   -}
  encodeRToL :: Rotor -> Int -> Char -> Char
  encodeRToL rotor rotorOffset key = 
    posAlpha((alphaPos (fst rotor !! ((alphaPos key + rotorOffset) `mod` 26)) - rotorOffset) `mod` 26)
 
  {- reflect: reflect reflects a character that is passed through the reflector
     args: reflector: the reflector
           character: the character
     returns: the reflected character
   -}
  reflect :: Reflector -> Char -> Char
  reflect [] x = x
  reflect ((a,b):xs) x 
    | x == a = b 
    | x == b = a 
    | otherwise = reflect xs x 

  {- encodeLToR: encodeLToR encodes a key that is passed through the rotor 
     from left to right with a specific offset
     args: rotor: the rotor
           rotorOffset: the offset of the rotor
           key: the key
     returns: the encoded key
   -}
  encodeLToR :: Rotor -> Int -> Char -> Char
  encodeLToR rotor rotorOffset key = 
    posAlpha (((findChar (fst rotor) (posAlpha((alphaPos key + rotorOffset) `mod` 26))) - rotorOffset) `mod` 26)

  {- findChar: findChar finds the index of a character in a string
     args: rotor: the rotor
           c: the character
     returns: the index of the character
   -}
  findChar :: String -> Char -> Int
  findChar rotor c = fromJust (findIndex (isPrefixOf [c]) (tails rotor))

  {- stecker: stecker encodes a character that is passed through the stecker
     args: plugBoard: the stecker
           c: the character
     returns: the encoded character
   -}
  stecker :: Stecker -> Char -> Char
  stecker plugBoard c = reflect plugBoard c

  {- encodeChar: encodeChar encodes a character that is passed through the enigma
     args: c: the character
           enigma: the enigma
     returns: the encoded character
   -}
  encodeChar :: Char -> Enigma  -> Char
  encodeChar c (SimpleEnigma lr mr rr reflector (ol, om, or)) = 
    encodeLToR rr advor (encodeLToR mr advom (encodeLToR lr advol 
      (reflect reflector 
        (encodeRToL lr advol (encodeRToL mr advom (encodeRToL rr advor c))))))
    where
        (advol, advom, advor) = advance lr mr rr (ol, om, or)
        
  encodeChar c (SteckeredEnigma lr mr rr reflector (ol, om, or) steck) = 
    stecker steck (encodeChar (stecker steck c) (SimpleEnigma lr mr rr reflector (ol, om, or)))

  {- encodeMessage: encodeMessage encodes a message that is passed through the enigma
     args: message: the message
           enigma: the enigma
     returns: the encoded message
   -}
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


{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] 
  type Crib = [(Char,Char)] 
  type IndexedCribItem = (Int, (Char, Char))
  type IndexedCrib = [IndexedCribItem]

  {- indexCrib: indexCrib indexes a crib
     args: crib: the crib
     returns: the indexed crib
   -}
  indexCrib :: Crib -> [IndexedCribItem]
  indexCrib crib = zip [0..] (crib)

  {- findIndexedCribItem: findIndexedCribItem finds all the items in the indexed crib
     that have a specific plain character
     args: char: the character
           cribi: the indexed crib
     returns: the items in the indexed crib that have the specific plain character
   -}
  findIndexedCribItem :: Char -> IndexedCrib -> [IndexedCribItem]
  findIndexedCribItem char [] = []
  findIndexedCribItem char ((i,(p,c)):xs) = if p == char then (i,(p,c)) : findIndexedCribItem char xs 
                                            else findIndexedCribItem char xs

  {- findMenus: findMenus finds all the menus in the indexed crib with particular starting items -}
  findMenus :: IndexedCrib -> [IndexedCribItem] -> [Menu]
  findMenus cribi [] = []
  findMenus cribi ((i,(p,c)):xs)  | findIndexedCribItem c cribi /= [] = 
                                    (map (i:)) (findMenus newCribi (findIndexedCribItem c newCribi)) ++ (findMenus cribi xs)
                                  | otherwise = [[i]] ++ findMenus cribi xs
                                 where
                                   newCribi = [(ci,(cp,cc)) | (ci,(cp,cc)) <- cribi , ci /= i]
  
  {- findLongestMenu: findLongestMenu finds the longest menu in a list of menus -}
  findLongestMenu :: [Menu] -> Menu
  findLongestMenu menu = head (sortBy (\xs ys -> compare (length ys) (length xs)) menu)

  {- longestMenu: longestMenu finds the longest menu in a crib
     args: crib: the crib
     returns: the longest menu in the crib
   -}
  longestMenu :: Crib -> Menu
  longestMenu crib | length (cribi) == 0 = []
                   | otherwise = findLongestMenu(findMenus cribi cribi)
                  where 
                    cribi = indexCrib crib


{- Part 3: Simulating the Bombe -}

  {- andvaceByInt: advanceByInt advances the rotors by a specific amount
     args: offsets: the offsets
           amount: the amount
           count: the count (used for recursion)
     returns: the new offsets
   -}
  advanceByInt :: Offsets -> Int -> Int -> Offsets
  advanceByInt (ol, om, or) amount count | count == amount = (ol, om, or)
                                         | otherwise = advanceByInt (newOl, newOm, newOr) amount (count+1)
                                         where
                                           (newOl, newOm, newOr) = advance rotor1 rotor2 rotor3 (ol, om, or)

  {- addSteckerPair: addSteckerPair adds a stecker pair to the board
     args: stecker: the stecker pair
           board: the Steckerboard
     returns: the new board
   -}
  addSteckerPair :: Stecker -> Stecker -> Maybe Stecker
  addSteckerPair [(a,b)] board | ((a,b) `elem` board) || ((b,a) `elem` board) = Just board
                               | (charInStecker a board) || (charInStecker b board) = Nothing
                               | otherwise = Just ((a,b):board)

  {- charInStecker: charInStecker checks if a character is in the stecker board
     args: char: the character
           board: the Steckerboard
     returns: True if the character is in the board, False otherwise
   -}
  charInStecker :: Char -> Stecker -> Bool
  charInStecker char ((x,y):xs) | length steck == 1 = if x == char || y == char then True else False
                                | otherwise =  if x == char || y == char then True else charInStecker char xs
                                where 
                                  steck = ((x,y):xs)

  {- followThroughMenu: followThroughMenu follows a menu through the crib, 
     adding stecker pairs to the board. If the function reaches it's end and a 
     complete board is found, it is returned, otherwise Nothing is returned
     args: crib: the crib
           menu: the menu
           board: the Steckerboard
           offsets: the offsets
     returns: Maybe a new board
   -}
  followThroughMenu :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  followThroughMenu crib (x:xs) board (ol, om, or) 
    | length (x:xs) == 0 = Just board
    | (addSteckerPair [(encodeC, (snd (crib !! x)))] board) == Nothing = Nothing
    | length (x:xs) == 1 = addSteckerPair [(encodeC, (snd (crib !! x)))] board
    | otherwise = followThroughMenu crib (xs) (fromJust (addSteckerPair [(encodeC, (snd (crib !! x)))] board)) (ol, om, or)
    where
      encodeC = encodeChar (stecker board (fst (crib !! x))) (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (advanceByInt (ol, om, or) (x+1) 0))

  {- nextSteckerPair: nextSteckerPair finds the next stecker pair in the alphabet
     args: stecker: the stecker pair
     returns: the next stecker pair
   -}
  nextSteckerPair :: Stecker -> Stecker
  nextSteckerPair [(a,b)] = [(a, (['A'..'Z'] !! (mod ((alphaPos b) + 1) 26)))]

  {- searchStecker: searchStecker searches for a stecker board that follows the menu
     args: crib: the crib
           menu: the menu
           board: the Steckerboard
           offsets: the offsets
     returns: Maybe a new board
   -}
  searchStecker :: Crib -> Menu -> Stecker -> Offsets -> Maybe Stecker
  searchStecker crib menu [(a,b)] (ol, om, or) | followThroughMenu crib menu [(a,b)] (ol, om, or) == Nothing =
                                                  if alphaPos b == (alphaPos a - 1) `mod` 26 then Nothing else searchNextStecker
                                               | otherwise = followThroughMenu crib menu [(a,b)] (ol, om, or)
                                              where 
                                                searchNextStecker = searchStecker crib menu (nextSteckerPair [(a,b)]) (ol, om, or)

  {- breakingEnigma: breakingEnigma is a helper function for breakEnigma,
     which searches for a stecker board that follows the menu using a particular offsets,
     if it is found, it is returned, otherwise the offsets are advanced and the function is 
     called again until the offsets are (25,25,25), at which point Nothing is returned
     args: crib: the crib
           menu: the menu
           board: the Steckerboard
           offsets: the offsets
     returns: Maybe an Offsets and a new board
   -}
  breakingEnigma :: Crib -> Menu -> Stecker -> Offsets -> Maybe (Offsets, Stecker)
  breakingEnigma crib menu steck (ol, om, or)  
    | (searchStecker crib menu steck (ol, om, or)) == Nothing =
      if (ol, om, or) == (25,25,25) then Nothing 
      else breakingEnigma crib menu steck (advanceByInt (ol, om, or) 1 0)
    | otherwise =  Just ((ol, om, or) , fromJust (searchStecker crib menu steck (ol, om, or)))

  {- breakEnigma: breakEnigma is the main function for breaking the Enigma,
     it calls breakingEnigma with the longest menu in the crib, an initial stecker board
     and the offsets (0,0,0)
     args: crib: the crib
     returns: Maybe an Offsets and a new board
   -}
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

  {- posAlpha: given an index in the alphabet, returns the corresponding
     uppercase letter ('A' = position 0; 'Z' = position 25)
   -}
  posAlpha :: Int -> Char
  posAlpha i = chr (ord 'A' + i) 