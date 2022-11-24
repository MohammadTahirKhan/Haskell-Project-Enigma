import Enigma

enigma1 = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')] 
enigma2 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) plugboard)
crib1 = "WETTERVORHERSAGEBISKAYA"
message1 = "RWIVTYRESXBFOGKUHQBAISE"

crib2 = "TURINGBOMBEHASKELLSIMULATIONSTOP"
message2 = "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW"

{- Function that will print "No result!" if Maybe type contains Nothing, or the
 - contents of the "Just" part otherwise. -}
printMaybe :: (Show a) => Maybe a -> IO ()
printMaybe = maybe (putStrLn "No result!") print

{- This is the type of thing that will happen when testing your code. Note
 - that (1) You must have your code in a module called "Enigma". (2) The functions
 - encodeMessage, longestMenu and breakEnigma are expecting arguments in a
 - particular format. Make sure you write your types so that they are compatible.
 -
 - NOTE: The actual contents of the main function when testing your code will be
 - different to this. You SHOULD NOT submit this file, only Enigma.hs.
 -}
main = do
    print "First a test of encodeMessage: "
    print (encodeMessage "Here is a test input string." enigma1)
    print "And another test of encodeMessage: "
    print (encodeMessage "Here is a test input string." enigma2)
    print "Then a test of longestMenu: "
    print (longestMenu (zip crib1 message1))
    print "And now a test of breakEnigma: "
    printMaybe (breakEnigma (zip crib2 message2))
