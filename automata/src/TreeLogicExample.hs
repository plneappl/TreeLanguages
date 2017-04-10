module TreeLogicExample where

import TreeLogic
import RoseTree
import Lib
import Automaton
import qualified Data.Set as DS
import Alphabet
import DeterministicAutomaton

instance Alphabet Int where
  allLetters = DS.fromList [0, 1, 2, 3, 4, 5]
ex0, ex1, ex2 :: RT Int
ex0 = Lf 0
ex1 = Lf 1
ex2 = Br 2 [ex0, ex1]
ex0', ex1', ex2' :: RT (Int, DS.Set Ident)
ex0' = fmap (\x -> (x, DS.empty)) ex0
ex1' = fmap (\x -> (x, DS.empty)) ex1
ex2' = fmap (\x -> (x, DS.empty)) ex2

p1, p2, p3, p4 :: Path
p1 = Eps
p2 = PLeft $ Eps
p3 = PRight $ Eps
p4 = PLeft $ p2

f1, f2, f2', f2'', f2''', f3, f4 :: Formula Int
f1 = ExistsFO "x" (Label "x" 1)
f2 = Not $ ExistsFO "x" $ ExistsFO "y" (Label "x" 2 `And` LChild "x" "y")
f2' = forallFO "x" $ forallFO "y" (Not (Label "x" 2) `Or` (Not (LChild "x" "y")))
f2'' = Not (Label "x" 2) `Or` (Not (LChild "x" "y"))
f2''' = bindAllVars f2''

f3 = LChild "x" "y"
f4 = RChild "x" "y"

d1 = formulaToDTA f1 
d2 = formulaToDTA f2 
d3 = formulaToDTA f3 
d4 = formulaToDTA f4

ex3, ex4, ex5 :: RT (Int, DS.Set Ident)
ex3 = Br (0, DS.singleton "x") [Lf (0, DS.singleton "y"), Lf (0, DS.singleton "y")]
ex4 = Br (0, DS.singleton "x") [Lf (0, DS.singleton "y"), Lf (0, DS.singleton "z")]
ex5 = Br (0, DS.singleton "x") [Lf (0, DS.singleton "z"), Lf (0, DS.singleton "y")]

main :: IO ()
main = do
  fixIOWin
  putStrLn (letterAtPath p1 ex1 `expect` Just 1 )   
  putStrLn (letterAtPath p1 ex2 `expect` Just 2 )   
  putStrLn (letterAtPath p2 ex1 `expect` Nothing)    
  putStrLn (letterAtPath p2 ex2 `expect` Just 0 )   
  putStrLn (letterAtPath p3 ex2 `expect` Just 1 )   
  putStrLn (letterAtPath p4 ex2 `expect` Nothing)    
  putStrLn (treeAccepted f1 ex0 `expect` False  )  
  putStrLn (treeAccepted f1 ex1 `expect` True   ) 
  putStrLn (treeAccepted f1 ex2 `expect` True   ) 
  putStrLn (treeAccepted f2 ex0 `expect` True   ) 
  putStrLn (treeAccepted f2 ex1 `expect` True   ) 
  putStrLn (treeAccepted f2 ex2 `expect` False  )  
  putStrLn (f2 `expect` f2')
  print (freeVars f2)
  print (freeVars f2'')
  putStrLn (f2 `expect` f2''')
  putStrLn (treeAccepted f1 ex0 `expect` automatonAccepts d1 ex0')
  putStrLn (treeAccepted f1 ex1 `expect` automatonAccepts d1 ex1')
  putStrLn (treeAccepted f1 ex2 `expect` automatonAccepts d1 ex2')
  putStrLn (automatonAccepts d3 ex3 `expect` True)
  putStrLn (automatonAccepts d3 ex4 `expect` True)
  putStrLn (automatonAccepts d3 ex5 `expect` False)
  putStrLn (automatonAccepts d4 ex3 `expect` True)
  putStrLn (automatonAccepts d4 ex4 `expect` False)
  putStrLn (automatonAccepts d4 ex5 `expect` True)
  putStrLn (treeAccepted f2 ex0 `expect` automatonAccepts d2 ex0')
  putStrLn (treeAccepted f2 ex1 `expect` automatonAccepts d2 ex1')
  putStrLn (treeAccepted f2 ex2 `expect` automatonAccepts d2 ex2')