module TreeLogicExample where

import TreeLogic
import RoseTree
import Lib

ex0, ex1, ex2 :: RT Int
ex0 = Lf 0
ex1 = Lf 1
ex2 = Br 2 [ex0, ex1]

p1, p2, p3, p4 :: Path
p1 = Eps
p2 = PLeft $ Eps
p3 = PRight $ Eps
p4 = PLeft $ p2

f1, f2, f2', f2'', f2''' :: Formula Int
f1 = ExistsFO "x" (Label "x" 1)
f2 = Not $ ExistsFO "x" $ ExistsFO "y" (Label "x" 2 `And` LChild "x" "y")
f2' = forallFO "x" $ forallFO "y" (Not (Label "x" 2) `Or` (Not (LChild "x" "y")))
f2'' = Not (Label "x" 2) `Or` (Not (LChild "x" "y"))
f2''' = bindAllVars f2''

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