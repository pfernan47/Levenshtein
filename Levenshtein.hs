module Levenshtein where

import ENFA
import Data.Map
import Data.Set
import Data.List



levenshtein :: String -> Int -> [String] -> [Bool]
levenshtein w n (x:xs) = [if ((length x )<= (length w +n) ) then (oneWord w n x) else (False)] ++  levenshtein w n xs
levenshtein w n []  = []

oneWord w n v
            |Data.Set.null (Data.Set.intersection  (accepting(buildENFA w n)) (evaluateEnfa v (buildENFA w n)))  =  False
            |otherwise                                                                                           = True

buildENFA w n = buildState n n w w (length w , n) (initENFA (0,0))
  where buildState nv n w wv (i,j) e
          |i /=0 && j==nv && wv /= []                             =trans ((i-1,j),Symbol(last w),(i,j))
                                                                   $(buildState nv n (init w) wv (i-1,j) e )
          |i==0 && j==nv && nv /= 0 && wv/=[]                     =trans ((i,j), Symbol (head wv) , (i+1,j))
                                                                   $ (buildState nv (n-1) wv wv (length wv , n-1 ) e )
          |i==0 && j==0 && nv == n && wv /= []                    =trans ((0,0), Symbol (head wv), (1,0)) .
                                                                   accept (length wv , nv) $ e
          |i==(length wv) && j==n && n /= nv && wv /=[]           =trans ((i,j), Any, (i,j+1)).
                                                                   accept (i,j) $ (buildState nv n wv wv (i-1,j) e )
          |i==0 && j==n && n /= nv && n /= 0 && wv /= []          =trans ((i,j), Any, (i,j+1)). trans((i,j),Symbol (head wv),(i+1,j)).
                                                                   trans((i,j),Eps,(i+1,j+1)).trans((i,j),Any,(i+1,j+1)) $ (buildState nv (n-1) wv wv (length wv, n-1) e )
          |i==0 && j==0 && n /= nv && wv /= []                    =trans ((0,0), Symbol (head wv), (1,0)).trans ((0,0), Any, (0,1)) .
                                                                   trans ((0,0) , Eps , (1,1)) . trans ((0,0), Any, (1,1)).accept (length wv , nv) $ e
          |wv == [] && j/=0                                       =trans ((i,j-1), Any , (i,j)) . accept (i,j) $ (buildState nv n wv wv (i, j-1 ) e )
          |wv == [] && j==0                                       =trans ((i,j), Any , (i,j+1)). accept (i,j) $ e
          |otherwise                                              =trans ((i,j), Symbol (last w),(i+1,j)) .
                                                                   trans ((i,j), Any, (i,j+1)) .
                                                                   trans ((i,j) , Eps , (i+1,j+1)) .
                                                                   trans ((i,j), Any, (i+1,j+1))
                                                                   $ (buildState nv n (init w) wv (i-1,j) e )

jump (i,j) a e
  |Data.Set.member (i,j) (accepting e)   =False
  |Data.Map.member a (delta e ! (i,j))   =True
  |otherwise                             =False
  
evaluateEnfa :: (Num t, Num t1, Ord a, Ord t, Ord t1) =>[a] -> ENFA (t1, t) a -> Set (t1, t)
evaluateEnfa w e = evaluate w e (0,0)

evaluate [] e (i,j)
  |jump (i,j) Eps e         =evaluate [] e (Data.Set.elemAt 0 ((delta e) ! (i,j) ! Eps))
  |otherwise                =Data.Set.singleton (i,j)
evaluate (x:xs) e (i,j)
  |jump (i,j) (Symbol x) e  = evaluate xs e (Data.Set.elemAt 0 ((delta e) ! (i,j) ! (Symbol x )))
  |jump (i,j) Any e         = Data.Set.union (evaluate  xs e (Data.Set.elemAt 0 ((delta e )! (i,j) ! Any))) (evaluate xs e (Data.Set.elemAt (if (Data.Map.size ((delta e) ! (i,j)) > 1) then 1 else 0) ((delta e) ! (i,j) ! Any)))
  |otherwise                = Data.Set.singleton (i,j)
