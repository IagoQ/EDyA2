module ListSeq where

import Seq
import Par


tabulate f n maxn | n == maxn = emptyS
                  | otherwise = let
                                  (x,xs) =  f n ||| tabulate f (n+1)
                                in
                                  x:xs


showt []  = EMPTY
showt [x] = ELT x
showt xs  = NODE (take xs half) (drop xs half)
              where
                half = div (length xs) 2


contractS op (x:y:zs) = let (xy, zs') = op x y ||| contractS op zs
                          in xy:zs'

contract f []       = emptyS
contract f [x]      = [x]
contract f (x:y:xs) = let (z,zs) = f x y ||| contraerList f xs
                          in z:zs

reduce f e []  = e
reduce f e [x] = f e x
reduce f e xs  = reduce f e (contract f xs)


scan _ e []  = (emptyS , e)
scan f e [x] = (singletonS e, f e x)
scan f e xs  = let (ys, r) = scan f e (contract f xs)
                   in (expandir f xs ys, r)
                  where
                    expandir _ [] _ = []
                    expandir _ [_] ys = ys
                    expandir f (x:_:xs) (y:ys) = let (z, zs) = f y x ||| expandir f xs ys
                                                     in y:z:zs



instance Seq [] where
   emptyS         = []
   singletonS  x  = x:emptyS
   lengthS l      = length l
   nthS l n       = l!!n
   tabulateS  f n = tabulate f 0 n
   mapS f l       = map f l
   filterS f l    = filter f l
   appendS l r    = l ++ r
   takeS l n      = take l n
   dropS l n      = drop l n
   showtS xs      = showt xs
   showlS         = foldr CONS NIL
   joinS ls       = concat ls
   reduceS f b l  = reduce f b l
   scanS f b l    = scan f b l
   fromList l    = l


