module ListSeq where

import Seq
import Par


tabulate f n maxn | n == maxn = []
                  | otherwise = let
                                  (x,xs) =  f n ||| tabulate f (n+1) maxn
                                in
                                  x:xs

parallelMap f [] = []
parallelMap f (x:xs) =  let (y, ys) = f x ||| parallelMap f xs
                        in y:ys

parallelFilter f [] = []
parallelFilter f (x:xs) = let (satisfies, ys) = f x ||| parallelFilter f xs
                            in if satisfies then x:ys else ys

append [] ys = ys
append (x:xs) ys = x:(append xs ys)

showt []  = EMPTY
showt [x] = ELT x
showt xs  = NODE l r
              where
                half = div (lengthS xs) 2
                (l,r) = takeS xs half ||| dropS xs half

showl [] = NIL
showl (x:xs) = CONS x xs

contract f []       = []
contract f [x]      = [x]
contract f (x:y:xs) = let (z,zs) = f x y ||| contract f xs
                          in z:zs

reduce f e []  = e
reduce f e [x] = f e x
reduce f e xs  = reduce f e (contract f xs)

expand _ [] _ = []
expand _ [_] ys = ys
expand f (x:_:xs) (y:ys) = let (z, zs) = f y x ||| expand f xs ys
                                 in y:z:zs

scan _ e []  = ([], e)
scan f e [x] = ([e], f e x)
scan f e xs  = let (ys, r) = scan f e (contract f xs)
                   in (expand f xs ys, r)


instance Seq [] where
   emptyS         = []
   singletonS  x  = [x]
   lengthS l      = length l
   nthS l n       = l!!n
   tabulateS  f n = tabulate f 0 n
   mapS f l       = parallelMap f l
   filterS f l    = parallelFilter f l
   appendS l r    = append l r
   takeS l n      = take n l
   dropS l n      = drop n l
   showtS xs      = showt xs
   showlS xs      = showl xs
   joinS ls       = concat ls
   reduceS f b l  = reduce f b l
   scanS f b l    = scan f b l
   fromList l     = l