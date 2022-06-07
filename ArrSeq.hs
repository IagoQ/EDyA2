module ArrSeq where

import qualified Arr as A

import Seq
import Par


append l r = tabulateS f (ll + lr) where 
               ll = lengthS l
               lr = lengthS r
               f n | n < ll  = nthS l n
                   |otherwise = nthS r (n - ll)

showt l | lengthS l == 0 = EMPTY
        | lengthS l == 1 = ELT (nthS l 0)
        | otherwise =  NODE l r
            where
               (l,r) = (takeS l half) ||| (dropS l half)
               half = div (lengthS l) 2

showl l | lengthS l == 0 = NIL
        | otherwise = CONS x xs
            where
               (x,xs) = (nthS l 0) ||| (dropS l 1)

contract f l | lengthS l == 0 = emptyS
             | lengthS l == 1 = l
             | otherwise = appendS z zs 
                  where
                     (z,zs) = singletonS(f (nthS l 0) (nthS l 1) ) ||| contract f (dropS l 2)

reduce f e l | lengthS l == 0 = e
             | lengthS l == 1 = f e (nthS l 0)
             | otherwise = reduce f e (contract f l)

scan f e l | lengthS l == 0 = (emptyS, e)
           | lengthS l == 1 = (singletonS e, f e (nthS l 0))
           | otherwise = let (ys, r) = scan f e (contract f l)
                   in (expandir f l ys, r)
                  where
                    expandir op l ys | lengthS l == 0 = emptyS
                                     | lengthS l == 1 = ys
                                     | otherwise = appendS (appendS (singletonS(nthS ys 0)) z) zs 
                                        where
                                          (z, zs) = singletonS(op (nthS ys 0) (nthS l 0)) ||| expandir op (dropS l 2) (dropS ys 1)


instance Seq A.Arr where
   emptyS         = A.empty
   singletonS x   = A.fromList [x]
   lengthS l      = A.length l
   nthS           = (A.!)
   tabulateS      = A.tabulate
   mapS f l       = tabulateS (f . nthS l) (A.length l)
   filterS f l    = A.flatten (mapS (\e -> if f e then singletonS e else emptyS) l)
   appendS l r    = append l r
   takeS l n      = A.subArray 0 n l
   dropS l n      = A.subArray n (lengthS l - n) l
   showtS xs      = showt xs
   showlS xs      = showl xs
   joinS ls       = A.flatten ls
   reduceS f b l  = reduce f b l
   scanS f b l    = scan f b l
   fromList l     = A.fromList l


