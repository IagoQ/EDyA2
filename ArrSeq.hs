module ArrSeq where

import qualified Arr as A

import Seq
import Par


append l r = let
                ll = lengthS l
                lr = lengthS r
                -- reescribir linea 
                f n = if n <= ll then nthS l n else nthS r (n - ll)
             in
                tabulateS f (ll + lr)


instance Seq A.Arr where
   emptyS         = A.empty
   singletonS  x  = A.fromList [x]
   lengthS l      = A.length l
   nthS           = (A.!)
   tabulateS      = A.tabulate
   mapS f l       = tabulateS (f . nthS l) (A.length l)
   filterS f l    = A.flatten (mapS (\e -> if f e then singletonS e else emptyS) l)
   appendS l r    = append l r
   takeS l n      = A.subArray 0 n l
   dropS l n      = A.subArray n (lengthS l - n) l
   showtS xs      = showt xs
   showlS         = foldr CONS NIL
   joinS ls       = concat ls
   reduceS f b l  = reduce f b l
   scanS f b l    = scan f b l
   fromList l    = l


