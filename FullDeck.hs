module FullDeck where
import Cards

{-fulldeck = Add fst(c) fulldeck
             where
               h = Empty
               c = genCard-}


genCard = [Card r s | r <- [Jack, Queen, King, Ace], s <- [Hearts, Diamonds]]