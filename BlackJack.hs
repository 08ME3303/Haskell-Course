module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random hiding (shuffle)

main :: IO ()
main = runGame implementation


{-         PART A            -}
-- 3.1
{- size hand2 will return two as the steps are:
   size hand2 = 1 + size (Add (card Jack Spades) Empty)
              = 1 + 1 + size Empty
              = 1 + 1 + 0 = 2
-}
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)
hand3 = Add (Card King Spades) (Add (Card (Numeric 5) Clubs) (Add (Card Ace Spades) (Add (Card Ace Hearts) Empty)))

-- | Returns an empty hand
empty :: Hand
empty = Empty

-- | Returns the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric c) = c
valueRank Ace = 11
valueRank _ = 10

-- | returns the value of the card
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- | Recursively calculates the numbers of Aces in an hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                       = 0
numberOfAces (Add c h) | (rank c) == Ace = 1 + numberOfAces h
                       | otherwise       = numberOfAces h

-- | Additional function that calculates the value an hand without
--   taking care if the value exceeds 21
valueHand :: Hand -> Integer
valueHand Empty     = 0
valueHand (Add c h) = valueCard c + valueHand h

-- | returns the value of the hand, reducing the total value by 10*numberOfAces
--   if it exceeds 21
value :: Hand -> Integer
value h | valueHand h > 21 = valueHand h - (10*numberOfAces h)
        | otherwise        = valueHand h

-- | simple function with a boolean expr
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- | function that determines who's the winner given the player and bank Hand

winner :: Hand -> Hand -> Player
winner h1 h2 | gameOver h1 || (value h2 >= value h1) && (not (gameOver h2)) = Bank
             | otherwise = Guest

-- | Given two hands as arguments, return a new one with the first on the top
--   of the other
(<+) :: Hand -> Hand -> Hand
Empty <+ h2 = h2
(Add card h1) <+ h2 = (Add card (h1 <+ h2)) --recursive case

-- | Checking the associative property of <+
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Checking the size of combined hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1<+h2) == (size h1 + size h2)

-- | Returns all cards of a given suit s
handSuit :: Suit -> Hand
handSuit s = (Add (Card Ace s) (Add (Card King s) (Add (Card Queen s)
             (Add (Card Jack s) (Add (Card (Numeric 10) s)
             (Add (Card (Numeric 9) s) (Add (Card (Numeric 8) s)
             (Add (Card (Numeric 7) s) (Add (Card (Numeric 6) s)
             (Add (Card (Numeric 5) s) (Add (Card (Numeric 4) s)
             (Add (Card (Numeric 3) s) (Add (Card (Numeric 2) s)  Empty)))))))))))))

-- | Returns a deck with all the cards
fullDeck :: Hand
fullDeck = (handSuit Hearts) <+ (handSuit Spades) <+ (handSuit Diamonds) <+ (handSuit Clubs)

-- | Draws a card from a deck and puts it in a hand           
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h1 = error "draw:The deck is empty."
draw (Add c1 d) h1 = (d,(Add c1 h1))       

-- | Creates an empty hand to start with
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- | Draws a card from deck to bank's hand
playBank':: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand <=16 = playBank' deck' bankHand'
                        | otherwise           = bankHand
                        where
                            (deck',bankHand') = draw deck bankHand


-- | Checks if a card is in the deck
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- | Returns a card from the deck and the remaining deck
pickCard :: Integer -> Hand -> (Card, Hand)
pickCard _ Empty = error "Empty Hand"
pickCard 1 (Add c d) = (c, d)
pickCard n (Add c d) = (c',Add c d')
    where
        (c', d') = pickCard (n-1) d

-- | Returns a Shuffled deck
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g deck  = Add newCard (shuffle g revDeck)
                      where
                          (n,g1) = randomR (1,size deck) g
                          (newCard,revDeck) = pickCard n deck

-- | Checking the size of the shuffled deck and the presence of all cards
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)
                      

implementation = Interface
    { iEmpty = empty
     ,iFullDeck = fullDeck
     , iValue   = value
     , iGameOver= gameOver
     , iWinner  = winner
     , iDraw    = draw
     , iPlayBank= playBank
     , iShuffle = shuffle
    }  