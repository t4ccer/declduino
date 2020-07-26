module Aggregator where

newtype Aggregator a = Aggregator [a]
    deriving(Show)

instance Semigroup (Aggregator a) where
    (Aggregator xs) <> (Aggregator ys) = Aggregator (xs++ys)

instance Monoid (Aggregator a) where
    mempty = Aggregator []

infixr 0 .>
(.>) :: a -> Aggregator a -> Aggregator a
(.>) a = (<>) (pack a)

pack :: a -> Aggregator a
pack x = Aggregator [x]

unpack :: Aggregator a -> [a]
unpack (Aggregator xs) = xs
