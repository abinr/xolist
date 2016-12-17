import XOList
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Eq a) => EqProp (List a) where
  (=-=) =
    eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    listGen

{-- Perhaps naive, risks to never terminate --}
listGen :: (Arbitrary a) => Gen (List a)
listGen =
  oneof [return Nil, liftA2 Cons arbitrary listGen]

main :: IO ()
main = do
  quickBatch $ monoid (undefined :: List String)
  quickBatch $ functor (undefined :: List (Int, String, Char))
  quickBatch $ applicative (undefined :: List (Int, String, Char))

