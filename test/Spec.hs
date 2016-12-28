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
    sized listGen

listGen :: (Arbitrary a) => Int -> Gen (List a)
listGen size =
  case size of
    0 ->
      pure Nil
    n ->
      oneof [ pure Nil, liftA2 Cons arbitrary (listGen $ n `div` 2)]

main :: IO ()
main = do
  quickBatch $ monoid (undefined :: List String)
  quickBatch $ functor (undefined :: List (Int, String, Char))
  quickBatch $ applicative (undefined :: List (Int, String, Char))
  quickBatch $ monad (undefined :: List (Int, String, Char))

