import           Test.Hspec
import           Test.QuickCheck
import           Exercises (BoolAndSomethingElse)

type FId1 f a = f a -> Bool

type FId2 f x a = f x a -> Bool

type FId3 f x y a = f x y a -> Bool

type FId4 f x y z a = f x y z a -> Bool

type FComp1 f a b c = Fun a b -> Fun b c -> f a -> Bool

type FComp2 f x a b c = Fun a b -> Fun b c -> f x a -> Bool

type FComp3 f x y a b c = Fun a b -> Fun b c -> f x y a -> Bool

type FComp4 f x y z a b c = Fun a b -> Fun b c -> f x y z a -> Bool

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

--functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
--functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fn f) (Fn g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main =
  hspec $ do
    describe "BoolAndSomethingElse" $ do
      it "Identity" $ property (functorIdentity :: FId1 BoolAndSomethingElse String)
      it "Compose" $
        property (functorCompose :: FComp1 BoolAndSomethingElse String Float Int)
