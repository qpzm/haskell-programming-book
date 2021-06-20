module Main where -- 여기는 moudle 이름이 Main이어야 cabal run monoid 로 실행 가능

import Test.QuickCheck

-- Monoid
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

type S = String
type B = Bool

main :: IO ()
main = do
    print "Check monoidAssoc"
    quickCheck (monoidAssoc :: S -> S -> S -> B)
    quickCheck (monoidLeftIdentity :: S -> B)
    quickCheck (monoidRightIdentity :: S -> B)
    -- verboseCheck (monoidAssoc :: S -> S -> S -> B)
    -- 아래처럼 어떤 인자가 들어왔는지 표시
    --Passed:
    --"\v.*\FSJ\v\ETX"
    --"\fC\SUB\ACK{C(a\1095968\&5\49407\SYN.\132039+\1003278q]s"
    --"\183537\995864~3\45834%M\1100928\100080z\1038853H3).V\101087?n7\1014289"
