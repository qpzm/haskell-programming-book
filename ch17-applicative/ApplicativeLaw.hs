-- Interchange Law u <*> pure y = pure ($ y) <*> u
-- 는 y가 아직 embed 되지 않은 상태일 때만 성립
-- commutative law와 다름에 주의
-- 아래는 commutative law가 성립하지 않는 예
main = do
    print $ [(+1), (*2)] <*> [1, 2] -- [2,3,2,4]
    print $ [($ 1), ($ 2)] <*> [(+1), (*2)] -- [2,2,3,4]
