-- 17.5 Exercise: Fixer Upper
main = do
    const <$> Just "Hello" <*> Just "World"
    (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
