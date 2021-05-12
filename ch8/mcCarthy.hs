mc n =
    case compare n 100 of
      GT -> n - 10
      _ -> mc $ mc (n + 11)

main = do
    print $ map mc [95..110]
