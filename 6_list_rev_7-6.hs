rev :: [a]  -> [a]
rev [] = []
rev (lsthead : lsttail) = rev lsttail ++ [lsthead]


main = do 
    let m = rev [1, 2, 3, 4, 5, 6]
    print(m)

    print([1] ++ [])
    
    print(rev ([1, 2, 3, 4, 5, 6] ++ []))
    print(rev ([1, 2, 3] ++ [4, 5, 6]))
    print(rev (rev [1, 2, 3, 4, 5, 6]))