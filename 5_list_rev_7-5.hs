--0305 Пуха | Обращение списков --
-- Конкатенация --
append :: [a] -> [a] -> [a]
append [] l2 = l2
append l1 l2 = (head l1) : append (tail l1) l2

-- Обращение --
rev :: [a] -> [a]
rev [] = []
rev list = append (rev (tail list)) [head list]

main :: IO()
main = do
    -- let l1 = [1, 2, 3] --
    let l1 = ["Day", "Good"]
    -- let l2 = [4, 5, 6] --
    let l2 = ["World", "Hi"]
    putStr("\nl1 = ")
    print(l1)
    putStr("reverse l1 = ")
    print(rev l1)
    putStr("l2 = ")
    print(l2)
    putStr("reverse l2 = ")
    print(rev l2)

    let result1 = rev (append l1 l2)
    putStr("\nrev (append l1 l2) = ")
    print(result1)

    let result2 = append (rev l2) (rev l1)
    putStr("append (rev l2) (rev l1) = ")
    print(result2)

    -- Встроенная конкатенация: ++ --
    -- Встроенное обращение списков: reverse --
    putStr("\nWith built-in functions: ")
    print(reverse (l1 ++ l2))