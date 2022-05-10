append :: [a] ->[a]->[a]
append [] l2 = l2
append l1 l2 = (head l1) : append(tail l1) l2

rev n = rever [] n
rever acc n =
    if n == []
    then acc
    else rever (head n:acc) (tail n)

main ::IO ()
main = do
    let m = rev[1, 2, 3, 4, 5, 6]
    print(m)