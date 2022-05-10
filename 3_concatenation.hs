append :: [Int] -> [Int] -> [Int]
append [] l2 = l2
append (h:t) l2 = h : append t l2



main::IO()
main = do
    print(append [] [])
    print(append [1] [])
    print(append [] [1, 2])
    print(append [3, 2, 5] [6, 3, 1])