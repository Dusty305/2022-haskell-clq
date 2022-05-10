
append :: [a] -> [a] -> [a]
append [] x = x
append x y = (head x) : append (tail x) y

main :: IO ()
main = do
let l1 = [1, 2, 3]
let l2 = []

print(l1 ++ l2)  -- [1, 2, 3] + [] --

let hl = (head l1) : l2  -- 1 + [] --
let tl = tail l1 ++ l2  --  [2, 3] + [] --
putStr "head l1 ++ tail l1 ++ [] = "
print (hl ++ tl)  -- [1] + [2, 3] --

putStr "append l1 and [] = "
print(append l1 l2)
print(append hl tl)