--Изменяемые массивы

-- newArray инициализация нового массива
-- readArray чтение массива
-- writeArray запись в существующий массив

import Data.Array.IO

main = do
 arr <- newArray (1, 3) 0 :: IO (IOArray Int Int)
 writeArray arr 1 101
 writeArray arr 2 111
 writeArray arr 3 110
 elt <- readArray arr 2
 print elt