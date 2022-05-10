--Неизменяемые массивы
import Data.Array
import Data.IORef

arr = array (1, 3) [(1, 2), (2, 34), (3, 5.678)] --строгое задание массива

arrNew = arr//[(3, 6)] 

arrNew1 = array (1, 3) [(i, 33 * i) | i <- [1..3]]

elt = (arr ! 3)




main = do
 ref <- newIORef arr
 arrRef <- readIORef ref
 print arr
 print arrRef

 print arrNew

 print arrNew1

 print elt