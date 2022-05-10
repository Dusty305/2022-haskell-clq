--Ссылки на объекты

import Data.IORef

-- newIORef инициализация новой ссылки
-- readIORef чтение ссылки
-- writeIORef переопределение ссылки на другой объект

main = do
 ref <- newIORef "Helloworld" -- теперь ref - ссылка на объект строку
 str <- readIORef ref
 print str 
 writeIORef ref "New String"
 str2 <- readIORef ref
 print str2