degree x n 
  | n == 0 = 1
  | n > 0 = x * degree x (n - 1) 
  | n < 0 = 1 / x * degree x (n + 1)