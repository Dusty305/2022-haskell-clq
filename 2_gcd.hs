mygcd 0 a = a
mygcd b 0 = b
mygcd a b = mygcd (mod (max a b) (min a b)) (min a b)