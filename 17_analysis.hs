matches :: String -> (Char -> Bool) -> Bool
matches [] func = True
matches str func 
    | func (head str) == True = matches (tail str) func
    | otherwise = False

lexer :: String -> [String]
lexer "" = []
lexer str = [token] ++ lexer (snd res)
    where
        res = head (lex str)
        
        token
            | matches (fst res) (\ch -> ch >= '0' && ch <= '9') = "(Number) " ++ (fst res)
            | matches (fst res) (\ch -> ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '=') = "(Operator) " ++ (fst res)
            | matches (fst res) (\ch -> ch == '(' || ch == ')') = "(Bracket) " ++ (fst res)
            | otherwise = "(Name) " ++ fst res


main = print(lexer "x + 5 = 786")