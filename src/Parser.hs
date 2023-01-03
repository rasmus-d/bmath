module Parser ( parse, Expr (..) ) where
    import Lexer ( Token ( .. ), Operator ( .. ) )

    data Expr = Lit String 
        | BinOp Operator Expr Expr
        | Not Expr 
        | Par Expr deriving (Eq, Show)

    precedence :: Operator -> Int
    precedence Or = 0
    precedence Xor = 0
    precedence And = 1

    parse :: [Token] -> Either String Expr
    parse toks = case parse' toks 0 of
        Right ([], e) -> Right e
        Right (left,_) -> Left $ "Parse error: unmatched tokens: " ++ show left
        Left l -> Left l
    parse' :: [Token] -> Int -> Either String ([Token], Expr)
    parse' tokens p = parseAtom tokens >>= uncurry (climb p)  

    climb :: Int -> [Token] -> Expr -> Either String ([Token], Expr)
    climb p (TokOp op : toks) e1 | precedence op >= p 
        = parse' toks (precedence op + 1) >>= (\(t,e) -> climb p t (BinOp op e1 e))
    climb _ tokens e = Right (tokens, e)

    parseAtom :: [Token] -> Either String ([Token], Expr)
    parseAtom (TokNot : toks) = parseAtom toks >>= (\(t,e) -> Right (t, Not e))
    parseAtom (LPar : toks) = case parse' toks 0 of
        Right (RPar : toks', expr) -> Right (toks', expr)
        Right (_, _) -> Left "Parse error: Unmatched parenthesis"
        left -> left
    parseAtom (Sym s : toks) = Right (toks, Lit s)
    parseAtom _ = Left "Parse error: Invalid syntax"