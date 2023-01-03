module Eval where 
    import Parser (Expr (..))

    type Scope = [(String, Bool)]

    eval :: Expr -> [(String, Bool)] -> Bool
    
