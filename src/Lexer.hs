module Lexer( scan, Token (..), Operator (..) ) where
    import Data.Char ( isAlpha )
    data Operator = And | Or | Xor deriving (Eq,Show)
    data Token = TokOp Operator | TokNot | Sym String | LPar | RPar deriving (Eq,Show)
    
    scan :: String -> Either String [Token]
    scan str = scan' str [] (1,1)
    scan' :: String -> [Token] -> (Int, Int) -> Either String [Token]
    scan' [] toks _ = Right $ reverse toks
    scan' (' ' : str) toks (l,c) = scan' str toks (l,c+1)
    scan' ('\n' : str) toks (l,_) = scan' str toks (l+1,0)
    scan' ('&' : str) toks (l,c) = scan' str (TokOp And : toks) (l,c+1)
    scan' ('|' : str) toks (l,c) = scan' str (TokOp Or : toks) (l,c+1)
    scan' ('^' : str) toks (l,c) = scan' str (TokOp Xor : toks) (l,c+1)
    scan' ('!' : str) toks (l,c) = scan' str (TokNot : toks) (l,c+1)
    scan' ('(' : str) toks (l,c) = scan' str (LPar : toks) (l,c+1)
    scan' (')' : str) toks (l,c) = scan' str (RPar : toks) (l,c+1)
    scan' str toks (line,col) = case span isAlpha str of 
        ([], c : _) -> Left $ "Error lexing: Unrecognized char " ++ 
            [c] ++ " at line " ++ show line ++ ", column " ++ show col
        (sym, rest) -> scan' rest (Sym sym : toks) (line,col + length sym)
        