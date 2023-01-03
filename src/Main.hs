module Main (main) where
    import Lexer (scan)
    import Parser (parse)
    main :: IO ()
    main = do
        line <- getLine
        let ast = scan line >>= parse
        case ast of 
            Right a -> print a
            Left err -> print err
        main
