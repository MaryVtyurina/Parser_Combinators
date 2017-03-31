import ParserCombinators
import Parser_data_definition
-- import BasicTests

main = do
    s <- readFile "test.txt"
    mapM_ (putStrLn . show) $ parse datadecls s

--TODO
-- изменить parse
