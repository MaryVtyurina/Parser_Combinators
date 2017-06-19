import ParserCombinators
import Parser_data_definition

main = do
    s <- readFile "test.txt"
    print $ parse datadecls s
