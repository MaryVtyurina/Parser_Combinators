main : main.hs
	ghc main.hs
	ghc BasicTests.hs

test : BasicTests.hs
	./BasicTests

check : main.hs
	  ./main
