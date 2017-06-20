main : main.hs
	ghc main.hs

test : BasicTests
	./BasicTests

BasicTests: BasicTests.hs
	ghc BasicTests.hs

check : main
	./main
