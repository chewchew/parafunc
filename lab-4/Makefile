#File to compile
matprod=MatrixProd
qsort=QuickSort

all: 
	ghc --make -O2 $(qsort).hs -rtsopts -threaded
	ghc --make -O2 $(matprod).hs -rtsopts -threaded

#Use 2 cores and lf is for eventlog and sparp sizes
#Also setting GC's allocation area to very large (100mb) 
#to avoid garbage collection
run: 
	./$(qsort) +RTS -s -N4
	./$(matprod) +RTS -s -N4

clean:
	rm -f *.eventlog
	rm -f *.hi
	rm -f *.o
