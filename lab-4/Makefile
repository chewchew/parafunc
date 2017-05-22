#File to compile
matprod=MatrixProd
file2Bin=mprod

all: 
	# ghc -o $(file1Bin) -O2 -threaded -rtsopts -eventlog $(file1)
	ghc --make -O2 $(matprod).hs -rtsopts -threaded

#Use 2 cores and lf is for eventlog and sparp sizes
#Also setting GC's allocation area to very large (100mb) 
#to avoid garbage collection
run: 
	./$(matprod) +RTS -s -N2

clean:
	rm -f *.eventlog
	rm -f *.hi
	rm -f *.o