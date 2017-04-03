#File to compile
file1=given.hs
file2=msort.hs

all: 
	ghc -O2 -threaded -rtsopts -eventlog $(file1)
	ghc -O2 -threaded -rtsopts -eventlog $(file2)

#Use 2 cores and lf is for eventlog and sparp sizes
#Also setting GC's allocation area to very large (100mb) 
#to avoid garbage collection
run: 
	# ./given +RTS -N4 -lfs -A1000M
	./msort +RTS -N4 -lfs -A500M

clean:
	rm -f given
	rm -f msort
	rm -f *.eventlog
	rm -f *.hi
	rm -f *.o