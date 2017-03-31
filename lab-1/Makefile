#File to compile
file=given.hs

all: 
	ghc -O2 -threaded -rtsopts -eventlog $(file)

#Use 2 cores and lf is for eventlog and sparp sizes
#Also setting GC's allocation area to very large (100mb) 
#to avoid garbage collection
run: 
	./given +RTS -N4 -lfs -A100M

clean:
	rm given.eventlog
