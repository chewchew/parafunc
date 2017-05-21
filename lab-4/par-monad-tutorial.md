[meatballgraph]: ./dataflowmeatballgraph.png "Data Flow Graph"
## Using the Par Monad for parallel Haskell programming
A great way to write parallel Haskell program is to utilize the Par Monad. This library avoids some problems with lazy evaluations in parallel and is very explicit regarding granularity and data dependencies. If a solution to your problem can be described using a data flow graph, look no further.

### Building parallelism with (kn)IVars and forks
Here is a small example of consuming meatballs in parallel
````haskell
finishPlate :: [Meatballs] -> TastedGood
finishPlate mbs = runPar $ do
  let (leftSplit,rightSplit) = splitAt (length mbs ´div´ 2)
  resultA <- new
  resultB <- new
  fork (put resultA (consume leftSplit))
  fork (put resultB (consume rightSplit))
  aIsOk <- get resultA
  bIsOk <- get resultB
  return (aIsOk and bIsOk)
````
We are spawning to parallel computations to determine wether or not the meatballs tasted good 
and then returning the result using `IVar`'s. 

`IVar`'s can be loosely be descirbed as write-once variables used to communicate
values between parallel computations. In order to get parallel computations, we
need to use a fork. 

Here are some basic functions that can help us with our cutlery:
````haskell
  new :: Par (IVar a)                       -- create a new IVar
  put :: NFDATA a => IVar a -> a -> Par ()  -- put the value in the supplied IVar
  get :: IVar a -> Par a                    -- get the value from an IVar 
  fork :: Par () -> Par ()                  -- fork a computation to happen in parallel
````

Two important things to know about the above functions is that `put` evaluates the
value to normal form and `get` only retrives a value from the IVar if there is
a value to retrive. 

Since `put` evaluates the value to normal form we don't have
to worry about forcing an evaluation to normal form. This is why we need a to be an instance of `NFDATA`. Only evaluating to weak-head normal form is something that can easily prevent speedups when parallelizing in Haskell with all the lazy evaluation that's going on.  

Due to the fact that the `get` function only retrievs a value from an IVar if there is a value to retrive we can get parallelism! We can wait until a value has been put into the IVar from somewhere (maybe from a parallel computation, wink wink). 

A great way to visualize the parallelism, created using the Par Monad, is a data flow graph. Every `fork` yields a new node and `get` connects the parent node with *forked* nodes.

![alt text][meatballgraph]

So this is what the parallelism looks like!

### Show me something real!
Matrix multiplication...

