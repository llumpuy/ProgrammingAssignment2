## Following the same structucture as the sample for vector means, 
## this assignment includes a "new" function (makeCacheMatrix()) 
## that produces objects of the "class" CacheMatrix, and a 
## function (cacheSolve()) that returns the inverse matrix over 
## an object of  this class -selectively using the cached  data 
## or producing it anew
##
## A simplified use of this class and function could be:
## > a <- matrix(c(1,2,5,-3),c(2,2))
## > superA <- makeCacheMatrix(a)
## > cacheSolve(superA)		## this would calculate a's inverse
## > cacheSolve(superA)		## this would retun the cached inverse val
##
## that is, the second invocation can reuse the stored value
## from the first one, instead of recomputing it
##
## There are no validations over the matrixes set (e.g.
## it could be checked it is an square matrix), in order to 
## leave open the use of the class to other operations



## makeCacheMatrix() is the constructor for the class, which  
## returns a list of 4 functions or "methods" to get 
## or set the values of matrix, and to get or set the values of its 
## inverse
## Two additional methods, setArgs() and getArgs() are used to 
## assure that cached results from inverse can be returned only
## if the additional parameters passed to cacheSolve() are consistent
## with those used calculating the cached value
## Internally the class contains the values of the original matrix 
## "x" used to initialise the instance or set to a different 
## value at a later time using set(), and its inverse "i" if it
## has been already calculated
## Args contains a list of optional arguments passed to Solve()/cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
	args <- list(.noarg=0)
        set <- function(y) { 
                x <<- y 
                i <<- NULL
		args <<- list(.noarg=0) 
        } 
        get <- function() x 
        setInverse <- function(Inverse) i <<- Inverse 
        getInverse <- function() i
	setArgs <- function(Arguments) args <<- Arguments
	getArgs <- function() args
 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse,
	     setArgs = setArgs,
	     getArgs = getArgs) 
}


## cacheSolve() returns the inverse of an object created via 
## makeCacheMatrix()
## In case the inverse has been calculated in a previous use of
## this instance, the cached value is returned. Otherwise the
## solve() R function is used to produce the output, and then
## it is stored for future use (but please see IMPORTANT NOTE
## below)
## At this point I kept the 'cached data' message to clarify 
## when the output is fresh or frozen -remove past review for 
## proper use!
## Any optional arguments that would be relevant to solve()
## are received by cacheSolve and passed along. 
##
## IMPORTANT NOTE: The previous statement entails a major 
## caveat. If the parameters passed to solve() would entail 
## different outcomes, the 'cached' value may not be correct
## if we're not also memorising the param values of the previous
## (first) invocation of cacheSolve()
## The problem was addressd here by:
##  -adding a new variable in makeCacheMatrix() that stores
##   the additional arguments to be passed to solve()
##  - validating within cacheSolve() that the currrent
##    argument list is consistent to that used when the
##    inverse was cached
##  - storing the values used when a fresh calculation was 
##    produced
## 
## I acknowledge this is an extreme approach as it is not sensitive
## to which parameters would/would not make a difference in
## the outcome
## 
## Error management from the cacheSolve() function is inherited 
## from the original solve() function (I just let solve()'s
## errors pop up)

cacheSolve <- function(x, ...) {
        i <- x$getInverse() 
	args <- x$getArgs()
	currArgs <- list(...)

        if(!is.null(i) && (length(setdiff(args,currArgs)) == 0) ) { 
                message("getting cached data")
                return(i) 
        } 
        data <- x$get() 
        i <- solve(data, ...) 
        x$setInverse(i)
	x$setArgs(currArgs)
        i 
}
