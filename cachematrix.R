## The next two functions setup caching inverted matrix
## The 'makeCacheMatrix' function defines how to make a cached matrix
## The 'cacheSolve' function actually checks to see if the inverted matrix has been previously calculated and stored.  If so, it will return the stored inverted matrix.
## If not, it will calculate, store and return the inverted matrix.

## The following function defines how to make a cached matrix.  There are 4 functions with the 'makeCacheMatrix' function.  
## The first being 'set' which sets the value of the matrix.  The second function, 'get', gets the matrix.
## The third function, 'setInverse', set the inverse of the matrix.  The fourth function, 'getInverse', gets the cached inversed matrix.
## The 'list' function just creates a list of functions so they can be called.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
       x <<- y
       m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will check to see if the inverse matrix is cached.  If so, return the cached inverted matrix.
## If not, compute the inverse matrix and store the results.  Next time the cached value is returned instead of recalculating again.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)

    m
}
