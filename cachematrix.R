## Sometimes computing the inverse of a matrix takes a long time.  If the contents of the matrix does not change, it makes sense to cache
## the inverse matrix so that wen we need it again.  We look up the cached value rather than recompute.  These functions setup the caching
## of the inverse matrix.

## The following function defines how to make a cached matrix.  There are 4 functions with the 'makeCacheMatrix' function.  
## The first being 'set' which sets the value of the matrix.  Teh second function, 'get', gets the matrix.
## The third function, 'setInverse', set the inverse of the matrix.  The fourth function, 'getInverse', gets the cached inverse matrix.
## The last 'list' function just creates a list of functions so they can be called.

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
## If not, compute the inverse and store the results for next time.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(! is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- sort(data)
    x$setInverse(m)

    m
}
