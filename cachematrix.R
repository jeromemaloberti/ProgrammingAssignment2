## These functions create and manage a cache for
## a matrix and its inverse.  

## Manage a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(newInverse) inverse <<- newInverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of a matrix if necessary, and update the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
