## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly. The inverse of a
## matrix can be cached using the following pair of functions.

## A special matrix object that can cache its inverse is created by this function:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the matrix from makeCacheMatrix.
## The inverse is retrieved from the cache if it has not already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        Mdata <- x$get()
        inv <- solve(Mdata)
        x$setInverse(inv)
        inv
}
