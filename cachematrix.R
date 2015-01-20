## Provides a matrix wrapper (makeCacheMatrix) that allows the caching
## of the matrix inverse for repeated use (cacheSolve)

## Provides gets/sets for a matrix object and its inverse, ensuring
## the inverse gets cleared if the original matrix is overwritten

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}      


## checks for a cached inverse of the matrix.  If this doesn't exist,
## compute it, save it for later, and return it

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    ## Save the data away so we can have it for later
    x$setinv(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
