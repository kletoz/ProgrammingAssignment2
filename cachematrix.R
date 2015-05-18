## Create a cache for the inverse of matrix. It avoids to compute the inverse
## of a matrix each time it is needed. The value is cached on the first call and
## could be used further, since the matrix not change.

## Create an matrix-like object that will cache the inverse of underlying
## matrix. You must call this function before use cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL        
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the cached inverse of a matrix-like cachedMatrix object. The inverse
## will be computed only in the first call.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
