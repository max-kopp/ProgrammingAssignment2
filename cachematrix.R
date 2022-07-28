## These are two functions which help cache the inverse of a matrix 
## so that doesn't have to compute it repeatedly.

## This function creates a special "matrix" object, which is a list, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() m
    
    list(set=set, get=get, setinv=setinv, getinv=getinv) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached inverse:")
        return(inv) # return the inverse if it was already calculated
    }
    data <- x$get
    inv <- solve(data) # we assume that the supplied matrix is always invertible
    x$setinv(inv) # store the inverse in the matrix object
    inv # return the inverse that was just calculated
}
