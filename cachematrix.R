## These functions create a system that either calculates matrix inversions or
## retrieves previously calculated matrix inversions from its cache, which
## can save time by preventing repeat calculations.

## This function creates a special matrix that contains the function to set the
## value of the vector, get the value of the vector, set the value of the matrix
## inversion, or get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function will calculate the inversion of the matrix created in
## makeCacheMatrix if not already set, and if it is, it will retrieve the
## calculaed inversion from its cache

cacheSolve <- function(x, ...) {
    library(matlib)
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- inv(data, ...)
    x$setinv(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
