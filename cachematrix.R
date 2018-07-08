## Two functions and defined in this script:
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

## This function creates a list containing different functions. Get and set 
## functions get and set the value of the given matrix, and getinv and setinv 
## functions get and set the value the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}

# The following function calculates the inverse of the list created 
# with the above function. However, it first checks to see if the inverse matrix
# has already been calculated. If so, it gets such matrix from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
