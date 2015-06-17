## makeCacheMatrix() creates "matrix" object and
## returns list of functions to manipulate it.
## 
## cacheSolve() returns the inverse of a matrix.
## It first checks if the inverse of a matrix has already been calculated,
## otherwise it calculates the inverse of a matrix and puts it to the cache
##
## Example:
## a <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## m <- makeCacheMatrix(a)
## cacheSolve(m)


## Creates "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y) {
            x <<- y
            x_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x_inverse <<- inv
    getinverse <- function() x_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates of gets from the cache the inverse of a "matrix" object

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
