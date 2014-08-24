## R Programming Coursera Project Assignment 2
## 2014-08-24, Tiago Moreiras
## 
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly.

## Creates a cached matrix object that can cache its inverse
##
## Arguments
## x    input matrix that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Cached Inverted Matrix of given x
    i <- NULL
    
    ## Sets a new source matrix, clearing the inverse
    set <- function(y) {
        # Sets a new value for x
        x <<- y
        # Clears the cached inverse
        i <<- NULL
    }
    
    ## Returns de input matrix
    get <- function() x
    
    ## Sets the inverse matrix of x
    ## inverse  inverse matrix of x
    setInverse <- function(inverse) i <<- inverse
    
    ## Gets the cached inverse matrix of x
    getInverse <- function() i
    
    ## Defines a list with all the function of object X
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
##
## Arguments
## x    cache matrix object
cacheSolve <- function(x, ...) {
    ## Gets the inverse matrix of x
    i <- x$getInverse()
    ## If a cached inverse was already calculated return value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Else, calculate inverse value, save it in the cached
    ## inverse of x and return value.
    data <- x$get()
    i <- solve(x$get(), ...)
    x$setInverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
