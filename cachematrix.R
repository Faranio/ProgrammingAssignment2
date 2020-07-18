## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of some matrix x
## returned by the makeCacheMatrix function above.
## If the inverse is already calculated, then the cacheSolve
## function should get this inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if (!is.null(i)) {
            message("Getting cached data")
            return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}