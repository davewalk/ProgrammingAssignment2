## This file includes functions for creating a CacheMatrix object and
## calculating the inverse of a CacheMatrix's matrix.

## makeCacheMatrix creates a matrix-like object that can cache its inverse.
## Arguments:
#    - x: A matrix to set as the matrix for the object upon creation. Optional.
## Returns: A CacheMatrix object with the following public functions:
##     set() - a function for setting the matrix for the object.
##     get() - a function for getting the previously set matrix.
##     getInverse() - a function for getting the previously set inverse
##                    of the matrix.
##     setInverse() - a function for setting the inverse of the previously
##                    set matrix.
makeCacheMatrix <- function(x = matrix()) {
        
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    getInverse <- function() i
    
    setInverse <- function(inverse) i <<- inverse
    
    list(set = set, get = get, getInverse = getInverse,
         setInverse = setInverse)
}

## cacheSolve computes the inverse of the matrix of a CacheMatrix object.
## Arguments:
##    - x: A CacheMatrix object.
## Returns:
##    - The inverse of the CacheMatrix object's matrix.
##      If the object's matrix is NULL, this function returns NULL.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        return(i)
    }
    
    m <- x$get()
    if (all(is.na(m))) {
        i
    } else {
        i <- solve(m)
        x$setInverse(i)
        i    
    }
}

# Usage:
#   test <- outer(1:4, 1:4, pmin.int)
#   x <- makeCacheMatrix()
#   x$set(test)
#   i <- cacheSolve(x)