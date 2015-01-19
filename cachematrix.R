## Pair of functions that are used return inverse of the matrix. We need 2 of them to 
## cache value of this inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
## example:
##
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cashed <- makeCacheMatrix(M)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## returns inverse of the chached matrix
##
## example:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cashed <- makeCacheMatrix(M)
## cacheSolve(cashed)
##
## last line will return inverse of the matrix M. if we call this forst time it will 
## calculate it from scratch. If we call it second and so on time it will return value
## stored in memory

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
