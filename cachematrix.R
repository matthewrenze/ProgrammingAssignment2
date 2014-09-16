## Cached Matrix Inverter
## Represents a set of functions used to compute and cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        return(x)
    }
    
    setinverse<- function(inverse) {
        i <<- inverse
    }
    
    getinverse <- function() {
        return(i)
    }
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## using a cached value of the inverted matrix if the matrix has not changed

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("Getting cached data.")
        return(i)
    }
        
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
}
