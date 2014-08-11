## The following functions implement a matrix capable of cacheing
## its inverse and an interface for calculating/retrieving the
## cached value

## This function implements a matrix with cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
} 


## This function provides an interface for calculating
## inverse for a new matrix and/or retrieving the cached
## result when available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data...")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
