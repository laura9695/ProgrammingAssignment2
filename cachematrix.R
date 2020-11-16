## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {         ## define the functions args
    m <- NULL                                       ## initializing
    set <- function(y) {                            ## set the value of the matrix
        x <<- y                                     ## assign value to object in parent environment 
        m <<- NULL                                  ## reset to NULL in parent environment
    }
    get <- function() x                             ## get the value of the matrix
    setinverse <- function(inverse) m <<- inverse   ## set the value of the inverse
    getinverse <- function() m                      ## get the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()                     ## get the value of the inverse
    if(!is.null(m)) {                       ## If the inverse has already been calculated
        message("getting cached data")      ## then retrieve the inverse from the cache
        return(m)                          
    }
    data <- x$get()                         ## otherwise compute the value of the inverse
    m <- solve(data, ...)                   ## solve() returns the inverse of the matrix
    x$setinverse(m)                         ## set the value of the inverse
    m
}
