## The following code consists of two functions: makeCacheMatrix and cacheSolve
## The function makeCacheMatrix creates a special matrix object that facilitates the caching
## of a matrix and it's inverse which is computed/cached via cacheSolve. 

## The function makeCacheMatrix creates a "special" matrix which returns a list consisting of four 
## functions: (1) a set function to set the value of the matrix, (2) a get function that returns the value
## of the function, (3) a setinverse function to set the value of the matrix inverse and (4) a
## getinverse function to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The function cacheSolve will return the inverse of the matrix that is passed to it as the argument. It first 
## checks to see if the inverse has been precomputed and cached. In this case, the cached value
## of the inverse is returned. If this value is not available, the inverse is computed using the
## in built solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


