## The functions in this document calculate and return the inverse of a matrix. The inverse is then stored 
## in the cache. If the cacheSolve function is called with the same argument, the inverse is returned from the cache.

## makeCacheMatrix creates a list containing 4 functions: set, get, setinverse & getinverse. 
## These are used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the list created in makeCacheMatrix. If the inverse has already been
## calculated for the list, the inverse is returned from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {                     ## Check if the inverse has already been calculated
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)                 ## Return a matrix that is the inverse of 'x'
        x$setinverse(inv)
        inv
}
