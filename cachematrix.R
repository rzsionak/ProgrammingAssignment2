## These functions cache an inverse of a matrix and calculate the inverse

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){ ##check for cached inverse matrix
        message("getting cached matrix")
        return(m)
    }
    data <- x$get() 
    m <- solve(data, ...) ##solve for inverse matrix if not cached
    x$setInverse(m) ##set inverse matrix in cache
    m
}
