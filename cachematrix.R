## These two functions can cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function returns a list with the next functions:
## set the value of the matrix
## get the value of the matrix
## set the value of solve
## get the value of solve

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve is a function that computes the inverse of the "special matrix" 
## returned by makeCacheMatrix function. If the inverse has already been 
## calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
