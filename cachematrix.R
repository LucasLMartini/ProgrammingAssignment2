## Put comments here that give an overall description of what your
## functions do

## This fuction generates a special type of matrix object that can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(sol) s <<- sol
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function returns the cached inverse of the matrix if it exists,
## otherwise it calculates it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data,...)
    x$setsolve(s)
    s
}
