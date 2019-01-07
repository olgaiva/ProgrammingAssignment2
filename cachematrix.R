## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## CacheMatrix: an object which consists of a matrix x,
## and its cached inverse i 
## which are accessible through various get and set methods

## makeCacheMatrix initializes a CacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve: Returns inverse of CacheMatrix x
## If x's inverse has not been computed,
## compute and cache it
## Else fetch the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i <- solve(x$get())
    x$setinv(i)
    i
}
