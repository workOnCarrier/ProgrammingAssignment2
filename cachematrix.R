## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of functions.
###     The functions operate on two atomic matrix variables x and inverse
###     X is the user provided matrix and invers is the inverse of the matrix
## The list of functions is
###     get() -- gets the atomic matrix 'x'
###     set(matrix) -- sets the atomic matrix 'x' with the value passed upon invocation and resets 'inverse'
###     setInverse(matrix) -- sets the matrix 'inverse' with the value passed upon invocation
###     getInverse()   -- gives the matrix 'inverse' if already set.. otherwise NULL
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y){
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    setInverse <- function (y) inverse <<- y
    getInverse <- function() inverse
    list ( set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse
           )
}

## Write a short comment describing this function
## this function uses the cached version of the inverse matrix
##  For this purpose,  an instance of the list returned by function makeCacheMatrix is used
## If cached value is non-null, it returns the stored value of 'inverse'
## If the cached value is null, it calculates the inverse matrix, stores in cache and returns the 'inverse'
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if ( !is.null(inverse)){
        message("getting cached data")
         return (inverse)
    }
    data <- x$get()
    inverse <- solve(x$get())
    x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
    inverse
}
