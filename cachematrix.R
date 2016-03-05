## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
