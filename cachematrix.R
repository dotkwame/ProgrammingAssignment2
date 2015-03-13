## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setmatrix_inv <- function(solvex) mi <<- solvex
        getmatrix_inv <- function() mi
        list(set = set, get = get,
             setmatrix_inv = setmatrix_inv,
             getmatrix_inv = getmatrix_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getmatrix_inv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setmatrix_inv(mi)
        mi
}
