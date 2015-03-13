## cacheSolve works objects created with makeCacheMatrix to 
## store previously computed results of matrix inverses

## makeCacheMatrix sets up a matrix object via a list 
## to store itself and a cache of its inverse
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

## cacheSolve first checks if the matrix's inverse has previously been 
## computed and cached to be returned. Computes and cache the inverse otherwise.
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
