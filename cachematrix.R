## Caching the Inverse of a Matrix
## By SSantiago 
## 
## The following R functions, makeCacheMatrix and cacheSolve, 
## aim to cache potentially time-consuming computations. 
## This R program will take advantage of the scoping rules 
## of the R language and how they can be manipulated to 
## preserve state inside of an R object. 
## 
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(specialmatrix = matrix()) {
    inverse <- NULL
    set <- function(x) {
        specialmatrix <<- x;
        inverse <<- NULL;
    }
    get <- function() return(specialmatrix);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function retrieves the inverse from the cache.
cacheSolve <- function(specialmatrix, ...) {
    inverse <- specialmatrix$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- specialmatrix$get()
    inverse <- solve(data, ...)
    specialmatrix$setinv(inverse)
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
}
