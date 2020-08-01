## The functions makeCacheMatrix and cacheSolve together helps in caching 
## the inverse of a matrix rather than computing it repeatedly. 

## The makeCacheMatrix function creates a special matrix R object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_mat <- function(new_mat) {
        x <<- new_mat
        inv <<- NULL
    }
    get_mat <- function() x
    set_inv <- function(inv_calc) inv <<- inv_calc
    get_inv <- function() inv
    
    list(get_mat = get_mat, set_mat = set_mat, get_inv = get_inv, set_inv = set_inv)
}

## The cacheSolve function below computes inverse of the matrix returned by the  makeCacheMatrix function above. If the inverse is already calculated, cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_local <- x$get_inv()
    if (!is.null(inv_local)) {
        message("Retrieving cached inverse:")
        return(inv_local)
    }
    mat_local <- x$get_mat()
    inv_local <- solve(mat_local)
    x$set_inv(inv_local)
    inv_local
}