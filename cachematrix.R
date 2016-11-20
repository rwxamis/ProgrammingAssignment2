## The following functions cashe the inverse of a matrix

## This function creates a special "matrix" object that:
### 1) set the value of the matrix 
### 2) get the value of the matrix
### 3) set the value of the inversed matrix &
### 4) get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_mtx <- NULL
    set_mtx <- function (y) {
        x <<- y
        inv_mtx <<- NULL
    }     
    get_mtx <- function() x
    set_inv <- function(inv) inv_mtx <<- inv
    get_inv <- function() inv_mtx
    list(set = set_mtx,  get = get_mtx,  setInversed = set_inv,  getInversed = get_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse 
## from the cache. Otherwise, it calculates the inverse of the matrix & sets the value of the 
## inversed matrix in the cache via the set_inv function
cacheSolve <- function(x, ...) {
    inv_mtx <- x$get_inv()
    if(!is.null(inv_mtx)) {
        message("Getting Inversed Matrix from cache")
        return(inv_mtx)
    }
    mtx <- x$get_mtx
    inv_mtx <- solve(mtx)
    x$set_inv(inv_mtx)
    inv_mtx
}
