# Create matrix object to store inverse
makeCacheMatrix <- function(m = matrix()) {
    inv_cache <- NULL   # store the inverse
    # replace the matrix + reset cached inverse
    setMatrix <- function(new_matrix) {
        m <<- new_matrix
        inv_cache <<- NULL
    }
    # return the matrix
    getMatrix <- function() {
        m
    }
    # store the inverse in cache
    cacheInverse <- function(inverse_matrix) {
        inv_cache <<- inverse_matrix
    }
    # return cached inverse
    getCachedInverse <- function() {
        inv_cache
    }
    list(
        set = setMatrix,
        get = getMatrix,
        setInverse = cacheInverse,
        getInverse = getCachedInverse
    )
}


#computes the inverse of the matrix
#returns the cached version instead of recomputing
cacheSolve <- function(mat_obj, ...) {
    cached <- mat_obj$getInverse()
    if (!is.null(cached)) {
        message("Using stored inverse")
        return(cached)
    }
    data <- mat_obj$get()
    inv_result <- solve(data, ...)
    mat_obj$setInverse(inv_result)
    inv_result
}
