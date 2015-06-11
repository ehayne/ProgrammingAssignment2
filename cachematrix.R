## Together, these functions calculate the inverse of a matrix and cache it.
## Before calculation of the inverse, it will check the cache to see if
## the inverse has already been computed and just use that value instead if 
## it finds it.

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set_cache <- function(y) {
      x <<- y
      m <<- NULL
    }
    get_cache <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set_cache = set_cache,
         get_cache = get_cache,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x) {
    cached_matrix <- x$get_inverse()
    if(!is.null(cached_matrix)) {
      message("getting cached data")
      return(cached_matrix)
    }
    data <- x$get_cache()
    inverse_matrix <- solve(data)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
