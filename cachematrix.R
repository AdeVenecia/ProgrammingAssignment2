## Functions that cache and solves the inverse of a matrix

## makeCacheMatrix is for caching a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inv_matrix <- function(inverse) inv_matrix <<- inverse
  get_inv_matrix <- function() inv_matrix
  list(set=set, get=get, set_inv_matrix=set_inv_matrix, get_inv_matrix=get_inv_matrix)
}


## Checks if inverse is already existing. Function computes and returns the inverse of the matrix
## if no cached result for it exists. Else it gets the result from cache if it is already existing and returns it.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inv_matrix()
    if(!is.null(inv_matrix)) {
    message("displaying from cached data...")
    return(inv_matrix)
  }
  orig_matrix <- x$get()
  inv_matrix <- solve(orig_matrix)
  x$set_inv_matrix(inv_matrix)
  inv_matrix
}
