## The overall purpose of these two function is to make sure that the sometime  
## Time-consuming computation of finding the inverse of a matrix only is done once, and then saves in memory

## Below function creates a list of functions that can: set the value of the matrix, get the value of the matrix,
## set the reverse of a matrix, get the reverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  set_rev_matrix <- function(solve) rev <<- solve
  get_rev_matrix <- function() rev
  list(set = set, 
       get = get,
       set_rev_matrix = set_rev_matrix,
       get_rev_matrix = get_rev_matrix)
}


## The below function calculates the the inverse of the matrix defined in the makeCacheMatrix function 
## and stores the inverse via the set_rev_matrix if not the inverse already has been calculated. If the 
## Inverse of the Matrix already has been calculated it gets the mean from the Cache instead of calculating it

cacheSolve <- function(x, ...) {
  rev <- x$get_rev_matrix()
  if(!is.null(rev)) {
    message("getting cached data")
    return(rev)
  }
  data <- x$get()
  rev <- solve(data, ...)
  x$set_rev_matrix(rev)
  rev
}
