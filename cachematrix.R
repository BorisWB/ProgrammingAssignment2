## These functions calculate the inverse matrix of the input matrix and
## cache the inverse matrix. If the function is called on the same matrix
## the cached matrix is returned to save computational time.

## Create a 'special matrix' that can store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  mat = NULL
  set <- function(y) {
    x <<- matrix(y)
    mat = NULL
  }
  get <- function() x
  setinv <- function(y) mat <<- y
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks whether there is a stored inverse matrix of the data, if so this
## is returned, otherwise the inverse matrix is calculated and returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("getting cached matrix")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data)
  x$setinv(mat)
  mat
}
