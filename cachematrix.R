## `makeCacheMatrix` creates new instances of the special matrix

## `cacheSolve` is a wrapper of the built-in `solve` function


## A constructor for cacheMatrix objects which provides getters & setters for the
## matrix and it's inverse to the outside world

makeCacheMatrix <- function(x = matrix()) {
  ## initially set the inverse to NULL
  i <- NULL
  ## allow updating the matrix by replacing it w/ a new one
  set <- function(y) {
    x <<- y
    ## wipe out any cached inverse since it may be invalid
    i <<- NULL
  }
  get <- function() x
  ## update this matrix's inverse
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## return a list w/ the functions to use a cacheMatrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  Check if the inverse of a matrix has already been calculated before re-computing using the `solve` function

cacheSolve <- function(x, ...) {
  ## Get the matrix's current inverse
  i <- x$getinverse()
  
  ## Cached inverse was not found, need to compute
  if (is.null(i)) {
    message('Computing matrix inverse.')
    ## Fetch the real matrix under the cacheMatrix
    data <- x$get()
    ## Calculate it's inverse and overwrite the initial NULL value
    i <- solve(data, ...)
    ## Cache the inverse
    x$setinverse(i)
  }
  
  ## Return the inverse
  i
}
