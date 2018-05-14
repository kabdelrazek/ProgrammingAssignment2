## Functions makeCacheMatrix and cacheSolve that cache the inverse of a matrix

## makeCacheMatrix: creates a matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matIinv <<- NULL
  }
  get <- function() x
  setMatInv <- function(inverse) matInv <<- inverse
  getMatInv <- function() matInv
  list(set = set, get = get, setMatInv = setMatInv, getMatInv = getMatInv)
}


## cacheSolve: computes inverse of matrix returned by makeCacheMatrix. If inverse previously calculated it retrieves it from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv <- x$getMatInv()
  if(!is.null(matInv)) {
    message("loading cached result")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setMatInv(matInv)
  matInv
}
