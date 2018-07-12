## The functions below computes the inverse of the special "matrix" created by makeCacheMatrix below. 
##If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(matrix) {
    g <<- k
    inverse <<- NULL
  }

  get <- function() 
  setInverse <- function(inverse) 
  inverse <<- inverse
  getinverse <- function()
  inverse <<- NULL
  list(setmatrix = setmatrix, get = get, setInverse = setInverse, getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
