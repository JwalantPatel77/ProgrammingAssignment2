##
##
## makeCacheMatrix is a function that returns a list of functions
## It stores a matrix and caches the inverse of the matrix
## Functions include: 
## * setMatrix - set the value of a matrix
## * getMatrix - gets the value of the matrix
## * setInverse - set the Inverse of matrix. If value cached then caches the matrix
## * getInverse - get the cached value of matrix
  
makeCacheMatrix <- function(x = matrix()) 
{
  ## holds the cached value or NULL if nothing cached
  cachedMatrix <- NULL

  ## set the value of a matrix  
  setMatrix <- function(newValue)
  {
    x <<- newValue
    cachedMatrix <<- NULL
  }
  ##get the stored matrix
  getMatrix <- function()
  {
    x
  }
  ## set the inverse of matrix
  setInverse <- function(solve)
  {
    cachedMatrix <<- solve
  }
  
  ## get the cached Matrix
  getInverse <- function() cachedMatrix
  
  ##returns a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## the following function returns the inverse of the matrix
## first it checks to see if there is a value that exists, if it does, it returns the cached inverse
## if it does not, it generates a new inverse
## this function assumes the matrix is always invertible
cacheSolve <- function(x, ...) 
{
  ## get the inverse of the matrix
  inv <- x$getInverse()
  ## check to see if this is null
  if (!is.null(inv))
  {
      message("getting cached inverse matrix")
      return(inv)
  }
  ##get the inverse of the matrix as cached data does not exist and set the invers
  data <- x$getMatrix()
  inv <- solve(data)
    x$setInverse(inv)
  ##return the inverse of the matrix
  inv
}
## testing this code
##   x = rbind(c(1,-1/4), c(-1/4,1))
##  m = makeCacheMatrix(x)
##  m$getMatrix()
##  cacheSolve(m)
## cacheSolve(m)
