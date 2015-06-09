
# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(y) {
    x <<- y
    
    # flushing the old value in the cache
    # as new value is assigned to the matrix
    cache <<- NULL
  }
  
  
  # returns the stored matrix
  getMatrix <- function(){
    x
  } 
  
  # cache the given argument
  cacheInverse <- function(inverse){
    cache <<- inverse
  }
  
  # get the cached value
  getInverse <- function(){
    cache
  } 
  
  # returns the list. Each element of the list is a function.
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)

}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse<- y$getinverse()
  
  
  # if a cached value exists return it 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
