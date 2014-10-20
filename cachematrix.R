##The makeCacheMatrix function is used to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  
  ## the set funtion uses free variable to cache the matrix that is passed in
  set <- function(y = numeric()) {
    x <<- y
    inv <<- NULL  
  }
  
  ## the get function get the value of the cached matrix
  get <- function() x
  
  ## the set function caches the inverse of the matrix 
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- 1/data
  x$setinverse(inv)
  inv
}
