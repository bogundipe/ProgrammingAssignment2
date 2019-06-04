## This functions caches inverse of a matrix

##matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## start the inverse property
  m <- NULL
  
  ##setting the matrix
  set <- function( matrix ) {
    x <<- matrix
    m <<- NULL
  }
  
  ##getting the matrix
  get <- function() {
    ## Return matrix
    m
  }
  
  ##setting inverse of the matrix
  setInverse <- function(inverse) {
    y <<- inverse
  }
  
  ##getting inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    y
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 'cacheSolve' retrieve the inverse of the matrix returned by 'makeCacheMatrix'
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is inverse of 'x'
  m <- x$getInverse()
  
  ## return inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}