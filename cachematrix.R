## the function is create to cache the inverse of a matrix
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function( matrix ) {
    x <<- matrix
    m <<- NULL
    
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
