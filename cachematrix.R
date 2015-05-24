makeCacheMatrix <- function(x = matrix()) #x is a matrix 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  #cache the inverse matrix
  getinverse <- function() m  #get cached inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   #4 defined functions
}


cacheSolve <- function(x, ...) {  
  m <- x$getinverse()  #get inverse from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   #return m if available in cache
  }
  data <- x$get()
  m <- solve(data, ...)   #solve for x not available in cache
  x$setinverse(m)   #cache inverse
  m
}