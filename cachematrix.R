## Functions checks if matrix inverse has been already calculated, if so it caching its inverse instead of calculating one more time.


## That function stores cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## It calculates the inverse, if that has been already calculated, it caching that inverse results.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

a_matrix <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(a_matrix)
a_matrix$getinverse()
