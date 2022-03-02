## The first function creates a cache of the inverted matrix
## The second function reads said cache
## The purpose is to avoid having to compute the inverse every time it is needed

## Creating the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Reading the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


B <- matrix(c(1,2,3,4),2,2)
View(B)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)
