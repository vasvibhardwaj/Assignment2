## Overall, the first function, makeCacheMatrix, that can store a special matrix and cache its inverse.
## The second function, cacheSolve either computes the matrix, caches it and returns it or if it has already been calculated, retrieves it from the cache. 
## Together, they improve efficiency and reduce the amount a person has to code to find the inverse of a matrix, especially large matrices.

## This defines a function makeCacheMatrix that creates a list to store a matrix and cache its inverse. It makes a "special" matrix that has a function to set and get the value of a matrix and set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
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


##This defines the function cacheSolve which computes the inverse of the matrix. If the inverse has already been calculated and stores, it retrieves the inverse from the cache instead of recalculating it.

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
