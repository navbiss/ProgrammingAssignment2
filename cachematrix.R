#Assignment: Caching the Inverse of a Matrix


# Part 1 makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function() {x}
  setinverse <- function (inverse){inv <<- inverse}
  getinverse <- function () {inv}
  list (set = get, get = get, setinverse = setinverse, getinverse=getinverse)
  
}

# Part 2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cachesolve <- function(x,  ...) {
  inv <- x$getinverse()
  if (!isnull(inv)){
    message ("Retrieving Cached Data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
  
}