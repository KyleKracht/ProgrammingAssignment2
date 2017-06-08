## these functions create an object that can hold a matrix and it's inverse.
##the second function checks if the inverse has already been found before computing it
## can only handle standard invertible matrices, will not create separate left and right inverses, and does not check for degenerate square matrices

## makeCacheMatrix creates objects that have both the input matrix and can hold an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cache solve checks if the inverse has already been found and stored in cache, and either returns the cache value or computes it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  inverse <- function(data){
    
    solve(data)
  }
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}
