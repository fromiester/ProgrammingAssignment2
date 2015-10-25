## These functions will cache the inverse of  a matrix
## and then when needing to create the inverse, it will
## check to see if it has the inverse cahced, if so it will
## return the cached inverse matrix
## If it is not cahced, it will then compute the inverse


## makeCacheMatrix function will create an inverse of the matrix and then store it in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function will check if the inverse of tmatrix is cahced, if not it will solve for it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
