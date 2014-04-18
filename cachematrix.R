## This pair of 

## Write a short comment describing this function

## Simple Matrix for Testing
A <- matrix(c(1,2,0,2,3,0,3,4,1), 3, 3

makeCacheMatrix <- function(x = matrix()) {
  ##Test if the matrix x is invertable
  if (det(x) == 0) {
    print("There is no Inverse for the given matrix!") ##Throw a Warning if det(x) = 0
  }
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
