## This pair of functions could be used to calculate and store the inverse of a matrix 
## or recall the previous saved inverse of the same matrix. Also a simple Testing matrix "A" is provided. 


## Simple Matrix for Testing
A <- matrix(c(1,2,0,2,3,0,3,4,1), 3, 3)

## This function first checks the determinant and prints are warning if a matrix with det = 0 is provided.
## Then the matrix is stored in a list, containing the matrix and if already calculated the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  ##Test if the matrix x is invertable
  if (det(x) == 0) {
    message("There is no Inverse for the given matrix!") ##Throw a Warning if det(x) = 0
  }
  inv <- NULL # variable for stored inverse, empty at the first call of makeCacheMatrix()
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse #functions that stores the inverse, when called from cacheSolve()
  getInverse <- function() inv                    #return the stored inverse
  #creates a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This functions first either returns a stored inverse or calculates the inverse of the given matrix 
## and then stores it using the subfunctions of makeCacheMatrix. Then the cached or calculated Inverse is returned.

cacheSolve <- function(x, ...) {
        ## get cached data from List and check if Null
        ## if not Null return cached data, if null calculate inverse an return
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
  }
}
