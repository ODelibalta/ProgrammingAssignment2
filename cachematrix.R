## Put comments here that give an overall description of what your
## functions do
## Assignment for data analysis class 

## Purpose :: Cache the computation of inversing a matrix 


## Function that creates a matrix that contains functionality to 
## set and get values of the matrix object
## set and get values of the inverse of the matrix object
makeCacheMatrix <- function(x = matrix()) {
      im <- NULL                    ## im is InverseMatrix
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) im <<- inverse
      getInverse <- function() im
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}

## This function calculates the inverse of the matrix object
## It will check the cache the first
## Function name is misleading but can't change it due to assignment rules
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'im'
      im <- x$getInverse()  # check the function above for a value
      if(!is.null(im)) {
            message("getting cached data")
            return(im)
      }
      data <- x$get()
      im <- solve(data, ...) ## we have nothing so far so just calculate it with solve()
      x$setInverse(im)  ## set the inverse 
      im
}
