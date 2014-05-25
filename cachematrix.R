

#This function creates a matrix which caches its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
  
    I <- NULL
    set <- function(y) {
      x <<- y
      I <<- NULL
    }
    
    get <- function() x
    setInverse <- function(Inverse) I <<- Inverse
    getInverse <- function() I
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  
  




## This function retireves the inverse of the matrix from the cache, if present in the cache
##If it is not present in the cache, the inverse is calucalted

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I

  
  
  
}
