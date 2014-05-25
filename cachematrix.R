

#This function creates a matrix which caches its inverse 
#set funtion is used to set the values in the matrix
#get function returns the matrix stored in x
#setInverse stores the inverse value in cache I
#getInverse returns the inverse value stores in the cache I

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
  
  
  




## This function retireves the inverse of the matrix 'x' from the cache, if present in the cache
##If it is not present in the cache, the inverse is calucalted
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()   ##copies the value from cache into I
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()     #if the inverse value is not present in cache, inverse is calculated
  I <- solve(data, ...)
  x$setInverse(I)
  I

  
  
  
}
