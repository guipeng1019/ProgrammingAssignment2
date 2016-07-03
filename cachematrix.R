## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## makeCacheMatrix - this function creates a "special matrix" that as input for the 2nd function.  
## And it generates a list of 4 operations that could be called in 2nd function
## which are: set matrix value, get matrix value, set matrix inversion value,& get matrix inversion value.


makeCacheMatrix <- function(x = matrix()) {               ## Function to cache calculation results
  Inv <- NULL 

  set <- function(y) {                                    ## Operation: set matrix value
    x <<- y
    Inv <<- NULL
  }
  get <- function() x                                     ## Operation: get matrix value 
  setInversion <- function(Inversion) Inv <<- Inversion   ## Operation: set matrix inversion 
  getInversion <- function() Inv                          ## Operation: get matrix inversion
  list(set = set, get = get,   
       setInversion = setInversion,   
       getInversion = getInversion)   
}


## cacheSolve - check the Inversion result from makeCacheMatrix. 
## If it is in cache get it from cache, otherwise calculate it. 

cacheSolve <- function(x, ...) {                          ## Calculate matrix inversion, x arg must be a makeCacheMatrix
  Inv <- x$getInversion()                                 ## Call the operation inside x: get matrix inversion 
  if(!is.null(Inv)) {                                     ## If the inversion is not NULL (already calculated)
    message("getting cached data")
    return(Inv)                                           ## Return the value from cache
  }
  data <- x$get()                                         ## If not, call the get matrix value operation inside 'x'
  Inv <- solve(data, ...)                                 ## Calculate the inversion
  x$setInversion(Inv)                                     ## and write it into cache so no repeated calculation next time
  Inv
  
  }
