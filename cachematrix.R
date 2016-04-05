## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  ## Function to create a matrix that can cache its own matrix inverse

  makeCacheMatrix <- function(x = matrix()) {
    ## initially set to NULL
    m <- NULL
    
    set <- function(y) {
      x <<- y     # set the value
      m <<- NULL  # clear the cache
    }
    
    ## used to get our matrix
    get <- function() x
    
    ## used to set the matrix inverse
    setInverse <- function(Inverse) m <<- Inverse
    
    ## used to get the matrix inverse
    getInverse <- function() m
    
    list(set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
    ## results listed as the following:
    ## set value of the matrix
    ## get value of the matrix
    ## set value of the matrix inverse
    ## get value of the matrix inverse
  }


## Write a short comment describing this function

  
  ## Function that will compute the inverted matrix set by makeCacheMatrix.
  ## If the inverse has been created with the matrix left unchanged,
  ## retrieves the cached result
  
  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheSolve <- function(x, ...) {
      
      ## to get the inverse of the matrix inside 'x'
      m <- x$getInverse()
      
      ## this is to return the cached version of 'x', rather than computing again
      ## because m is not empty
      if(!is.null(m)) {
        message("getting cached data")
      return(m)
      }
      
      ## in this case, cache is empty, so the matrix inverse must be calculated again
      data <- x$get()         ## get the value of the matrix
      m <- solve(data, ...)   ## get the matrix inverse
      x$setInverse(m)         ## cache this result
      
      ## return the caching matrix
      m
   }
