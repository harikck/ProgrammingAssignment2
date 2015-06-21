

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## Uses the <<- operator to assign a value to an object in an environment 
## that is different from the current environment. 


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## set the value of the vector
  set <- function(y) {
    ## assigning a value to an object using '<<-' in an environment 
    ## different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  
  ## return the value of the vector
  get = function() {
    x
  }
  
  ## set the inverse of the matrix
  setInv <- function(inverse) {
    inv <<- inverse 
  }
  
  ## return the invoice of the matrix
  getInv <- function(){
    inv
  } 
  
  ## retun a list containing the functions
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


## Function to calculate the inverse of a vector .
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
  
  ## Get the inverse of the matrix x
  inv <- x$getInv()
  
  ## if the inverse is not null
  if (!is.null(inv)){
    ## get it from the cache. 
    ## return it and skips the computation
    return(inv)
  }
  
  ## if inverse is not cached, calculates the inverse 
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  ## sets the value of the inverse in the cache.
  x$setInv(inv)
  
  ## return the calculated inverse of the matrix x
  return(inv)
  
}
