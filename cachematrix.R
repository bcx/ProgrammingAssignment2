
## `makeCacheMatrix` and `cacheSolve`
##
## This file contains the `makeCacheMatrix` function, which is used 
## to construct a wrapper object.  This wrapper object holds a matrix,
## which can be retrieved using the `get` method and updated with 
## the `set` method.
##
## The wrapper also contains the functions `getInv` and `setInv`, 
## which exist to support use of the wrapper with the `cacheSolve`
## function.

## Calling `cacheSolve` on a wrapper instance will extract and return
## the inverse of the wrapped matrix if it has been previously calculated. 
## Otherwise it will calculate the inverse and store it in the wrapper 
## for future use.
##
## Sample Usage:
## -------------
##    > m <- matrix(c(-1, -2, 1, 1), 2, 2)
##    > cm <- makeCacheMatrix(m)
##    > cacheSolve(cm)
##         [,1] [,2]
##    [1,]    1   -1
##    [2,]    2   -1
##    > cacheSolve(cm)
##    getting cached data
##         [,1] [,2]
##    [1,]    1   -1
##    [2,]    2   -1
##    > 

## ==================================================================
## `makeCacheMatrix`
## ==================================================================
##
## Create & return a matrix wrapper object.
##
## May be invoked with an initial matrix to wrap, otherwise will
## initially wrap an empty matrix.
##
## Note that updating the wrapped matrix via `set` will clear the
## cached inverse.
##
## The cached inverse of the wrapped matrix can be retrieved (or 
## computed, set, and retrieved, if it has been cleared or not 
## yet set) using the `cacheInverse` function.

makeCacheMatrix <- function(x = matrix()) {

    # Matrix inverse is initially unset.
    inv <- NULL
    
    # Wrapped matrix can be updated. 
    # Doing so will clear the cached inverse.
    set <- function(m) {
      x <<- m
      inv <<- NULL
    }
    
    # Wrapped matrix can be retrieved.
    get <- function() x
    
    # Inverse of the wrapped matrix can be get and
    # set using these methods.
    setInv <- function(inv) inv <<- inv
    getInv <- function() inv
    
    # Return a list of methods (functions).  These functions
    # all share the same environment, which consists of the
    # matrix `x` and it's cached inverse `inv`.
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## ==================================================================
## `cacheSolve`
## ==================================================================
##
## This function can be invoked with an instance of the matrix wrapper
## object created using `makeCacheMatrix`.  Invoking it will return the
## inverse of the wrapped matrix.
##
## The inverse will be retrieved from the wrapper's cache if the cache
## has been set, otherwise `cacheSolve` will compute the inverse, store
## it using the wrapper's `setInv` method, and then return it.

cacheSolve <- function(x, ...) {

  # `x` should be a matrix wrapper created with `makeCacheMatrix`.
  
  # Attempt to read the cached inverse of the wrapped matrix. 
  inv <- x$getInv()
  
  # If the cached inverse is not null, return it.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise compute the inverse of the wrapped matrix...
  inv <- solve(x$get())
  
  # ... set the inverse in wrapper ...
  x$setInv(inv)

  # ... and then return the computed inverse.
  inv
}

