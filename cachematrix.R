## This group of functions allows you to cache the inverse of an invertible
## matrix.

## makeCacheMatrix creates the actual matrix object.
## cacheSolve solves and returns the inverse of the matrix.
##  If the inverse has not been calculated yet, cacheSolve
##  will calculate it. If it has been calculated, it will
##  retrieve the cached version.


## Creates the matrix and inverse cache
## Once you have initialized the CacheMatrix object (actually, a list),
## you can call $get to retrieve it or $set to re-initialize 
## it to a new matrix. For example, if you have a matrix m and a
## CacheMatrix cm, and you want to set cm to m, call
## cm$set(m). To view the contents, call cm$get()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # Initializes the inverse cache to null
    
    # Creates a function to set x to a new matrix
    set <- function(y) {
        # x is saved in the makeCacheMatrix environment
        # The <<- operator searhces through parent environments
        # for a variable with that name. We have such a variable
        # in the makeCacheMatrix environment so that is where
        # it is assigned.
        x <<- y 
        
        # inverse is reset to null. If we didn't do this, we would
        # still have the old cached inverse even after
        # assigning a new value to the original matrix.
        inverse <<- NULL
    }
    get <- function() x # Returns the current matrix
    
    # This function saves the calculated inverse in the cache
    setinverse <- function(solve) inverse <<- solve
    
    # This function returns the inverse, null if it isn't cached yet
    getinverse <- function() inverse
    
    # List containing all the functions for the "object."
    # This is what gets returned.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function sees if the inverse has already been calculated and cached
## If it has, it returns the cached version, if it hasn't, it calculates
## the inverse and returns it.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    # If x does not have a cached inverse, this will be null
    inverse <- x$getinverse()
    
    # Checks for a cached inverse. If there is one, it returns it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If we get here, the inverse hasn't been calculated and there's no cache
    
    data <- x$get()
    
    # Here is where the inverse is actually calculated
    inverse <- solve(data, ...)
    
    # Inverse cached back in the original object
    x$setinverse(inverse)
    
    # Return the inverse
    inverse
    
}
