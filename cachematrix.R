## This group of functions allows you to cache the inverse of an invertible
## matrix.

## makeCacheMatrix creates the actual matrix object.
## cacheSolve solves and returns the inverse of the matrix.
##  If the inverse has not been calculated yet, cacheSolve
##  will calculate it. If it has been calculated, it will
##  retrieve the cached version.


## Creates the matrix and inverse cache
## Once you have initialized the CacheMatrix object,
## you can call $set or $get to re-initialize it to a new
## matrix. For example, if you have a matrix m and a 
## CacheMatrix cm, and you want to set cm to m, call
## cm$set(m). To view the contents, call cm$get()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ##Initializes the inverse cache to null
    
    # Creates a function to set x to a new matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x #returns the current matrix
    
    #This function saves the calculated inverse in the cache
    setinverse <- function(solve) inverse <<- solve
    
    #This function returns the inverse, null if it isn't cached yet
    getinverse <- function() inverse
    
    #List of all the available functions for the object
    #so that they can be called with $ notation
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function sees if the inverse has already been calculated and cached
## If it has, it returns the cached version, if it hasn't, it calculates
## 

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    #If x does not have a cached inverse, this will be null
    inverse <- x$getinverse()
    
    #Checks for a cached inverse. If there is one, it returns it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #If we get here, the inverse hasn't been calculated and there's no cache
    
    data <- x$get()
    
    #Here is where the inverse is actually calculated
    inverse <- solve(data, ...)
    
    #Inverse cached back in the original object
    x$setinverse(inverse)
    
    #Return the inverse
    inverse
    
}
