# ---------------------------------------------------------------------------- #
# Matrix inversion is usually a costly computation and there may be benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
#
# This R file contains the following functions :
#
#    1. makeCacheMatrix: This function creates a special "matrix" object 
#       that can cache its inverse.
#
#    2. cacheSolve: This function computes the inverse of the special "matrix" 
#       created with makeCacheMatrix.  If the inverse has already been computed
#       and the matrix has not changed, then cachesolve retrieves the inverse 
#       from the cache.
#
# ---------------------------------------------------------------------------- #

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special matrix, which supports the caching of  
    # its inverse matrix, for easy retrieval and avoid re-compution.
    #
    # Args:
    #   x : a standard matrix.   
    # 
    # Return Value:
    #   a list containing four functions:
    #       1. set() - sets the value of the matrix
    #       2. get() - gets the value of the matrix
    #       3. setinv() - sets the value of the inverse of the matrix
    #       4. getinv() - gets the value of the inverse of the matrix

    inv_x <- NULL  # initialize the inverse cache

    set <- function(y) {
        x <<- y
        inv_x <<- NULL  # clears inverse cache when the matrix value is changed.
    }

    get <- function() {
        x
    }
    
    setinv <- function(inverse) {
        inv_x <<- inverse
    }
    
    getinv <- function() {
        inv_x
    }
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

# ---------------------------------------------------------------------------- #

cacheSolve <- function(x, ...) {
    # This function returns the inverse of the special "matrix" previously 
    # created by the makeCacheMatrix function defined above.
    #  
    # It stores the computed inverse value into the matrix x's cache, which is
    # queried and returned for subsequent calls to this function for the same
    # matrix.
    #
    # Args:
    #   x : a special matrix created using makeCacheMatrix().  
    #       
    # Returns:
    #   The inverse matrix of x
    #
    # Exceptions:
    #   solve(x) will throw errors if x is not a square invertible matrix.
    

    # Retrieve the inverse matrix of 'x' from its cache 
    i <- x$getinv()
    if (is.null(i)) {        
        # The inverse is not cached.  So, call solve() to compute its inverse.
        message("Info : calling solve() to compute the inverse matrix")
        i <- solve(x$get())

        # Now store the computed inverse matrix into cache
        message("Info : storing inverse matrix into cache")
        x$setinv(i)

        # return the computed inverse matrix.
        return(i)
    } 
    else {
        # the inverse is available in the cache.  So, skip solve() and 
        # return the cached inverse matrix.
        message("Info : returning the inverse matrix from cache")
        return(i)
    }
}
# ---------------------------------------------------------------------------- #
