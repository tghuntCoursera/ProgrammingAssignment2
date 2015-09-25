## 
#   This file provides two functions that aid in the storage of
#   a matrix and it's inverse.
#
#   This R file provides the following function
#       makeCacheMatrix
#       cacheSolve
#   See below for more details
#
##


## 
#  The makeCacheMatrix function returns a object that provides the 
#  following:
#
#   Functions:
#       set         -> Provides a method to set the current matrix in the object
#       get         -> Provides a method to return the current matrix from the object
#       setInverse  -> Provides a method to set the inverse matrix in the object
#       getInverse  -> Provides a method to get the inverse matrix from the object\
#
#   Storage
#       A matrix specified by the user
#       The inverse of the matrix if setInverse is called
#
##
makeCacheMatrix <- function(x = matrix()) {
    # Holds the current matrix
    currMatrix <- NULL
    
    # Holds the current inverse matrix
    # or NULL is a new object is created or a new matrix is set
    invMatrix <- NULL
    
    # Set function to set the current matrix
    # When called it clears the current inverse matrix to force
    # it to be solved again.
    set <- function(y) {
        currMatrix <<- y
        invMatrix <<- NULL
    }
    
    # Get function to return the current matrix, or NULL if not set yet
    get <- function() currMatrix
    
    # Set function to set the current inverse matrix
    setInverse <- function(inverse) invMatrix <<- inverse
    
    # Get function to return the current inverse matrix, or NULL if not set yet
    getInverse <- function() invMatrix
    
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## 
# cacheSolve function requires a user supplied makeCacheMatrix object (function above)
#   and does not provide any callable sub functions.
#   
#   This function utilizes the functions in the supplied object to:
#       get the current inverse function
#       if a NULL is returned the function will call the necessary functions to:
#           get the objects current matrix
#           generate an inverse matrix
#           store the inverse matrix in the object
#       return the inverse matrix
##
cacheSolve <- function(x, ...) {

    # Calls the objects getInverse function
    invMatrix <- x$getInverse()
    
    # If returned inverse matrix is not NULL then return it
    if(!is.null(invMatrix)) {
        # Inform the user that a cached copy is being returned
        message("Returning cached inverse matrix")
        return(invMatrix)
    }
    
    ## Call the objects get function to get the current matrix
    tmpMatrix <- x$get()
    
    # call the solve function that returns the inverse of a standard square matrix
    tmpInvMatrix <- solve(tmpMatrix)
    
    # Call the objects setInverse function to cache the current inverse matrix
    x$setInverse(tmpInvMatrix)
    
    # Return the inverse matrix.
    tmpInvMatrix
}


########
# Validation notes:
#
#   The following site was referenced to as a source to determine
#   if the function was operating as intended.
#       https://www.mathsisfun.com/algebra/matrix-inverse.html
#
#   The following simple matrix was used during testing
#   x <- matrix(c(3,3.2,3.5,3.6), 2,2)
#
#   The output is as follows
#   x
#        [,1] [,2]
#   [1,]  3.0  3.5
#   [2,]  3.2  3.6
#
#   The returned inverse matrix is as follows
#   solve(x)
#        [,1]  [,2]
#   [1,]   -9  8.75
#   [2,]    8 -7.50
#
#   The returned cached matrix is as follows
#   solve(x)
#        [,1]  [,2]
#   [1,]   -9  8.75
#   [2,]    8 -7.50
#
########
