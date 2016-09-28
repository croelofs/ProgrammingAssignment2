## makeCacheMatrix is a function that creates an object to store a
## matrix and its inverse. The function cacheSolve uses this object
## to set the inverse of the matrix. When executed a second time 
## this function will be much faster, as the inverse is cached in 
## the object made by makeCacheMatrix.

# This function creates a special "matrix" object that can cache its inverse.
# It 'contains' 4 functions that set the value of the matrix, get the value of
# the matrix and set and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     # initialize inverse
        set <- function(y) {
                x <<- y       # set matrix values
                inv <<- NULL  # set inverse back to NULL
        }
        get <- function() x           # get matrix values
        setinverse <- function(inverse) inv <<- inverse # set the inverse of the matrix
        getinverse <- function() inv  # get the inverse of the matrix
        list(set = set, get = get,    # return list of functions to do stuff
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the "matrix"-object returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse that is cached in the object.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse() # get inverse of matrix
        if(!is.null(inv)) { # check whether inverse is already calculated
                message("getting cached data") 
                return(inv) # return inverse that was already calculated (cached)
        }
        data <- x$get() # get the values of x (the matrix)
        inv <- solve(data, ...) # calculate inverse of the matrix 
        x$setinverse(inv) # set the inverse of vector
        inv
}
