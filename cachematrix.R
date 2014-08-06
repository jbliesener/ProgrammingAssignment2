## This code implements an object that caches a matrix
## and its inverse

## makeCacheMatrix(x=matrix()) creates a list that holds 
## functions to access a matrix and its inverse for reading
## and writing:
##   $set: allows to set a new matrix
##   $get: gets the stored matrix
##   $setInverse: sets the inverse matrix
##   $getInverse: gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # inverse matrix holder
        xInv <- NULL
        
        # function to set original matrix, clears the 
        # inverted matrix every time a new original
        # matrix is set. This guarantees that solve()
        # will be excuted on the next call to cacheSolve
        # whenever the matrix changes
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        
        # function to return the original matrix
        get <- function() x
        
        # function to set the inverted matrix
        setInverse <- function(inverse) xInv <<- inverse
        
        # function to return the inverted matrix
        getInverse <- function() xInv
        
        # create & return list object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix stored
## within a list created with the makeCacheMatrix function.
## If the matrix hasn't be inverted yet, it calls solve()
## and stores the result within the list object. If, however,
## the matrix has already been inverted before and hasn't 
## changed since that, the inverted matrix is recovered
## from the list and returned.

cacheSolve <- function(x, ...) {
        ## get stored inverted matrix
        inv <- x$getInverse()
        
        ## does it exist?
        if(!is.null(inv)) {
                # not null, has been calculated before
                message("getting cached data")
                return(inv)
        }
        
        # null, invert matrix and store result
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
