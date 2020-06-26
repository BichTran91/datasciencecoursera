## The following function creates a special "matrix" object 
## that can cache its inverse.

## It creates a list containing functions to
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse of the matrix
        ## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        setM <- function(y) {
                x <<- y
                invM <<- NULL
        }
        getM <- function() x
        setInvM <- function(inverse) invM <<- inverse
        getInvM <- function() invM
        list(setM = setM, 
             getM = getM, 
             setInvM = setInvM, 
             getInvM = getInvM)
}


## The following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInvM()
        if(!is.null(invM)) {
                message("get the inverse of the matrix")
                return(invM)
        }
        ## otherwise, calculate the inverse 
        calInvM <- x$getM()
        invM <- solve(calInvM, ...)
        x$setInvM(invM)
        invM
}
