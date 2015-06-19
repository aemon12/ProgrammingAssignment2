## This set of functions can be used to store the result of matrix inversion calculation
## in order not to repeat this lengthy operation.
## If any data within the solved matrix is changed, the inverse result is erased.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInversion <- function(solve) i <<- solve
        
        getInversion <- function() i
        
        list(set=set, get=get, setInversion=setInversion, getInversion=getInversion)
        
        ## Store the matrix inversion calculation result
        ## Manipulate the data within the matrix without retaining the calculated inverse
}


cacheSolve <- function(x, ...) {
        i <- x$getInversion()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } else {
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setInversion(i) 
        
        i
        }
        ## Return a matrix that is the inverse of 'x'
}
