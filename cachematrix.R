## Calculating the inverse of a matrix can be costly. The following pair of 
## functions cache the calculation of the inverse of a matrix so that it
## is calculated on demand and only once.

## Return a list of functions that set and get a matrix and set and get the 
## inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {

    ## Init the inverse property; not initially known so set to NULL.
    i <- NULL
    
    ## Set the matrix to a value; null the inverse as if previously calculated
    ## it will be invalid.
    set <- function(mat) {
        x <<- mat
        i <<- NULL
    }
    
    ## Return the matrix
    get <- function() x
    
    ## Set the inverse matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## Get the inverse matrix
    getinverse <- function() i
    
    ## Return the list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Use the makeCacheMatrix functions to either calculate or return
## the inverse of a matrix.
cacheSolve <- function(x, ...) {

    ## Get the inverse...
    i <- x$getinverse()
    
    ## ...check if it's been cached and if so, return the result
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    
    ## Otherwise get the matrix...
    data <- x$get()
    
    ## ...calculate the inverse, set and return it
    i <- solve(data)
    x$setinverse(i)
    i
}
