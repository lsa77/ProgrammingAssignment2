##I couldn't figure out why do I need a matrix here so I decided to stay with a vector.
##this works as in dr. Peng's example but in matrix context
makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    set_inverted <- function(solve) inverted <<- solve
    get_inverted <- function() inverted
    list(set = set, get = get,
         set_inverted = set_inverted,
         get_inverted = get_inverted)
}

##cacheSolve checks if there is an inverted version of the matrix in the cache. if there is one - returns it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$get_inverted()
    
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    
    data <- x$get()
    inverted <- solve(data, ...)
    x$set_inverted(inverted)
    inverted
}
    

