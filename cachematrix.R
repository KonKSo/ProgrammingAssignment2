
## makeCacheMatrix reates "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(invSolve) s <<- invSolve
        getSolve <- function() s
        list(set = set, get = get,
             setsolve = setSolve,
             getsolve = getSolve)

}


## cacheSolve computes the inverse of a "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
