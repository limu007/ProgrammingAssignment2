## creates extended class with "storage" for matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## if no cached result is found in x, cacheSolve uses "solve" function to calculate it (and store in x)
## extra arguments (to "solve") allowed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) { return(i)}
        i<-solve(x$get(), ...)
        x$setinv(i)
        i
}
