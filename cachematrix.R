## the makeCacheMatrix function will return a list of functions with a matrix cached if previously run

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## initialize m as NULL
        set <- function(y) {
                x <<- y                         ## creates the given matrix y in the function environment
                m <<- NULL
        }
        get <- function() x                     ## function that returns the matrix
        setinv <- function(j) m <<- j           ## sets the inverse
        getinv <- function() m                  ## returns invere function
        list(set = set, get = get, setinv = setinv, getinv = getinv)  ##returns list to run functions
}


cacheSolve <- function(x, ...) {
        m <- x$getinv()                         ## x is a list so this pulls getinv() which is the function that returns inverse
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)                       ## checks if m is not NULL and returns it if so
        }
        data <- x$get()                         ## if m is NULL it goes to this line which pulls in y, original matrix
        m <- solve(data)                        ## calculates inverse of y
        x$setinv(m)                             ## sets the inverse of y to be cached matrix
        m                                       ## return m
}
