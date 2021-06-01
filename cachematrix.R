## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## THis function store a matrix and after using the cacheSolve function will 
## store the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function return the inverse of a matrix. If the Matrix was defined in 
## the function makeCacheMatrix and cacheSolve already used it will return the 
## the result already stored without calculation and the message "getting cached
## data".
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
