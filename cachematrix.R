## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set it to NULL
    m.in <- NULL
    # store a matrix
    set <- function(y) {
        x <<- y
        # since the matrix is assigned a new value, flush the cache
        m.in <<- NULL
    }
    # returns the stored matrix
    get <- function() x
    # cache the given argument 
    setinverse <- function(Inverse) m.in <<- Inverse
    # get the cached value
    getinverse <- function() m.in
    # return a list. Each named element of the list is a function
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
    # get the cached value
    m.in <- x$getinverse()
    # if a cached value exists return it
    if(!is.null(m.in)) {
        message("getting cached data")
        return(m.in)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    data <- x$get()
    m.in <- pseudoinverse(data, ...)
    x$setinverse(m.in)
    # return the inverse
    m.in
}

# function test code
a <- matrix(1:10, nrow = 10, ncol = 20)
i.a <- makeCacheMatrix(a)
i.a$get()
cacheSolve(i.a)
in.a <- cacheSolve(i.a)
in.a %*% a
