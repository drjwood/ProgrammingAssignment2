## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initialises a cacheMatrix object from the passed matrix x, 
# setting x in its environment. Four functions are returned, enabling setting and
# retrieving the matrix x itself, as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # var inv will store the inverse of the matrix x
        
        # define the functions to set and get the matrix itself, and those to set and get the inverse
        set <- function(y) {
                # set is a func that will assign matrix x in the cacheSolve env to the y passed
                # and inv in the cacheSolve env to null (resets inverse)
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        # get simply returns matrix x (which is defined in the cacheSolve env)
        setinverse <- function(inverse) inv <<- inverse
        # setinverse assigns to inv in the cacheSolve env the inverse (matrix) passed
        getinverse <- function() inv
        # getinverse simply returns inverse matrix inv (which is defined in the cacheSolve env)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        # makeCacheMatrix returns a named list of the four functions defined above
}


## cacheSolve takes the cacheMatrix object x and calculates its inverse, assigning this to the
# inv variable, accessed in turn using the functions provided by makeCacheMatrix.
# It first tests x to see whether the inverse is already available (ie, cached), and
# if so simply returns that directly without further calculation.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # var inv is set to the value of the inverse assigned to
        # the passed cacheMatrix object x
        if(!is.null(inv)) {
                # if inv has a value, return it and we're done - NOT RECALCULATED
                message("getting cached inverse matrix")
                return(inv)
        }
        # processing continues here only when inv has not already been determined
        data <- x$get()
        # var data is set to the matrix assigned to the passed cacheMatrix object x
        inv <- solve(data, ...)
        # inv is set to the calculated inverse of the matrix using solve
        # any additional parameters are passed directly to the solve function
        x$setinverse(inv)
        # cacheMatrix object x has its inverse set to the value of inv
        inv
        # cacheSolve returns the newly calculated inverse of matrix x
}
