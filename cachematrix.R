## Matrix inversion is usually costly computation and this is a example of funcions
## that cache the inverse of a matrix

## This function creates special "matrix" that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ##set add new matrix data
        set <-function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## The following function check the matrix, as a result from the first function, and
## return a matrix that is inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##If is it FALSE, will be called solve to calculate the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)  
        ##return(invMatrix)
        m
}
