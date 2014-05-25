## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makes a cached version of a matrix. A matrix will
## have to be provided for input.
makeCacheMatrix <- function(x = matrix()) {

        invMat <- NULL  ## Reset  invMat
        ## These 4 functions are similar to the one in the example
        ## provided in the assignment instructions.
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(invMatrix) invMat <<- invMatrix
        getinverse <- function() invMat
        
        ## In the last part we basically "name" the various methods
        ## makeCacheMatrix contains.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getinverse()
        ## Test if the solution is cached. If so, return it.
        if (!is.null(a)){
                print ("Retrieving cached matrix....")
                return(a)
        }
        ## If we get to this point, the inverse matrix is not cached
        ## Hence we have to solve it, and store the solution.
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
