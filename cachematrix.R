## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function basically has 4 "methods". You can either
## 1) call the "set" method and provide a new matrix as input
## 2) calling the "get" method will retrieve the matrix, or return NA
## 3) setinverse will store the result of the inverse of the matrix
## 4) getinverse will retrieve the inverse matrix

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
## This function will return the inverse of a matrix, provided
## an non-singular matrix. If result is stored in x, then
## the result will not be calculated. If it is not stored, then
## it will be calculated and stored in x.

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
