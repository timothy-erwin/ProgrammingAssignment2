## Put comments here that give an overall description of what your
## functions do  ::
## These functions combine to store a matrix, check subsequent entries
## as different from the current matrix, and perform the same functions
## for the Solve function that is performed on the entered matrices.
##
##
## Write a short comment describing this function :: 
## This function creates a special "matrix" object that
## is used to set and get the can cache its inverse. It
## also checks the set matrix versus the cached matrix,
## setting a flag in the parent environment for use in
## the inverse function to inform the user that they have
## not changed the matrix and the stored matrix is being
## returned.  This functions also stores and gets the
## inverse.
makeCacheMatrix <- function(x = matrix(), initialized = FALSE) {
        s <- NULL
        set <- function(y) {                            
                library("compare")
                if(initialized == FALSE) {
                        if(is.na(x))  {
                                x <<- y
                                initialized <<- TRUE 
                                return("Matrix initialized.")
                        }
                        
                }        
                equalityCheck <<- isTRUE(compareEqual(x, y, transform=character(),
                        ignoreDimOrder=FALSE))
                        if(equalityCheck == TRUE){
                                print("Same matrix entered.")
                                if(inverseChanged == FALSE) {
                                        equalityCheck <<- FALSE
                                }
                                else {
                                        inverseChanged <<- FALSE
                                }
                        }
                        else {
                        x <<- y
                        print("Matrix changed.")
                        inverseChanged <<- FALSE
                        }
        }
        get <- function() x
        storeMatrixInverse <- function(solve) s <<- solve
        getMatrixInverse <- function() s
        list(
                set = set,
                get = get,
                storeMatrixInverse = storeMatrixInverse,
                getMatrixInverse = getMatrixInverse)
}
##
##
## Write a short comment describing this function ::
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, inverseChanged = FALSE, ...) {
        if(is.null(x$getMatrixInverse())) {data <- x$get()
                            s <- solve(data)
                            x$storeMatrixInverse(s)                                 
                            equalityCheck <<- TRUE
                            inverseChanged <<- TRUE
                            print("Cache initialized.")
                            return(s)
                
        }
        if(equalityCheck == TRUE){
        print("Input matrix has not changed.")
        print("Searching for stored inverse.")
        print("Please enter a different matrix in a$set(matrix(##:##)) for a different result")
        print("The stored inverse is;")
        print(x$getMatrixInverse())
        }
        else {data <- x$get()
        s <- solve(data)
        x$storeMatrixInverse(s)                                 
        equalityCheck <<- TRUE
        inverseChanged <<- TRUE
        s
        }
}

## Please assign an object such as "a" the function makeCacheMatrix(),
## the set the matrix for evaluation using a$set(),
## and find the inverse by calling cacheSolve(a).