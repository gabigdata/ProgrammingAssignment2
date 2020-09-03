## Gabriela 2assignment 
##Functions to find the inverse of a square matrix

## This function is like a squeleton that returns a list of functions that
## once we run the cacheSolve function this list will have the result of the inverse
## of our matrix
## if the matrix if already exists is alreadycached otherwise a new one is
## created and save in the list

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function recieves the list of functions from the previously function
## This function solves the inverse of the matrix previously saved in the previously function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cache inverse")
        return(inverse)        
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}