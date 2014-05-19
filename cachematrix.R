## Function makeCacheMatrix 

### Description
# This function creates a special "matrix" object that can cache its inverse.
# Computing the inverse of a square matrix can be done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) returns 
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(new_matrix) m <<- new_matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

### Function cacheSolve 

### Description
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## To test\

### Source the R code
# source("cachematrix.R")

### Create a Matrix
# myMatrix <- matrix(1:4,2,2)

### Assign Matrix to Cache
# myCacheMatrix <- makeCacheMatrix(myMatrix)

### Solve the Inverse Matrix, if is Cache bring Cahe value.
# cacheSolve(myCacheMatrix)

### Run again to confirm is pulling form Cache.
# cacheSolve(myCacheMatrix)
