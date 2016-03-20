## Caching the inverse of a matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly (there 
## are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## The makeCacheMatrix funcion creates a special object that can cache the 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}


## The cacheSolve function calculate the inverse of the matrix x$get() 
## created by makeCacheMatrix function. If the inverse exists, then the 
## function retrieve it from the cache.

cacheSolve <- function(x, ...) {
     
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
    
}


##test of the code

mat <- matrix(1:4,2,2)

mat_list <- makeCacheMatrix(mat)

mat_list$get()

mat_list$getInverse()

cacheSolve(mat_list)

cacheSolve(mat_list)




