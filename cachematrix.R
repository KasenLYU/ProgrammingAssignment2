## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix", which is really a list containing a function to 
## 1.set the set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(m = matrix()){
    inv <- NULL
    set <- function(n){
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSovle <- function(m, ...){
    inv <- m$getinverse()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setinverse(inv)
    inv
}
