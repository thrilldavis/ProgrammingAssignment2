## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInvMatrix <<- NULL

    set <- function(y) {
        x <<- y
        cachedInvMatrix <<- NULL
    }
    
    
    get <- function() x
    setinverse <- function(inv) cachedInvMatrix <<- inv
    getinverse <- function() cachedInvMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #print()
    #Check if I have already instantiated cacheMatrix, whether it is null and 
    #whether the new matrix is equal to the stored matrix
    if(!exists("cacheMatrix") || is.null(cacheMatrix) || !identical(cacheMatrix$get(),x)){
        cacheMatrix <<- makeCacheMatrix(x)
    }
    
        ## Return a matrix that is the inverse of 'x'
    m <- cacheMatrix$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- cacheMatrix$get()
    m <- solve(data, ...)
    message("setting cached inverse")
    cacheMatrix$setinverse(m)
    m
}
