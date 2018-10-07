##This function will cache a provided matrix and it's inverse if the inverse is set
##in the setinverse method. Should the function be re-instantiated or 
##the set method called, the inversed matrix will be nulled out and a new 
##one will need to be provided.
##
##Arguments - x - a matrix to be cached
##
##Returns a list of methods for getting the original matrix, setting a new 
##matrix, setting the inverse matrix and getting the inverse matrix
##
##makeCacheMatrix$get() returns original matrix
##makeCacheMatrix$set(matrix) sets a new matrix
##makeCacheMatrix$getinverse() returns inverse of original matrix
##makeCacheMatrix$setinverse(matrix) sets new inverse matrix
##
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


##Will return the inverse of a provided matrix
##if the matrix is one that has already been provided, the inverse matrix 
##from the cache will be returned otherwise, a new inverse matrix will be 
##calculated and cached
##
##arguments - x - a matrix to get the inverse on
##
##returns the inverse matrix
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
