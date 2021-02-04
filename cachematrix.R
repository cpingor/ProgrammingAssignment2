## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        get<-function() x
        setInverse<-function(inverse_mat) m<<-inverse_mat
        getInverse<-function() m
        list(get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the object returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
        return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
}
