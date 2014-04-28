## makeCacheMatrix function generates a matrix and its inverse needs to be
## calculated by cacheSolve function. The cacheSolve function could retrieve
## the previously calculated inverse of matrix if the matrix is returned by 
## makeCacheMatrix function again.

## This makeCacheMatrix function creates a matrix that could cache its inverse
## by using four functions(set,get,setinv and getinv)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
               x <<- y
               m <<- NULL
        }
        get <- function ()x
        setinv <- function(solve) m <<- solve
        getinv <- function()m
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)

}


## This cacheSolve function gets the inverse of the matrix delivered by 
## makeCacheMatrix function and check if the inverse has already been 
## calculated. If so, the function will retrive the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                  message("getting cached data")
                  return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}



