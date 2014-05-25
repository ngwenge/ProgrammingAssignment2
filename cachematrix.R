## Basically, there are two "parent" functions being created, 'makeCacheMatrix', the one that 
## gets and caches the matrix; 'cacheSolve', the one that execute the inverse matrix function. 

## 'makeCacheMatrix' basically consists of four "child"/subfunctions, which are 'set', 'get', 
## 'setinverse' and 'getinverse' functions and these function generally help to cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## 'cacheSolve' is executing the 'solve' function which provides the inverse of the matrix from
## 'makeCacheMatrix'. If a matrix is executed once with this function, the result will be cached 
## it will print out the message "getting cached data" together with the answer when we execute 
## the same matrix for the second time (or more). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
