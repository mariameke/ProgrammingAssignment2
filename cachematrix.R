## The following functions cache the inverse of a matrix

## The following function creates a special "matrix" that is actually  
## a list of functions as follows: 
## 1. set to Set values of the matrix 
## 2. get to Get values of the matrix 
## 3. setinverse to Set the values of the inverse matrix 
## 4. getinverse to Get the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {   
                x <<- y
                m <<- NULL
        }
        get <- function()     
                x      
        setinverse <- function(solve) 
                m <<- solve
        getinverse <- function()      
                m
        ## Return a list of the makeCacheMatrix functions
        list(set = set, get = get,    
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function computes the inverse of the special matrix "x" 
## returned by makeCacheMatrix
## If inverse matrix computed, get the inverse matrix from cache
## If not, compute the inverse matrix of the data and set it in the cache
cacheSolve <- function(x, ...) { 
        m <- x$getinverse()
        if(!is.null(m)) {                 
                message("getting cached data") 
                return(m)
        }
        data <- x$get()             
        m <- solve(data, ...)      
        x$setinverse(m)
        m
}
