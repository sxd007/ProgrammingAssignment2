## to make a matrix to cache the inverse matrix

## a function to set up a list that set matrix, get matrix, set inverse matrix and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y         
                m <<- NULL
        }
                
        get <- function() x     
        
        setinverse <- function(){
                m <<- solve(x)
        }
        getinverse <- function() m        
        
        list(set= set, get= get, setinverse = setinverse, getinverse = getinverse)
        
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        x$setinverse()
        m <- solve(data)
        m        
}
