## These two functions are used to create a special object
## that stores a matrix and cache it's inverse


## This function creates a special "matrix" which is a list
## of 4 functions that are used to 
## - get / set the matrix
## - get / set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Used to store the inverse
    m <- NULL  
    
    # Setter for the matrix
    set <- function(y) {
        
        #Store the matrix in a different environnement
        x <<- y
        
        # When new data is set the inverse must be reinitialized
        m <<- NULL
    }
    
    # Getter for the matrix
    get <- function() x
    
    # Setter for the inverse
    setinverse <- function(inverse) m <<- inverse
    
    # Getter for the inverse
    getinverse <- function() m
    
    # Return all 4 functions in a list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    # Get the cached inverse
    m <- x$getinverse()
    
    # If the inverse is cached
    if(!is.null(m)) {        
        message("getting cached data")
        
        # Return the cached value of the matrix and exit the function
        return(m)
    }
    
    # Get the original matrix
    data <- x$get()
    
    # Solve its inverse
    m <- solve(data, ...)
    
    # Cache the inverse
    x$setinverse(m)
    
    # Return the inverse
    m
}
