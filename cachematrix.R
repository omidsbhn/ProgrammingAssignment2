## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL     # Initialize the inverse of x to NULL
        setMat <- function(y){  #Function to set the matrix 
                x <<- y
                invMat <<- NULL  # Remove any inverse matrix previously 
        }
       
        # A function to get the matrix x
        getMat <- function() x
        
        #A function to set the inverse of matrix x
        setInv <- function(i) invMat <<- i
        
        # A function to get the inverse of matrix x
        getInv <- function() invMat
        
        #Return a list consisting the previous functions
        list(setMat = setMat, getMat = getMat, 
             setInv = setInv, getInv = getInv )
        
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        i <- x$getInv()  #Get the inverse of matrix x from the cache function
        
        
        # Check the availability of inverse matrix in cache
        # If it is available, show the massage 
        # and return the cached inverse matrix
        if(!is.null(i)){
                message("Getting cached data!")
                return(i)
        }
        
        # If it is not available 
        d <- x$getMat()  # get the matrix x
        i <- solve(d)  # calculate the inverse of x
        x$setInv(i)  # cache the inverse matrix of x
        i # return the inverse matrix of x
}