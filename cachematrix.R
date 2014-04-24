# The following pair of functions provide the means to cache the
# inverse of a matrix in order to avoid repeated computations.


# The makeCacheMatrix function creates a special "matrix" object that 
# can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        
        # minv is the global variable that stores the 
        # matrix inverse
        
        minv <- NULL 

        set <- function(y) {
                x    <<- y
                minv <<- NULL
        }

        #The 'get' function returns the original matrix.
        get   <- function() x 
        
        #The 'getMatrixInv' function returns the matrix inverse.
        getMatrixInv <- function() minv
        
        #The 'setInv' function assigns the matrix inverse
        #to global variable minv.
        
        setInv <- function(mi)     minv <<- mi
        
        #The object's list of 'public' functions:
        
        list(set = set,
             get = get,
             setInv = setInv,
             getMatrixInv = getMatrixInv)        
        
}




# The cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        
        #The referenced object's matrix inverse 
        #is assigned to local variable 'mi'.
        
        mi <- x$getMatrixInv()
        
        #If the matrix inverse has been already 
        #calculated then the cached result is returned.
        
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        
        
        #Else the matrix inverse is calculated (the matrix 
		#supplied should be invertible)
        
        mi <- solve(x$get())
        
        #Finally, cache the result into the referenced object's
        #variable minv.
        
        x$setInv(mi)
        
        mi      
         
        
        
}
