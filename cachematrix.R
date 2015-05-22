## Creates a special "matrix" object that can cache its inverse
## Main function makeCacheMatrix stores four functions (set, get, setinverse, getinversez)
makeCacheMatrix <- function(x = matrix()) { 
    ##Initialise the inverse, i, with NULL
    i <- NULL
    
    ##Replaces matrix x stored in main function with matrix y and resets i since a new matrix is stored
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    
    ##Returns matrix x stored in the main function
    get <- function() 
    {
        x
    }
    
    ##Stores input (inverse) in variable i into main function
    setinverse <- function(inverse) 
    {
        i <<- inverse
    }
    
    ##Returns i, the inverse
    getinverse <- function() 
    {
        i
    }
    
    ##List that stores the four functions defined in this main function
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Argument x takes makeCacheMatrix(matrix())
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##Assigns i with i in getinverse of makeCacheMatrix
    i <- x$getinverse() 
    
    ##If i is not NULL, return i, which is the inverse
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }
    
    ##Continues if i is NULL to compute the inverse
    
    ##data gets the matrix stored in makeCacheMatrix
    data <- x$get() 
    ##The inverse of the matrix is computed and stored in i
    i <- solve(data) 
    ##The inverse is stored in setinverse of makeCacheMatrix
    x$setinverse(i) 
    ##Returns i, the inverse of the matrix
    i 
}