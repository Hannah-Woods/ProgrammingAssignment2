## These functions together calculate the inverse of a matrix, and cache the inverse so that if the inverse is called again, it does not need to be recalculated.

## The makeCacheMatrix function creates a list of four functions that can recall or set the values and inverse of the matrix x.  Note that setting the inverse directly by calling the seting function prevents the inverse from being calculated correctly, but is necessary to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {                         #sets the values of the matrix
        x<<-y                                  
        inv<<-NULL
    }
    get<-function() x                          #gets the values of the matrix
    setinv<-function(solve) inv<<- solve       #sets the inverse of the matrix
    getinv<-function() inv                     #gets the inverse of the matrix
    list(set=set, get=get,                     #Output is a list of these four functions
        setinv=setinv, getinv=getinv)             
}


## ThecacheSolve function takes the matrix x imputed into the makeCacheMatrix function and either returns the previously cached inverse of the matrix x, or calculates and caches the inverse of the matrix x.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)) {                    #check if inverse has already been done   
            message("getting cached data")     #if yes, output is return message and                   
            return(inv)                         #stored result
        }
        data<-x$get()                          #retrieves values of matrix 
        inv<-solve(data, ...)                  #calculates inverse
        x$setinv(inv)                          #stores result
        inv                                    #output is inverse matrix
}

