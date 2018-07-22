## Author: Ajinkya Shinde
## Date : 07/21/2018


## makeCacheMatrix function creates a special "matrix" object and then
## cache its inverse.It returns a list containing a function to 
## 1.set the value for the matrix
## 2.get the value for the matrix
## 3.set the inverse for the matrix
## 4.get the inverse for the matrix
makeCacheMatrix <- function(x = matrix()) {
    #1. Set the inverse of the matrix to null
    inv <- NULL     
    
    #2. Initialize the matrix with passed value - y
    set<- function(y) {
        x <<- y  
        inv <<- NULL
    } 
    
    #3. Get the matrix
    get<- function() x
    
    #4. Set the inverse for the matrix to the passed value - i
    setInverse<- function(i) inv <<- i
    
    #5. Get the inverse for the matrix to the passed value - i
    getInverse<- function() inv
    
    #6. Finally return the list where each element is a function 
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)   
}


## cacheSolve function first checks if the cached inverse of the matrix exists
## and returns it.If the inverse is not cached, it first gets the matrix,
## then computes the inverse for that matrix and caches the inverse.
## Also, returning the cached version.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #1. First, get the cached value of inverse of matrix
    inv<- x$getInverse()
    
    #2. Check if the cached value exists . If yes, return the value
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }

    
    #3. If the cached value does not exist.Calculate and return it
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
        
}
