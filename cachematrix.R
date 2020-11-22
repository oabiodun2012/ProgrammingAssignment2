##  The makeCacheMatrix function will create a special "matrix" object that can 
##  cache its inverse while th cacheSolve function computes
##  the inverse of the special "matrix" returned by makeCacheMatrix if the  
##  inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve will retrieve the inverse from the cache.

## The below function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## argument stores variable x as a 
                                            ## matrix
  
  inv <- NULL                           ## makes the inverse of matrix possible
  set <- function(y) {                   #the value of the matrix is set
    x <<- y                             
    inv <<- NULL                  
  }
  get <- function() x                  ## the value of the matrix argument is 
                                       ## returned by the get function
  
  setinverse <- function(inverse) inv <<- inverse   ## inv in the parent  
                                                 ## environment is been assigned 
                                            ##a value in the present environment
   
  getinverse <- function() inv                      
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                  ##the above will make subsetting of all the functions possible

}




## this function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
   inv <- x$getinverse()                 #inverse of x is retrieved with this 
  if(!is.null(inv)) {
    message("getting cached data")      #the loop will avoid null
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
