## July 2017
## Author Maryna
## This function creats a list which contains four functions:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix in cache
##get the value of the inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## creating a memory address (placeholder) to use for cache
  set <- function(y) {
    x <<- y #assign to x a new matrix value different from initial, "<<" reassigning x in parent environment
    m <<- NULL #cleaning cache from previous inverse matrix, "<<" reassigning m in parent environment 
  }
  get <- function() x #define function that returns x if called from other function
    setinv <- function(inverse) m <<- inverse #get inverse matrix through
#argument "inverse" and set it into cache
    
    getinv <- function() m #retrives value from the cache
    
 # Returned list is a product of makeCacheMatrix function
   list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function gets value from cache and if it's not a NULL 
## then returns it as a result.
## If cache is empty then this function performs assigned operation with data
## in this case makes inverse matrix, and transfer the result to cache

## In both functions makecachevector and cacheMean
## the argument is "x". It can be confusing because
## in cacheMean and cacheSolve the argument "x" is actually
## the resulted list of four functions from
## makecachevector and makeCacheMatrix accordingly
## To call these functions from console:
## matr<<-matrix(c(4,2,7,6),2,2)
## callCon<- makeCacheMatrix(matr)
## cacheSolve(callCon)
## I used "madeList" instead of "x" in list of arguments below

cacheSolve <- function(madeList, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- madeList$getinv() #get value from cache
  if (!is.null(m)) { #if cache is not empty then return its value
    message("getting cached data")
    return(m)
  }
  data <- madeList$get() # get value of matrix from makeCacheMatrix and assign it to data
  m <- solve(data, ...) #inverse matrix stored in data and assign it to "m"
  madeList$setinv(m) # set m into cache  
  m #return inverse matrix
}
