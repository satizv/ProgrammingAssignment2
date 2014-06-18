# File Name     -   cachedmatrix.R
# Function Name -   makeCacheMatrix
# Description   -   To define and retrive matrix variables
# Inputs        -   Input Matrix
# Output        -   Output Matrix
# Sub Functions -   set() - Function to define the Global value of the input matrix
#               -   get() - Function to retrive the value of the input matrix
#               -   setinverse - Function to define the Global Value of the inverse
#               -   getinverse - Function to retrive the value of the inverse
########################################################################################

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y,r,c) {
    x <<- matrix(y,nrow=r,ncol=c)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse ,
       getinverse = getinverse )
}
########################################################################################
# File Name         -   cachedmatrix.R
# Function Name     -   cacheSolve
# Description       -   To calculate the inverse of a matrix. 
#                       Retrives inverse from cache if already calculated
# Calling Function  -   makeCacheMatrix 
# Inputs          -     Input Matrix by calling MakeCacheMatrix. 
#                       Use set() sub function in makeCacheMatrix to define variable
# Output          -     Output Matrix
########################################################################################
cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}