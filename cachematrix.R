##This function initiall creates the structure of the matrix and then after cacheSolve has been called atleast once 
##will start caching the inverse of the matrix. Function can be tested as follows
## create a variable z <-makeCacheMatrix(matrix(1:4,2,2)) to cache the matrix structure
## check by typing at the prompt >z$get()
## you should get the following result
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## >z$getinv() should return a NULL value

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL # local scope to the makeCacheMatrix function
  set <- function(y) # function sets up matrix inverse value
  {
    x <<- y # set this with parent scope and assign the matrix value 
    minv <<- NULL # sets this with parent scope and matrix inverse value is not set yet
  }
  
  get <- function() {x} # return the same matrix
  
  setinv <- function(matrixinv) { # function to set the matrix inverse value
    minv <<- matrixinv
  }
  
  getinv <- function(){minv}
  
  list(set=set, get=get, setinv=setinv, getinv=getinv) #create a list of matrix functions to be accessed in                                                      #the cacheSolve function
}


## cacheSolve function will take a matrix as input. e.g. z matrix(1:4, 2,2) and will calculate the inverse the first time
## after which it will set the value of the matrix in cache by using x$setinv(minv) below. You can check this function by 
##typing the following at the prompt cacheSolve(z) and get the following output
## >cacheSolve(z)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##then type cacheSolve(z) again at the prompt
##you should get the following output
##Data Generated from Cache
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv() # this is null when function executes for the first time
  if(!is.null(minv)){
    message("Data Generated from Cache")
    return(minv)
  }
  
  matrixvalue <- x$get() # get the data using 
  minv <- solve(matrixvalue) # get inverse
  x$setinv(minv) # set cache the first time
  minv # return minv
}
