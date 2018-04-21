makeCacheMatrix <- function(i = matrix()) {
  inva <- NULL
  #setting up the matrix
  setvalue <- function(j) {
      i <<- j
    inva <<- NULL
  }
  #retrieving the matrix
  getvalue <- function() i
  
  #setting up the inverse of the marix
  setinva <- function(inverse) inva <<- inverse 
  
  #getting the inverse of the matrix
  getinva <- function() inva
  
  #returning the list of the methods
  list(setvalue=setvalue, getvalue=getvalue, setinva=setinva, getinva=getinva)
}

## Computing the inverse of the  matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated,then the "cachesolve" 
## should retrieve the inverse from the cache

cacheSolve <- function(i, ...) {
  
  ## Returning  a matrix that is the inverse of 'i'
  inva <- i$getinva()
  #retrieving the inverse of the matrix if it is already created and set
  if (!is.null(inva)){
      message("getting cached data")
    return(inva)
  }
  
  #get the matrix from the object.
   indata <- i$getvalue()
   
  ##calculating inverse of the matrix if it is not already in cache
  inva <- solve(indata)
  i$setinva(inva)
   return(inva)
}

m <- matrix(rnorm(9),3,3)
m1 <- makeCacheMatrix(m)

#return the matrix
cacheSolve(m1)