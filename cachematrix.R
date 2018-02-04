makeCacheMatrix <- function(i = matrix()) {
  inva = NULL
  setvalue = function(j) {
      i <<- j
    inva <<- NULL
  }
  getvalue = function() i
  setinva = function(inverse) inva <<- inverse 
  getinva = function() inva
  list(setvalue=setvalue, getvalue=getvalue, setinva=setinva, getinva=getinva)
}

cacheSolve <- function(i, ...) {
  
  inva = i$getinva()
  #retrieving cache value of the inverse of matrix
  if (!is.null(inva)){
      message("getting cached data")
    return(inva)
  }
  
   indata = i$getvalue()
  ##calculating inverse of the matrix if it is not already in cache
  inva = solve(indata)
  i$setinva(inva)
   return(inva)
}

m <- matrix(rnorm(9),3,3)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)