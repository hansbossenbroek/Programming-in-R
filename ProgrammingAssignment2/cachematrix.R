## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This is the 'constructor'-function of the cached matrix solver.
## Essentially, it creates a "matrix" object that can later be populated with a 'get'
## and a 'set' function to retrieve the matrix in cache.
makeCacheMatrix <- function(x = matrix()) {
  # The local variables to store the matrix and it's solved variant
  localSolvedMatrix<-NULL
  localOriginalMatrix<-NULL
  
  # This function is used to store and retrieve the original matrix.
  set<-function(y) {
    localOriginalMatrix<<-y
    localSolvedMatrix<<-NULL
  }
  get<-function() {
    localOriginalMatrix
  }
  
  # This function stores a solved matrix in the local variable (i.e. cache)
  setmatrix<-function(solvedMatrix) {
    localSolvedMatrix<<- solvedMatrix
  }
  
  # The function returns the solved matrix in cache (if any)
  getmatrix<-function() {
    localSolvedMatrix
  }
  
  # List with functions.
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  # First get the matrix from the local variable
  tempMatrix<-x$getmatrix()

  # Check if it has been initialized with a value.
  if(!is.null(tempMatrix)){
      message("getting cached data")
      return(tempMatrix)
  }

  # If not, solve this matrix and store it in the local variable above.
  matrix<-x$get()
  tempMatrix<-solve(matrix, ...)
  x$setmatrix(tempMatrix)
  tempMatrix
}