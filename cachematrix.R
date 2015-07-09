## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## The function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix = function(x = matrix()) 
{
  m = NULL
  
  setMatrix = function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  getMatrix = function()
  {
    x
  }  
  
  setInverseMatrix = function(im)
  {
    m <<- im
  }
  
  getInverseMatrix = function()
  {
    m
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve = function(x)
{
  ## Return a matrix that is the inverse of 'x'
  
  # Get cached inverse matrix if it exists
  im = x$getInverseMatrix()
  
  # Check if inverse matrix exists
  if(!is.null(im))
  {
    # Return cached inverse matrix
    return(im)
  }
  
  # Calculate inverse matrix
  m = x$getMatrix()
  im = solve(m)
  x$setInverseMatrix(im)
  
  # Return calculated inverse matrix
  im
}
