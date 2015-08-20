# This is Week 3 Assignment to for R Programming, part of the Johns Hopkins University Data Science
# Specialization on Coursera.org

# https://www.coursera.org/specialization/jhudatascience/1?utm_medium=courseDescripTop
# https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions 
# This is an example of using cached results in a parent environment to learn about and demonstrate
# lexical scoping in R.

# In this assignment, we cache the inverse of a matrix, as Matrix inversion is usually a costly
# computation.  There may be some benefit to caching the inverse of a matrix rather than compute 
# it repeatedly.
  
# The following functions are included in this assignment:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.

# bblount - 2015-08-19

# makeCacheMatrix creates a special matrix object and can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Declare a variable to hold the matrix and th.  
  m <- NULL
  
  # Declare a variable to hold the current matrix for comparison.
  # This will be used to check if the matrix has changed since the inverse matrix
  # was calculated.
  currentMatrix <- matrix(numeric(0), 0,0) 
  
  # set the value of the matrix if a new matrix is assigned using the set function
  # and reset m as the old value of m is not needed
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function () x
  
  # set the value of the inverse of the original matrix
  setInverseMatrix <- function(solve) m <<- solve
  
  # get the value of the inverse matrix
  getInverseMatrix <- function() m
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


# cacheSolve will return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  # get the current value of m which holds the previously calculated inverse function or null if
  # the previous matrix has not been 'solved'
  m <- x$getInverseMatrix()
  
  # If the inverse matrix is not null and the passed in matrix is equal to the last matrix used,
  # then return the cached inverse matrix, else exit the if and calculate the inverse matrix for
  # the new matrix
  if (!is.null(m) && all(x$get() == currentMatrix)) {
  #if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Set the parent environment currentMatrix variable to the new matrix to have it available
  # the next time cacheSolve is called to test whether the cached value should be returned or if
  # we need to calculate the new inverse matrix
  currentMatrix <<- x$get()
  
  # create the new inverse matrix 
  m <- solve(currentMatrix, ...)
  
  # call the setter function to set the inverse matrix value for the current matrix
  x$setInverseMatrix(m)
  
  #return the value of the inverse matrix
  m
  
}
