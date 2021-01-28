###makeCacheMatrix - Does store the matrix and return a list object with each 
#element is function to set or get matrix and, set and get inverse of matrix.

###cacheSolve-Uses the object created when makeCacheMatrix is invoked and that 
#return object is used by cacheSolve function. if the Inverse of the specific
#object exists it gets the value from cache functionality using getinv function.
#if the object is new, it calcautes the inverse and then sets the values using
#setinv function of makeCacheMatrix object and uses as cache for further calls
#cacheSolve function is invoked for some object.

#This file can sourced using source command and this functions can be executed.
#These are all tested and is working as expected.

## makeCacheMatrix - Has formal argument where a matrix is passed in input and has
#variable inv which is set to NULL by default and later functions setm,getm,setinv,
#getinv are defined in this environment and they use lexical scoping to feth values
#of x and inv whenever needed for getm or getinv. Setinv function is used to set
#inv value of inverse of new matrix object when invoked from cacheSolve function, and setm 
#is used to set new matrix value into the same exisiting object if needed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(set_m) {
    x <<- set_m
    inv <<- NULL
  }
  getm <- function() x
  setinv <- function(set_inv) inv <<- set_inv
  getinv <- function() inv
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve - this function uses the return object from makeCacheMatrix function
# and initially tries to check if the inverse of object exists by checking for value
#getinv invoked for the object passed, and it returns cached value if it already exists.
#if th object is new and its inverse is not calculate yet,then its value is fetched
#from getm function invoked for passed object and then later inverse is calculated
#using solve function and is set to object using setinv function.
 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m_data <- x$getm()
  m_inv <- solve(m_data, ...)
  x$setinv(m_inv)
  m_inv
}
