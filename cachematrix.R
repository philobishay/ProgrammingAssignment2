## Put comments here that give an overall description of what your
## functions do
# The task of calculating matrix inverses is computationally intensive, and
# thus, a new matrix-like object is created using function "makeCacheMatrix".
# This function returns a list of functions and may store the computed
# inverse in its environment. A second function, "cacheSolve" reads the 
# lists created by "makeCacheMatrix" and computes the inverse, but only
# if it has not already been stored in the object


## Write a short comment describing this function
# makeCacheMatrix is a constructor function that takes a matrix as an argument 
# and creates a matrix-like object which is technically a list of functions. 
# The input matrix is assigned it to the name "nums" and if computed, the inverse
# is assigned to "inv".
# NOTE: "nums" and "inv" live in the makeCaucheMatrix environment. 
# This can be observed using ls(environment(myMatrix$get.inverse)), where "myMatrix"
# was constructed using the makeCacheMatrix function.
# The function outputs the following functions as a "list" objects:
# 1. set: this function assigns an input matrix to "nums"
# 2. get: this function retrieves the input matrix, "nums"
# 3. set.inverse: assigns the argument "inverse" to the variable "inv"
# 4. get.inverse: retrieves the variable "inv"


makeCacheMatrix <- function(nums = matrix()) {
  inverse <- NULL
  set <- function(y) {
    nums <<- y
    inverse <<- NULL
  }
  get <- function() nums
  set.inverse <- function(inv) inverse <<- inv
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## Write a short comment describing this function
# cacheSolve reads the constructed matrix-like object and checks whether
# an inverse has been computed. If yes, it returns the cached inverse and prints
# "getting cached data".
# If not, it computes the inverse and passes it back to the previously constructed
# matrix-like object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached.inverse <- x$get.inverse()
  if(!is.null(cached.inverse)) {
    message("getting cached data")
    return(cached.inverse)
  }
  data <- x$get()
  cached.inverse <- solve(data, ...)
  x$set.inverse(cached.inverse)
  cached.inverse
}

# Example
# 
# A = matrix(c(2,2,3,2),2,2)
# myMatrix = makeCacheMatrix(A)
# cacheSolve(myMatrix)
# cacheSolve(myMatrix)
