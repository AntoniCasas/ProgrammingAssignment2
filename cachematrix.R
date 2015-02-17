## The following function is a function factory, basically you pass a matrix (variable 'x') and it will return 
### a list of functions that you can use on that matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL #you set variable 'i' to NULL. This variable will be used to store the inverse matrix in the functions below
  set <- function(y) {
        x <<- y
        i <<- NULL
  } #this function assigns the value you pass to the variable 'x', and also sets 'i' to NULL (both 
    #outside the current scope, in the parent environment)
  
  get <- function() x #this function simply returns the value of variable 'x'
  setinverse <- function(inverse) i <<- inverse #this function allows you to set 'i' to the inverse matrix
  getinverse <- function() i #this function returns the value 'i'(i.e. the inverse matrix)
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #the list of functions returned
}


## This function will only calculate the inverse matrix of a particular matrix if it's not been calculated before
## It will only work on an object returned from makeCacheMatrix because the object getinverse is only defined there
## The inverse matrix is cached 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # It stores into 'i' the inverse matrix from the object of type makeCacheMatrix you passed 
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  } # if 'i' has already been calculated or set, it returns this inverse matrix, prints an alert about it and exits
  data <- x$get() # we get the original matrix 'x'
  i <- solve(data,...) # we calculate the inverse of the original matrix and we store it into 'i'
  x$setinverse(i) # we set the inverse matrix into the makeCacheMatrix object
  i #it returns the inverse matrix 'i'
}