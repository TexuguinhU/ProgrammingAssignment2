# makeCacheMatrix: return a list of functions to:
# 1- Set the value of the matrix
# 2- Get the value of the matrix
# 3- Set the value of the inverse
# 4- Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # 'inversed' will store the cached inverse matrix
  inversed <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  # Getter for the matrix
  get <- function() x  
  
  # Setter for the inverse
  setinv <- function(inverse) inversed <<- inverse
  
  # Getter for the inverse
  getinv <- function() inversed
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Return the inverse of a matrix.
# If the inverse is calculated (2nd+ call), returns the cached matrix.
cacheSolve <- function(x, ...) {
  # Setter the matrix 'inversed'
  inversed <- x$getinv()
  
  # If the inverse is already calculated, return it
  # 2nd+ call of the 'cacheSolve' function
  if (!is.null(inversed)) {
    message("The inverse of a matrix is already calculated.")
    message("Returning the cached matrix.")
    return(inversed)
  }
  
  # If the inverse is not calculated
  data <- x$get()
  inversed <- solve(data, ...)  
  
  # Cache the inverse
  x$setinv(inversed)  
  
  # Return the matrix
  inversed
}


# EXAMPLE USAGE
#
# Create matrix "matriz"
matriz <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3)
# Create special matrix "matriz_special"
matriz_special <- makeCacheMatrix(matriz)
# Return the matrix
matriz_special$get()
# Return the inverse
cacheSolve(matriz_special)
# Return the inverse, but that's the 2nd call, so return the inverse matrix
cacheSolve(matriz_special)

# EXAMPLE SOURCE:
#
# http://www.purplemath.com/modules/mtrxinvr2.htm
