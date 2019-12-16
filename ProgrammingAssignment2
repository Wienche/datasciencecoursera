           # Week 3 PeerGraded Assignment.

                  # Set working directory.

setwd("~/Desktop/R/Course2")

# First of all. What IS the inverse matrix? 
                         # Create a matrix.

TestData <- matrix(c(1,2,3,0,1,5,5,6,0),nrow=3,  byrow = TRUE)

                             # Show matrix.
View(TestData)

                  # Compute inverse matrix.
InverseMatrix <- solve(TestData)

                     # Show inverse matrix.
View(InverseMatrix)

                                                     # Next step is to do the same for through the cache. 

makeCacheMatrix <- function(x = matrix()) {    ## this line defines the main function and its sarguments. 
    inv <- NULL                ## this is to make the object in which the inversed matrix will be stored. 
    set <- function(y) {                       ## this line defines the main function and its sarguments.  
      x <<- y                                        ## here y is connected to x through superassignment.
      inv <<- NULL                                        ## if there is a new matrix, reset inv to NULL.
    }
    get <- function() x                ## define the get fucntion - returns value of the matrix argument.
    
    setinverse <- function(inverse) {                      ## assigns value of inv in parent environment.
      inv <<- inverse
      } 
    getinverse <- function() {                                     ## gets the value of inv where called.
      inv
      } 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                 ## you need this in order to refer to the functions with the $ operator.
  }
  
                                                   # This will run the function above with the test data. 
ResultCache <- makeCacheMatrix(TestData)  

                          # The result is a list of four functions: set, get, setinverse and getinverse. 
                                   # The point is that x and inv are stored in the enclosing environment 
                                                     # of the set, get, setInverse, getInverse functions

                 # Next up is to make the function that will retrieve the inverse matrix from the cache. 

cacheSolve <- function(x, ...) {                                             # Return an inverse matrix. 
    inv <- x$getinverse()
    if(!is.null(inv)) {                        
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)                              # Solve is the actual inverting of the matrix. 
    x$setinverse(inv)
    inv
  }

R_InverseMatrix <- cacheSolve(makeCacheMatrix(TestData))

 # Last step is to check if the inverse matrix that was created directly. 
         # is the same as the function that was created through the cach. 

Check <- InverseMatrix == R_InverseMatrix

                                                                 # It is. 

