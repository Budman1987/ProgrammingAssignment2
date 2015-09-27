

makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  ##############################################
  ##constructor for makeCacheMatrix; sets m as null and sets the x variable to that of y
  ##############################################
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##############################################
  ##Returns the value of x (used return the matrix)
  ###############################################
  get <- function() x
  
  ##############################################
  ## SEtInverse function will just assing a a
  ##value in the makeCaseMatrix enviroment
  ## user will have to issue "solve(matrix) 
  ##before setting the value ###################
  setInverse <- function(inverse) m <<- inverse
  
  ##############################################
  ## Will return whatever m has been assigned to
  ##############################################
  
  getInverse <- function() m
  
  ##############################################
  ##a list of the functions so we can call them 
  ##locally form whatever child enviroment   
  ##############################################
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
######################################################

cacheSolve <- function(x, ...) {
  ##############################################
  ##function that will return the inverse if its not null
  #################################################
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ###############################################
  ##calls get from the MakeCacheMatrix to retun the matrix
  data <- x$get()
  ##############################################
  ##calss inverse function from makeCacheMatrix object
  ##############################################
  m <- inverse(data, ...)
  #############################################
  ## calling SetInverse function to set the inverse
  ###################################################
  x$setInverse(m)
  m
}
