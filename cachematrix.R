#Function makeCacheMatrix(x=matrix())
#this function takes in an initial matrix and stores it localy to its envrioment. it will do simple check to ensure that the supplied information is a matrix 
#This has the following helper functions:
#  getMatrix -- returns the matrix
#  ResetMatrix -- change the contents of the stroed matrix and resets the inverse store as well, checks to ensure that the value is a matrix before setting
#  SetMatrixInv  -- sets the inverse of the matrix.  checks to ensure that the value is a matrix and tries to ensure it is the inverse of the stored matrix
#  GetMatrixInv -- returns the inverse of the matrix
#  getIdent -- gets the indentiy matrix of the matrix
#  chkSqr -- returns true if the matrix is a square matrix


## Make cached matrix to cache a copy of a matrix in memory with functions to do:
# "For this assignment, assume that the matrix supplied is always invertible."


makeCacheMatrix <- function(x = matrix()) {

#Check if supplied value is a matrix or not, if not print error and return 1
  if(!is.matrix(x)) {
    print("The value is not a matrix, please try agian")
    return(invisible(1))
  }
  #setting initial value of cacheset value to null so systems till be ready to caclue new cache
  invCache<-NULL
  
  #this resets the current matrix into a new matrix value, storing its value in the lcoal variable and resets the cacheset variable
  ResetMatrix <- function(NewMatrixValue) {
    if(!is.matrix(NewMatrixValue)) {
      print("The value is not a matrix, please try agian")
      return(invisible(x))
    }
    x <<- NewMatrixValue  
    invCache <<- NULL
  }
  
  #retrive the cached matrix from memory
  getMatrix <- function(){
    x
  }
  
  
  ###########################################################
  #getting and setting matrix inversions
  
  
  SetMatrixInv <- function(InvMatrixVal){
    
    if(!is.matrix(InvMatrixVal)) {
      print("The value is not a matrix, please try agian")
      return(invisible(1))
    }
    
    
    if(!identical(diag(nrow=nrow(InvMatrixVal)), round(InvMatrixVal %*% solve(InvMatrixVal),digits = 0))) {
      print("supplied matrix is not the inverse, please use cacheSolve() function to set this value")
    }
    
    invCache <<- InvMatrixVal 
  }
  GetMatrixInv <- function(){
    invCache
  }
  
  #bonus stuffs - Check if the matrix returns an identiy matrix
  
  getIdent <- function(){
    round(x %*% solve(x),digits = 0)
  }
  
  #Check if the matrix is square
  chkSqr <- function() {
    matrix_identy<- round(x %*% solve(x, digits=0))
    chk1 <-sum(sapply(matrix_identy,sum))
    if(chk1== nrow(x) & chk1 == ncol(x)){
      print("the sum of the sum of the matrix is equal to the row length / column length")
      return(TRUE)
    }
    else{
      print("Not a square matrix")
      return(FALSE)
    }
      
  }
  
  list(ResetMatrix = ResetMatrix, 
       getMatrix = getMatrix,
       GetMatrixInv = GetMatrixInv,
       SetMatrixInv = SetMatrixInv,
       getIdent = getIdent,
       chkSqr = chkSqr)
}


## The cacheSolve fucntion, gets the inverse of teh matrix.  Setting times for when it starts and finished
## this fuction is to take the matrix that was generated with makecachedmatrix.  
## it will use the $getmatrix and $setmatrixinv functions of the object passed to it to get the matrix to do the inversion on as well
## to set the cache of the inversion of the matrix.  
cacheSolve <- function(x, ...) {
  ##checks if the matrix cache is already present / is not null.  If it is, returns the cache version of the inverse
  if(!is.null(x$GetMatrixInv())) {
    print("using cached version")
    return(x$GetMatrixInv())
  }
  ##comments to user that it is starting calcuations, storing the variable and when it finally completes.  
  
  print(paste0("Calcucating new matrix inversaion. started at ", Sys.time()))
  Inv <- solve(x$getMatrix())
  print(paste0("work complete, saving in Matrix ", Sys.time()))
  x$SetMatrixInv(Inv)
  print(paste0("Work completed at ", Sys.time()))
  Inv
  }