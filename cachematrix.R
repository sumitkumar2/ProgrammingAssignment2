## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## the function makeCacheMatrix has four method.  methods  deal with matrix x, store inverse matrix I and cache it. 
## By calling the set function the inverse matrix I is erased and after calculation replace by new  value.
makeCacheMatrix <- function(x = matrix()){
  
  
  I <- NULL              ## init inverse property
  
  
  set <- function(m){    ## setting matrix x
    x <<- m
    I <<- NULL
  }
  
 
  get <- function(){     ## get matrix x
    x               
  } 
  
  
  setInv <- function(i) {  ## set the inverse matrix of x
    I <<- i
  }
  
 
  getInv <- function() {     ## get the inverse matrix of x
    I
  }
  
     ## list of internal methods
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## function calculate a inversion matrix of the 'special' matrix CM. The matrix CM is returned by function makeCacheMatrix().
## If the inverse matrix CM has not been calculated yet or changed, then the inverse matrix IM of the matrix CM is calculated and set 
## and returned as the result. If the matrix CM has been already calculated then it's retrived from the cached valued
cacheSolve <- function( x, ...){
  ## Return a matrix that is the inverse of 'x'
  
  
  IM <- x$getInv()      ## inversion matrix IM of the matrix x
  
 
  if (is.null(IM)) {
    message('Calculation the inverse...')
    
    data <- x$get()
    IM <- solve(data, ...)
    x$setInv(IM)
  } else {
    message('Cached inverse...')
  }
  
  IM
}