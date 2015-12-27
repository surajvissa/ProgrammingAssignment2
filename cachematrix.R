##A memory-efficient way of potentially laborious computations through caching illustrated.
##Two functions: 'makeCacheMatrix' & 'cacheSolve' are used to compute the inverse of an input matrix.


##makeCacheMatrix() is a function list which "gets" and "sets" input as well as result values

makeCacheMatrix <- function(x = matrix())  ##An object of type 'matrix' taken as input 
     
{
     m<-NULL              ##Initialization of m used subsequently to store result values
     
     set<-function(y) ##Sets new value to x when called, and is cached. M is set to NULL for new input matrix.
     
          {
          x<<-y
          m<<-NULL
          }
     
     get<-function() x   ## Gets input matrix without requiring any additional input
     
     setinverse<-function(solve) m<<- solve  ##Caches result value  
     
     getinverse<-function()m  ##Retrieves result without requiring additional input            
     
     list(set=set, get=get,setinverse=setinverse,getinverse=getinverse) ##list of functions
}

##cacheSolve function uses makeCacheMatrix() object as input.

cacheSolve <- function(x,...) 
     
{
     m<-x$getinverse()   # Pulls inverse matrix for input
     
     if(!is.null(m))     # If result available, pulls from cache. Else, registers as NULL, and computes inverse.
     
          {
              message("getting cached data")
              
              return(m)
          }
     
     matrix<-x$get() 
     
     m<-solve(matrix)  # Matrix inverse found using solve() function
     
     x$setinverse(m)
     
     m
}


## Specimen Output for input 2X2= c(4,3,3,2)

## a<-makeCacheMatrix(matrix(c(4,3,3,2),2,2,FALSE))

## cacheSolve(a)

##       [,1]  [,2]
## [1,]   -2     3
## [2,]    3    -4
