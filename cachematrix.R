## matrix is entered, checked for equivalency with the stored inverse matrix value and if none is found than the inverse function is calculated

## takes in matrix and stores it for use in other functions

makeCacheMatrix <- function(x = matrix()) {
  z<-dim(x)                       #takes the dimensions of the matrix
  m<-matrix(nrow=z[1],ncol=z[2])  #creates a matrix of the same dimensions as the input so that comparison in the cache function doesn't spit back an error
  if(z[1]!=z[2]){                 #checks whether the matrix is invertible
    print("please enter a invertible matrix") #if it isn't invertible you are asked to enter one that is
  }
  set<-function(y){
    x<<-y    #sets x to the value of y
    z<-dim(x)#takes dimensions of x 
    m<<-matrix(nrow=z[1],ncol=z[2])#creates a matrix with the same dimensions so that they can be directly compared for equivalency
  }
  get<-function()x #get function returns value of x
  setinverse<-function(inverse) m<<-inverse #m is given the value of inverse, inverse is entered to the function from the cache
  getinverse<-function() m #returns matrix value m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #define names for arguments variables
}

## calculates the inverse of the matrix unless the matrix is already inverted

cacheSolve <- function(x, ...) {
  m<-x$getinverse()#calls the current value for m
  d<-x$get()#calls for the data values
  #print(d)
  if(isTRUE(all.equal(m,d))==T){#checks for equivalencies between matrices
    return(m)#if the matrices are equivalent than m is returned with it's current value
  }
  m<-solve(d)#creates inverse value of the data and enters it into variable m
  #print(m)
  x$setinverse(m)#m is then stored in the setinverse argument
  m #returns the inverse matrix
  
}
