##############         Programming Assignment 2: Lexical Scoping

###NAME: Nathaly Lizeth Cruz Tipantiza
###Date: 14 -10 -2017

#Assignment: Caching the Inverse of a Matrix
#Write the following functions:
# -->makeCacheMatrix

#If the inverse has already been calculated (and the matrix has not changed),
#then cacheSolve should retrieve the inverse from the cache.

###-----Structure ---#


## Create the function "makeCacheMatrix"--> you can use this function to obtain a "matrix" object
#                                       than can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinversa <- function(inverse) inversa <<- inverse
        getinversa <- function() inversa
        list(set = set,get = get,
             setinversa = setinversa,
             getinversa = getinversa)
}


##Create the function "cacheSolve"--> you can use this function to obtain directly
#                                       the inverse of the special "matrix", obtained before
#                                       by the function "makeCacheMatrix" .
cacheSolve<- function(x, ...){
        inversa<-x$getinversa()
        if (!is.null(inversa)){
                return(inversa)
        }
        datos<- x$get()
        inversa<-solve(datos, ...) 
        inversa
}
        

####-- Checking the functions created with an example---
       
##First create a matrix with any values you want (remember that the matrix that you
# are going to create its necessary that it have an inverse.)
example_matrix <- matrix(rnorm(9),3,3)
example_matrix
#View the data of the matrix created: example_matrix
#       [,1]       [,2]       [,3]
#[1,] 0.9612636  1.7486651 -0.2011690
#[2,] 0.4480820 -0.8547137  0.4067372
#[3,] 0.1016493  0.2924009 -1.6314014

matrix1<- makeCacheMatrix(example_matrix)  
cacheSolve(matrix1)
 #View of the data: inverse of example_matrix
#       [,1]        [,2]       [,3]
#[1,] 0.50357654  1.10311510  0.2129298
#[2,] 0.30493956 -0.61108954 -0.1899577
#[3,] 0.08603204 -0.04079454 -0.6337494

