#' brute_force_knapsack
#' 
#' Given data frame \code{x}, max weight \code{W}, give the best solution with brutal force of O(2^n)
#' 
#' @param x, a data frame of items with different values and weights
#' @param W, the capacity of knapsack
#' @return a list with best value and selected elements 
#' @examples
#' set.seed(42)
#' n<- 1000000
#' knapsack_objects <-data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#'  v=runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 

brute_force_knapsack<-function(x, W){
if(!is.data.frame(x)) stop("First argumet should be a data frame!")
else
  {if(!(grep("w",names(x))&grep("v",names(x)))) stop("There should be columns 'w' and 'v' in the data frame!")
   if(length(grep(TRUE,x<0))>0) stop("There is negative value in data frame!")}
if(!((is.numeric(W))&(W>0))) stop("The second argument should be positive number!")
  
  library(binaryLogic)
  
  n<-nrow(x)
  zhi<-0;yuan<-NULL
  len<-2^n-1
  for(i in 1:len)
  {te<-as.logical(as.binary(i,n=n))
   if(sum(x$w[which(te==TRUE)])>W) next
   else if(sum(x$v[which(te==TRUE)])>zhi)
    {zhi<-sum(x$v[which(te==TRUE)])
     yuan<-which(te==TRUE)}}
   
  return (list(value=zhi,elements=yuan))
}