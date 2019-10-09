knapsack_dynamic<-function(x, W){
  if(!is.data.frame(x)) stop("First argumet should be a data frame!")
  else
  {if(!(grep("w",names(x))&grep("v",names(x)))) stop("There should be columns "w" and "v" in the data frame!")
    if(length(grep(TRUE,x<0))>0) stop("There is negative value in data frame!")}
  if(!(is.numeric(W))&(W>0)) stop("The second argument should be positive number!")
  
  
  
  
}