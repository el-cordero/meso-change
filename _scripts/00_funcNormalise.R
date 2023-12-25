# Function

# normalize vector to a scale of 0 to 1
normalise <- function(x){
  y = (x - min(x,na.rm=TRUE))/ (max(x,na.rm=TRUE) - min(x,na.rm=TRUE))
  return(y)
}