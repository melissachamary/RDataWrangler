#' as.ID
#' specify unique primary key in table, and check uniqueness
#' 
#'@param dataVector value vector
#'@return boolean
#' @export
#' 
#' 
as.ID<-function(dataVector){
  return(uniqueness(dataVector))
}
#' uniqueness
#' check unicity of value in vector
#' 
#'@param dataVector value vector
#'@return boolean
#' @export
#' 
#'
uniqueness<-function(dataVector){
  return(length(dataVector)== length(unique(dataVector)))
}
#' boolean
#' check if vector values are boolean
#' 
#'@param dataVector value vector
#'@return boolean
#' @export
#' 
#'
boolean<-function(dataVector){
  return(is.logical(dataVector))
}

#' positive
#' check data are strictly positive (>0)
#' 
#'@param dataVector value vector
#'@return boolean
#' @export
#' 
#'
positive<-function(dataVector){
  return(length(which(dataVector<0))==0)
}

#' generatedKey
#' generate character key on specified dataframe subset
#' 
#'@param dataFrame column subset data frame on which identifiers should be generated
#'@return vector
#' @export
#'
#'
generatedKey<-function(dataFrame){
  print(is.data.frame(dataFrame))
  print(typeof(dataFrame))
  if(is.data.frame(dataFrame)){
    
    lengthGeneration<-nrow(unique(dataFrame))
  }else{
    print(dataFrame)
    lengthGeneration<-length(unique(dataFrame))
  }
  return(as.character(c(1:lengthGeneration)))
}
