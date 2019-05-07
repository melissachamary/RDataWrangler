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
    if(is.data.frame(dataFrame)){
    print("in if")
    lengthGeneration<-nrow(unique(dataFrame))
    key<-as.character(c(1:lengthGeneration))
    
  }else{
    lengthGeneration<-length(unique(dataFrame))
    key<-as.character(c(1:lengthGeneration))
    names(key)<-unique(dataFrame)
  }
  return(key)
}

#' generateWirdNomenclatureTable
#' generate table nomenclature from extended table when length(unique(id) != length(unique(label))
#' 
#'@param dataFrame column subset data frame on which identifiers should be generated
#'@return vector
#' @export
#'
#'
generateWirdNomenclatureTable<-function(dataFrame){
  if(is.data.frame(dataFrame)){
    print("in if")
    nomTable<-unique(dataFrame)
    names(nomTable)<-c("id","label")
    
  }else{
    lengthGeneration<-length(unique(dataFrame))
    nomTable<-data.table(id=as.character(c(1:lengthGeneration)),label=unique(dataFrame))
  }
  return(nomTable)
}
