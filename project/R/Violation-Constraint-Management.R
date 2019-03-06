#' delete
#' Delete data at indice when foreing key constraint were violated
#'
#'@param tables list of data table
#'@param fk_table name of data frame on which foreign key constraint is checked
#'@param fk_field column name on which foreign key constraint is checked
#'@param pk_table name of data frame corresponding to table checking
#'@param namePrefix prefix flag column when flag like function is called (unused in this f)
#'@param indice row indice in fk_table
#'
#' @import data.table
#' 
#' @export
#' 

delete<-function(tables,indice,fk_field, fk_table, pk_table, namePrefix){
  flagValue <- c(tables[[fk_table]][[fk_field]])
  flagValue[which(flagValue%in%indice)]<-NA
  tables[[fk_table]][,c(fk_field):= flagValue]
}
#' flag
#' flag violated constraint data into new annotation column in fk_table (named <prefixName>_<pk_table>)
#'
#'@param tables list of data table
#'@param fk_table name of data frame on which foreign key constraint is checked
#'@param fk_field column name on which foreign key constraint is checked
#'@param pk_table name of data frame corresponding to table checking
#'@param namePrefix prefix flag column when flag like function is called (unused in this f)
#'@param indice row indice in fk_table
#'
#' @import data.table
#' 
#' @export
#' 

flag<-function(tables, indice, fk_field, fk_table, pk_table, namePrefix){
  flagName<-paste(namePrefix,sep="_",paste(pk_table))
  flagValue <- c(tables[[fk_table]][[fk_field]])
  flagValue[which(!flagValue%in%indice)]<-NA
  print(length(flagValue)==dim(tables[[fk_table]])[1])
  
  tables[[fk_table]][,c(flagName):= flagValue]
}

#' delete_flag
#' delete data which violated constraint in fk_field and flag violated it into new annotation column in fk_table (named <prefixName>_<pk_table>)
#'
#'@param tables list of data table
#'@param fk_table name of data frame on which foreign key constraint is checked
#'@param fk_field column name on which foreign key constraint is checked
#'@param pk_table name of data frame corresponding to table checking
#'@param namePrefix prefix flag column when flag like function is called (unused in this f)
#'@param indice row indice in fk_table
#'
#' @import data.table
#' 
#' @export
#' 

delete_flag<-function(tables,indice,fk_field, fk_table, pk_table, namePrefix){
  flag(tables,indice,fk_field, fk_table, pk_table, namePrefix)
  delete(tables,indice,fk_field, fk_table, pk_table, namePrefix)
}
