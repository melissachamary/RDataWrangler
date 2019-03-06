#' foreignKeyConstraint
#' check foreign key constraint and apply action defined by user
#' 
#'@param tables list of data table
#'@param fk_table name of data frame on which foreign key constraint is checked
#'@param fk_field column name on which foreign key constraint is checked
#'@param pk_table name of data frame corresponding to table checking
#'@param pk_field name of field corresponding to the foreign key check
#'@param action function name that apply certain action when contraint is violated, by default three function name ("delete","flag","delete_flag") on
#'
#' @import data.table
#' 
#' @export
#' 

foreignKeyConstraint<-function(tables, fk_field, fk_table, pk_field, pk_table, action){
  if(is.na(action)){
    message<-paste("[Foreign key check but not reported] ",sep=" ",
                   paste(fk_table,sep="$",fk_field))
    warnings(message)
  }
  dataFK<-tables[[fk_table]][[fk_field]]
  dataPK<-c(unique(tables[[pk_table]][[pk_field]]),NA)
  print(head(dataFK))
  print(head(dataPK))
  fkFail<-dataFK[which(!dataFK %in% dataPK)]
  if(length(fkFail)>0 && !is.na(action)){
    print(paste("[Foreign key check] KO",sep=" ", paste(fk_table,sep="$",fk_field)))
    setkeyv(tables[[fk_table]], c(fk_field))
    get(action)(tables, fkFail, fk_field, fk_table, pk_table,"UNSOLVED_FK")
  }else{
    print(paste("[Foreign key check] OK",sep=" ", paste(fk_table,sep="$",fk_field)))
  }
}

