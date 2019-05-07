#' wrangle.TablesList
#' Import data table list into R (according source_file description) and wrangle into new data table list following wrangle_parameter_file description.
#'
#'@param wrangle_parameter file of parameter data frame (see format)
#'@param sources_file name of data frame on which foreign key constraint is checked
#'@return list of data table
#'
#'@import data.table
#'@import lubridate
#'@import stringr
#'@import readxl 
#' 
#' @export
#' 
#' 
wrangle.TablesList<-function(wrangler_parameter , table_list, call_customFunction){
  if(is.list(table_list)){
    raw<-table_list
  }else{
    stop("[wrangle.TablesList] table_list argument isn't type of list")
  }
  
  wrangler<-importWranglerParameter(wrangler_parameter)
  
  if(nrow(wrangler)==0){
    stop("[wrangle.Files] empty wrangler parameters")
  }
  #--- check before wrangler execution
  source_table_name<-unique(wrangler$source_table)
  target_table_name<-unique(wrangler$target_table)
  
  if(length(which(source_table_name%in%names(raw)))>length(names(raw))){
    stop("[wrangle.Files] all source table not identified in source_file")
  }else{
    for(source in source_table_name){
      attSource<-colnames(raw[[source]])
      attWParamSource<-unique(unlist( str_split(
        wrangler$source_field[which(wrangler$source_table == source)],pattern='[|]')))
      if(length(which(!attWParamSource%in%attSource))>0){
        stop(paste("[wrangle.Files] table source",paste(source,
                                                        sep = " doesn't contains some column used in wrangler parameter ", 
                                                        attWParamSource[which(!attWParamSource%in%attSource)])))
      }
      
    }
  }
  
  #--- check implementation of necessary function
  necessaryFunction<-unlist(unique(c(wrangler[,c("mapper","check_fail","ref_fail")])))
  necessaryCheck<-unlist(lapply(necessaryFunction, FUN = function(x){
    if(is.na(x)){ return(TRUE)}else{
      return(exists(x,mode='function'))}
  }))
  stopifnot(all(necessaryCheck))

  
  #--- wrangle
  tables<-list()
  
  ##--- call universally function
  if(!is.null(call_customFunction)&& is.data.frame(call_customFunction)&& length(
    which(c("table_name","function_name")%in%colnames(call_customFunction)))==2){
    for(i in nrow(call_customFunction)){
      f_name<-call_customFunction$function_name[i]
      if(exists(x=f_name,mode='function') && !is.na(call_customFunction$table_name[i])){
        raw[[call_customFunction$table_name[i]]]<-get(call_customFunction$function_name[i])(raw)
        tables[[call_customFunction$table_name[i]]]<-get(call_customFunction$function_name[i])(raw)
        
      }else{
        print(paste("[Wrangle Pre-process] error in custom_call_data at line",i))
      }
  }
  
  }
  for(t in target_table_name){
    i<-which(wrangler$target_table == t)
    map <- wrangler[i,]
    id_field <- map$target_field[which(map$check=="as.ID")]
    if(length(id_field)==1){
      id_map<-map[which(map$check=="as.ID"),]
    }  else{
      #### split error
      if(length(id_field)==0){
        error_check<-"no"} else
        {error_check<-"multiple"}
      stop(paste("[wrangle.Files]",paste(error_check,
                                         sep=" id specified for same source table (as.ID in check field) ", t)))
    }
    
    #--- gère des mapper TODO ----
    if(is.na(id_map$source_field)) {
      # Multi-column mapper
      x   <- get(id_map$mapper)(raw[[id_map$source_table]])
    } else {
      # Direct mapper
      fieldSel<-unlist(str_split(id_map$source_field,pattern="[|]"))
      if(length(fieldSel)>1){
        x   <- get(id_map$mapper)(raw[[id_map$source_table]][,fieldSel, with=FALSE])
        tables[[id_map$target_table]] <- x
      }else{
        x   <- get(id_map$mapper)(raw[[id_map$source_table]][[id_map$source_field]])
        tables[[id_map$target_table]] <- data.table(id = x)
        setnames(tables[[id_map$target_table]],c(id_field))
      }
    }
   
    for(att in which(map$target_field != id_field)) {
      att_map<-map[att,]
     
      if(is.na(att_map$source_field)) {
        # Multi-column mapper
        x   <- get(att_map$mapper)(raw[[att_map$source_table]])#[ft])
      } else if(!att_map$source_field%in% colnames(raw[[att_map$source_table]])){
        atts<-unlist(str_split(att_map$source_field,pattern="|"))
        x   <- get(att_map$mapper)(raw[[att_map$source_table]][[atts]])#[ft])
      }else if(!is.na(att_map$ref_table)){
        # Direct mapper #
        x   <- get(att_map$mapper)(x=raw[[att_map$source_table]][[att_map$source_field]]#[ft]
        ,table = tables[[att_map$ref_table]])
      }else{
        #### Nomenclature table management
        x   <- get(att_map$mapper)(raw[[att_map$source_table]][[att_map$source_field]])#[ft])
      }
      
      if(length(x)>dim(tables[[att_map$target_table]])[1]){
        tables[[att_map$target_table]][, c(att_map$target_field) := unique(x)]
      }else{
        tables[[att_map$target_table]][, c(att_map$target_field) := x]
      }
      if(!is.na(att_map$ref_field)&& !is.na(att_map$ref_table)){
        foreignKeyConstraint(tables,att_map$target_field, att_map$target_table, att_map$ref_field, att_map$ref_table, att_map$`ref_fail`)
      }else if (!is.na(att_map$ref_field) || !is.na(att_map$ref_table)) {
        warnings(paste("[Foreign constraint missing required data]",sep=" ", 
                       paste(att_map$target_table,sep="$",att_map$target_field)))
      }else{
        print(paste("[No foreign constraint]",sep=" ", 
                    paste(att_map$target_table,sep="$",att_map$target_field)))
      }
    }
    
    
  }
  print("##### End #####")

  return(tables)
}


#' wrangle.Files
#' Import data table list into R (according source_file description) and wrangle into new data table list following wrangle_parameter_file description.
#'
#'@param wrangle_parameter file of parameter data frame (see format)
#'@param sources_file name of data frame on which foreign key constraint is checked
#'@return list of data table
#'
#'@import data.table
#'@import lubridate
#'@import stringr
#'@import readxl 
#' 
#' @export
#' 
#' 
wrangle.Files<-function(wrangler_parameter , sources_file, call_customFunction){
  raw<-importTableFromSource(sources_file)
  tables<- wrangle.TablesList(wrangler_parameter , table_list=raw, call_customFunction)
  return(tables)
}

#' importTableFromSource
#' Import data table list into R
#'
#'@param sources_file parameter file to source data (see source_format)
#'@return table list
#'@import data.table
#'@import readxl
#' 
#' 
#' @export
#' 
importTableFromSource<-function(parameter, useAll = FALSE){
   source_data<-read.csv(parameter,stringsAsFactors = FALSE)
   source_data$sourcePath<-str_squish(source_data$sourcePath)
   print(source_data$sourcePath)
   col_source<-colnames(get_sources_file())
   if(length(which(colnames(source_data)%in%col_source))<length(col_source)){
     stop("[importTableFromSource] bad source file format")
   }
   files<-source_data$sourcePath[which(file_test("-f",source_data$sourcePath))]
   raw_data<-vector("list", length(unique(source_data$tableName)))
   names(raw_data)<-unique(source_data$tableName)
   if(useAll && length(files)<nrow(source_data)){
     stop("[importTableFromSource]  file missing error ", sep=" : ",
          source_data$sourcePath[!which(file_test("-f",source_data$sourcePath))])
   }
   else if(length(files)<nrow(source_data) ){
     warning("[importTableFromSource] file missing warning, some following table will not be filled in return list()", sep=" : ",
          source_data$tableName[!which(file_test("-f",source_data$sourcePath))])
   }
   for(f in files){
     tableName<-source_data$tableName[which(source_data$sourcePath == f)]
     extension<-str_extract(f, "[.][a-zA-Z]*$")
     if(extension == ".csv"){
       data<-read.csv(f)
     }else if(extension %in% c(".xls",".xlsx")){
       data<-as.data.frame(read_excel(  f))
     }else{
       warning(paste("[importTableFromSource] unreconized extension",
                     paste(extension, sep=" for file ",f)))
     }
      if(is.null(raw_data[[tableName]])){
        raw_data[[tableName]]<-data
      }else if(is.data.frame(raw_data[[tableName]])) {
        colnamesRD<-colnames(raw_data[[tableName]])
        if(length(which(colnames(data)%in%colnamesRD)) == length(data)){
          raw_data[[tableName]]<-rbind(raw_data[[tableName]], data)
        }else{
          raw_data[[tableName]]<-rbind(raw_data[[tableName]], data[,which(colnames(data)%in%colnamesRD)])
          warning(paste("[importTableFromSource] data from ",
                        paste(f, sep=" limited extraction due to column missing in previous file used to fill same table. \n Column insert limitation to :  ",colnamesRD))) 
       }
      }
   }
   raw_data<-lapply(raw_data, FUN = function(x){
     return(as.data.table(x))
   })
   return(raw_data)
}


#' importWranglerParameter
#' Get wrangler parameter data from source file
#'
#'@param wrangler_parameter wrangler parameter file to transform data (excel or csv-comma)
#'@return wrangler data frame
#'@import readxl
#' 
#' 
#' @export
#' 
importWranglerParameter<-function(wrangler_parameter){
  wrangler<-data.frame()
  if(! file_test("-f",wrangler_parameter)){
    stop(paste("[importWranglerParameter] wrangler parameter file doesn't exist",wrangler_parameter))
  }
  extension<-str_extract(wrangler_parameter, "[.][a-zA-Z]*$")
  if(extension == ".csv"){
    wrangler<-read.csv(wrangler_parameter)
  }else if(extension %in% c(".xls",".xlsx")){
    wrangler<-as.data.frame(read_excel(wrangler_parameter),stringsAsFactors=FALSE)
  }else{
    warning(paste("[importWranglerParameter] unreconized extension",
                  paste(extension, sep=" for file ",wrangler_parameter)))
  }  
  col_wrangler<-colnames(get_wrangler_parameter_file())
  if(length(which(colnames(wrangler)%in%col_wrangler))<length(col_wrangler)){
    stop("[importWranglerParameter] bad source file format")
  }
  return(wrangler)
}

