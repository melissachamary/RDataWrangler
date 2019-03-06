#' get_sources_file
#' generate csv file and data frame compliant with wrangle.Files parameters
#'
#'@param file file_path to generate source_file
#'@return data.frame 
#'
#' 
#' @export
#' 
get_sources_file<-function(folder=NA){
  source_data<-data.frame(tableName = NA, sourcePath = NA)
  if(!is.na(folder) && folder != ""  && file_test("-d", folder)){
      write.csv(file=paste(folder,sep="/","source_data.csv"), source_data,row.names = FALSE)
  }
  return(source_data)
}

#' get_wrangler_parameter_file
#' generate csv file and data frame compliant with wrangle.Files parameters
#'
#'@param folder folder location in which csv  wrangler_parameter_file should be generated
#'@return data.frame 
#'
#' 
#' @export
get_wrangler_parameter_file<-function(folder=NA){
  wrangler_parameter<-data.frame(source_table = NA, source_field = NA, target_table= NA,target_field =NA
                          , mapper = NA , check= NA , check_fail = NA , 
                          ref_table= NA , ref_field = NA, ref_fail= NA)
  if(!is.na(folder) && folder != "" && file_test("-d", x = folder)){

      write.csv(file=paste(folder,sep="/","wrangler_parameter_file.csv"), wrangler_parameter,row.names=FALSE)
    
  }
  return(wrangler_parameter)
}

