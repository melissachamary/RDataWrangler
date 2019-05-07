library(stringr)
library(readxl)
library(data.table)

### FUNCTION DEBUG####
map_sex_concept<-function(x){
  
  return(unlist( lapply(X=x, FUN = function(X){
    f<-"DEPI0000000022"
    m<-"DEPI0000000021"
    if(is.na(X)){ return(NA) }
    else if(X == "F"){ return(f)}
    else if (X=="M"){return(m)}
    else{return(NA)}
  }))
  )
}

map_ast_interpretation<-function(x){
  return( unlist( lapply(X=x, FUN = function(X){
    r<-"DEPI0000000005"
    s<-"DEPI0000000006"
    if(is.na(X)){ return(NA) }
    else if(X){ return(r)}
    else{return(s)}
  }) ))
}

##---change--

map_by_label_old<-function(x,table){
  tLabel<-unique(table)
  return(unlist(lapply(X=x, FUN = function(x){
    return(tLabel[which(tLabel[,2] == x),1])
  })))
}

##### NEW TO TRANSFERT #####
map_by_label_default<-function(x,table,defaultValue='GLIMS-TAXO'){
  #ret<-map_by_label(x,table)
  #ret[which(is.na(ret))]<-defaultValue
  tLabel<-unique(table)
  print(tLabel[,c(names(tLabel)[2]),with=FALSE])
    a<-(unlist(lapply(X=x, FUN = function(x){
    if(length(which(tLabel[,c(names(tLabel)[2]),with=FALSE]== x)) == 0){
      return(defaultValue)
    }else{
      return(tLabel[which(tLabel[,c(names(tLabel)[2]),with=FALSE] == x),c(names(tLabel)[1]),with=FALSE])
    }
  })))
    print(head(a))
  return(a)
}
map_by_label<-function(x,table){
  
  tLabel<-unique(table[,c("id","label"),with=FALSE])
 
  map<-function(x){ if(!x%in%tLabel$label){return(NA)}
    else{return(tLabel$id[which(tLabel$label == x)])}}
  table_x<-data.table(lab=x)
  table_x<-table_x[,id:= map(lab), by=.(lab)]
  
  return(c(table_x$id))
  #X<-x
  #x<-X[10001]
  #return(unlist(lapply(X=x, FUN = function(x){
   # if(length(which(tLabel$label == x))==0){return(NA)}else{
    #  return(tLabel[which(tLabel$label == x),c("id","label"),with=FALSE])
      
  #  }
  #})))
  # return(unlist(lapply(X=x, FUN = function(x){
  #  if(length(which(tLabel[,"label"] == x))==0){return(NA)}else{
  #   return(tLabel[which(tLabel[,"label"] == x),"id"])
  #  
  #  }
  #  })))
}


#######
map_label_semSample<-function(x){
  return(map_by_label(x,as.data.frame(tables[["SEM-SAMPLE"]])[,c("id","label")]))
}
#-- unusefull
map_label_atb_hierarchy<-function(x){
  return(map_by_label(x,as.data.frame(tables[["SEM-ATB-HIERARCHY"]])[,c("id","label")]))
  
}

map_label_id<-function(x){
  print(x)
  
  return(map_by_label_default(x,as.data.frame(tables[["SEM-TAXO"]])[,c("id","label")],"GLIMS-TAXO"))
  
}

create_atb_hierarchy<-function(raw){
  famNamesH<-as.data.frame(raw$glims_drug)[,c("family","sub_family")]
  nom<-unique(c(famNamesH$family,famNamesH$sub_family))
  nomenclature.df<-data.table(id=paste("GDRUG",sep="",1:length(nom)),
                              label=unique(nom))
  atb_hierarchy<-merge(x = unique(famNamesH), by.x= c("sub_family"), y=nomenclature.df, by.y=c("label"), all.x = TRUE)
  atb_hierarchy<-merge(x = as.data.frame(atb_hierarchy), by.x= c("family"), y=nomenclature.df, by.y=c("label"), all.x = TRUE)
  names(atb_hierarchy)<-c("fam","label","id","subCOf")
  atb_hierarchy<-atb_hierarchy[,c(2,3,4)]
  toAdd<-cbind(nomenclature.df[which(!nomenclature.df$id%in%atb_hierarchy$id),], subCOf="GLIMS-ATB")
  atb_hierarchy<-rbind(toAdd,atb_hierarchy)
  atb_hierarchy$subCOf[atb_hierarchy$id == atb_hierarchy$subCOf]<-"GLIMS-ATB"
  return(as.data.table(atb_hierarchy))
  
}
#####

load(file = "/Users/mimi/Documents/HCL_work/workspace/epitrack-ontologies/HCL-data/tables.RData")

wrangler_parameter = "/Users/mimi/Documents/HCL_work/workspace/epitrack-ontologies/wraper_r_process/Process-Schema_light.xlsx"
table_list = tables

call_customFunction = data.frame(table_name="SEM-ATB-HIERARCHY",function_name="create_atb_hierarchy",stringsAsFactors = FALSE)
return_list<-wrangle.TablesList(wrangler_parameter ,table_list ,call_customFunction )
### TEST DE 1 id multiple label ####
org_DE<-unique(table_list$ward[,c("DE","DE_label"),with=FALSE])
targ_DE<-return_list$DE
### CONSTRAINT : assignation bon label au bon id ==> OK
#merge par id match label ==> FauxNeg
merge<-merge(x=org_DE,y=targ_DE, by.x=c("DE"), by.y=c("id"),all=TRUE)
all(merge$DE_label==merge$label) ### FAUX 1id multiple label
#merge par label match id ==> VraiPos
merge<-merge(x=org_DE,y=targ_DE, by.x=c("DE_label"), by.y=c("label"),all=TRUE)
all(merge$DE==merge$id) # VRAI 1 label 1 id 


### TEST CR 1label multiple id ####
cr<-table_list$ward[,c("CR","CR_label"),with=FALSE]
org_CR<-unique(table_list$ward[,c("CR","CR_label"),with=FALSE])
targ_CR<-return_list$CR
### CONSTRAINT : assignation bon label au bon id ==> OK 1 mais pas 2
#merge par id match label ==> VraiPos
merge<-merge(x=org_CR,y=targ_CR, by.x=c("CR"), by.y=c("id"),all=TRUE)
all(merge$CR_label==merge$label) ## VRAI 
#merge par label match par id ==> FauxNeg
merge<-merge(x=org_CR,y=targ_CR, by.x=c("CR_label"), by.y=c("label"),all=TRUE)
all(merge$CR==merge$id) ## FAUX 


### TEST UF fk good assignation ####
org_UF<-table_list$ward
targ_UF<-return_list$UF
pole<-return_list$POLE
  ## CR ==> OK
merge<-merge(org_UF[,c("id","CR"), with=FALSE],targ_UF[,c("id","partOf_CR"), with = FALSE], by = c("id"),all = TRUE)
merge$id[which(merge$CR!=merge$partOf_CR)]
all(merge$CR==merge$partOf_CR)
  ## DE ==> OK
merge<-merge(org_UF[,c("id","DE"), with=FALSE],targ_UF[,c("id","type_DE"), with = FALSE], by = c("id"),all = TRUE)
merge$id[which(merge$DE!=merge$type_DE)]
all(which(merge$DE==merge$type_DE))

  ## POLE ==> OK
targ_UF_pole<-merge (x=targ_UF, y=return_list$POLE, by.x=c("partOf_pole"),by.y=c("id") )
merge<-merge(org_UF[,c("id","pole"), with=FALSE],targ_UF_pole[,c("id","label.y"), with = FALSE], by = c("id"),all = TRUE)
merge$id[which(merge$CR!=merge$partOf_CR)]
all(merge$CR==merge$partOf_CR)

## HG ==> OK
targ_UF_hg<-merge (x=targ_UF, y=return_list$HG, by.x=c("partOf_hg"),by.y=c("id") )
merge<-merge(org_UF[,c("id","hospital_group"), with=FALSE],targ_UF_hg[,c("id","label.y"), with = FALSE], by = c("id"),all = TRUE)
merge$id[which(merge$CR!=merge$partOf_CR)]
all(merge$CR==merge$partOf_CR)

## ATB (SEM-ATB-HIERARCHY, SEM-ATB)
org_ATB<-table_list$glims_drug
targ_ATB_S<-return_list$`SEM-ATB`
targ_ATB_SCLASS<-return_list$`SEM-ATB-HIERARCHY`
targ_ATB_FULL<-rbind(targ_ATB_S[,c("id","label","subCOf"),with=FALSE], targ_ATB_SCLASS)
targ_ATB_FULL_EXT<-merge(x=targ_ATB_FULL, y=targ_ATB_FULL[,c("id","label"),with=FALSE], by.x=c('subCOf'), by.y=c("id"))
names(targ_ATB_FULL_EXT)[3:4]<-c("label","subCOfLabel")
## TEST LABEL SIMPLE ==> OK
merge<-merge(x=org_ATB[,c("id","label"), with=FALSE],y= targ_ATB_S[,c("id","label"),with=FALSE],
             by=c("id"), all=TRUE)
all(merge$label.x==merge$label.y)
## TEST LABEL PARENT ==> KO
merge<-merge(x=org_ATB[,c("id","sub_family"), with=FALSE],y= targ_ATB_FULL_EXT[,c("id","subCOfLabel"),with=FALSE],
             by=c("id"), all=TRUE)
all(merge$sub_family==merge$subCOfLabel) 
merge[which(merge$sub_family!=merge$subCOfLabel)]
## TEST MAP ATC
merge<-merge(x=org_ATB[,c("id","atc5"), with=FALSE],y= targ_ATB_S[,c("id","map_atc"),with=FALSE],
             by=c("id"), all=TRUE)
all(merge$family==merge$subCOfLabel) 

### TEST SEM-TAXO fk good assignation ####
org_TAXO<-table_list$taxon
org_TAXO_lab<-merge(x=org_TAXO,y=org_TAXO[,c("id","label"),with=FALSE], by.x=c("parent"), by.y=c("label"), all.x=TRUE)
names(org_TAXO_lab)[c(2,5)]<-c("id","subCOf")
targ_TAXO<-return_list$`SEM-TAXO`
## PARENT
merge<-merge(org_TAXO_lab[,c("id","subCOf"), with=FALSE],targ_TAXO[,c("id","subCOf"), with = FALSE], by = c("id"),all = TRUE)
merge[which(merge$subCOf.x!=merge$subCOf.y),] ## PARENT OK à 6 près 
merge<-merge(org_taxo[,c("id","subCOf"), with=FALSE],targ_TAXO[,c("id","subCOf"), with = FALSE], by = c("id"),all = TRUE)
merge$id[which(merge$subCOf.x!=merge$subCOf.y)]
all(merge$subCOf.x==merge$subCOf.y,na.rm = TRUE) #OK mais certains sont NA dans org_taxo et sont substitué à GLIMS-TAXO dans la target

#### TEST SAMPLE ####
org_SAMPLE<-table_list$sample
targ_SAMPLE<-return_list$`SAMPLE`
targ_SEMSAMPLE<-return_list$`SEM-SAMPLE`
targ_SAMPLE_lab<-merge(x=targ_SAMPLE,y=targ_SEMSAMPLE, by.x=c("bearearOf"), by.y=c("id"), all.x=TRUE)

### CONSTRAINT FK SEM-SAMPLE ==> OK
merge<-merge(org_SAMPLE[,c("id","sample_type"), with=FALSE],targ_SAMPLE_lab[,c("id","label"), with = FALSE], by = c("id"),all = TRUE)
merge[which(merge$sample_type!=merge$label),]  
all(merge$sample_type==merge$label)  

### CONSTRAINT FK PATIENT ==> OK
merge<-merge(org_SAMPLE[,c("id","patient"), with=FALSE],targ_SAMPLE[,c("id","fromPatient"), with = FALSE], by = c("id"),all = TRUE)
merge[which(merge$sample_type!=merge$label),]
all(merge$sample_type==merge$label)
### DATE ==> OK
merge<-merge(org_SAMPLE[,c("id","date"), with=FALSE],targ_SAMPLE[,c("id","date"), with = FALSE], by = c("id"),all = TRUE)
merge[which(merge$sample_type!=merge$label),]
all(merge$sample_type==merge$label)

#### TEST OBS-TAXO ####
org_IDENT<-table_list$isolate
targ_IDENT<-return_list$`OBS-TAXO`

org_taxon<-org_TAXO
org_isolate<-org_IDENT

targ_taxon<-targ_TAXO
targ_isolate<-targ_IDENT
### CONSTRAINT ASSIGNATION TAXONOMIQUE ==> OK (test avec le unsolved pour ceux qui étaient différent)
targ_merge<-merge(x=targ_isolate,y=targ_taxon[,c("id","label"),with=FALSE], by.x=c("hasResult"),by.y=("id"), all.x=TRUE)
names(targ_merge)[c(1)]<-"taxon"
merge<-merge(org_isolate[,c("id","taxon"), with=FALSE],targ_merge[,c("id","taxon","UNSOLVED_FK_SEM-TAXO"), with = FALSE], by = c("id"),all = TRUE)
a<-data.table(merge[which(is.na(merge$taxon.x==merge$taxon.y)),])
all(a$taxon.x==a$`UNSOLVED_FK_SEM-TAXO`)
b<-data.table(merge[which(!is.na(merge$taxon.x==merge$taxon.y)),])
all(b$taxon.x==b$taxon.y)

org_merge<-merge(x=org_isolate[,c("id","taxon"),with=FALSE],y=org_taxon[,c("id","label"),with=FALSE], by.x=c("taxon"),by.y=("id"), all.x=TRUE)
names(org_merge)[c(1)]<-"hasResult"
merge<-merge(org_merge[,c("id","hasResult"), with=FALSE],targ_isolate[,c("id","hasResult","UNSOLVED_FK_SEM-TAXO"), with = FALSE], by = c("id"),all = TRUE)
a<-data.table(merge[which(is.na(merge$hasResult.x==merge$hasResult.y)),])
all(a$hasResult.x == a$`UNSOLVED_FK_SEM-TAXO`)
b<-data.table(merge[which(!is.na(merge$hasResult.x==merge$hasResult.y)),])
all(b$hasResult.x == b$hasResult.y)

#### CONSTRAINT ASSIGNATION SAMPLE ==> OK
merge<-merge(org_isolate[,c("id","sample"), with=FALSE],targ_isolate[,c("id","observedOn"), with = FALSE], by = c("id"),all = TRUE)
all(merge$sample==merge$observedOn)

#### TEST OBS-AST ####
org_AST<-table_list$ast
targ_AST<-return_list$`OBS-AST`
#### CONTRAINTE ASSIGNATION ATB ==> OK
merge<-merge(org_AST[,c("id","glims_drug"), with=FALSE],targ_AST[,c("id","hasTest"), with = FALSE], by = c("id"),all = TRUE)
all(merge$glims_drug==merge$hasTest)
### CONTRAINTE ASSIGNATION IDENT ==> OK
merge<-merge(org_AST[,c("id","isolate"), with=FALSE],targ_AST[,c("id","refineObservation"), with = FALSE], by = c("id"),all = TRUE)
all(merge$isolate==merge$refineObservation)



### ####
target_table_name<-c("DE" , "TA",  "CR" , "HG" ,"POLE", "UF", "SEM-SAMPLE", "SEM-ATB","SEM-TAXO", "PATIENT","SAMPLE","OBS-TAXO","OBS-AST")
t<-target_table_name[1] ## OK
t<-target_table_name[2] ## OK
t<-target_table_name[3] ## OK

t<-target_table_name[4]
t<-target_table_name[5]
target_table_name<-target_table_name[1:8]
t<-"SEM-TAXO"


