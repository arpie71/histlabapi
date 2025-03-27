
'%ni%'<-Negate('%in%')

ck_fields<-function(fields){
  #' This function checks to make sure that the fields listed are correct
  #' And it gets the field list ready to send to API endpoint
  #' @param fields List of fields to check
  #' Note: right now fields are hard-coded but will be moved to configuration file

  field.list<-c("authored","body","body_html","body_summary","chapt_title","countries","collection","date","date_year","date_month","from_field","doc_id","location","nuclear","persons","topics","classification","refs","cable_references","source","source_path","cable_type","subject","title","to_field","tags","description","category","pdf","title_docview","orighand","concepts","type","office","readability","persons","countries","person_ids")
  n<-NULL
  for(f in fields){
    if(f %ni% field.list){
      if(is.null(n)) n<-f else n<-paste(n,f, sep=',')
    }
  }
  notice<-NULL
  if(!is.null(n)){
    if(grepl(",",n)) verb<-"are" else verb<-"is"
    notice<-paste("Field Error:", n,verb,"not valid")
  }
  return(notice)
}

ck_list<-function(x) {
  #' This function will ensure a valid format for options that allow multiple values such as collections, ids, fields
  #' @param x Object to check
  #'
  if(length(x)==1){
    x<-gsub(',\\s+',',',x)
    x<-gsub('\\s+',',',x)
    x<-as.list(strsplit(x,",")[[1]])
  }
  return(x)
}


ck_collections<-function(collections){
  #' This function checks to make sure that the collections listed are correct
  #' @param collections List of collections to check
  #' Note: right now collections are hard-coded but will be moved to configuration file

  collection.list<-c("cpdoc","clinton","kissinger","cfpf","frus","ddrs","cabinet","briefing","worldbank")
  n<-NULL
  for(c in collections){
    if(c %ni% collection.list){
      if(is.null(n)) n<-c else n<-paste(n,c, sep=',')
    }
  }
  notice<-NULL
  if(!is.null(n)){
    if(grepl(",",n)) verb<-"are" else verb<-"is"
    notice<-paste("Collection Error:", n,verb,"not valid")
  }
  return(notice)
}

ck_date<-function(dates){
  #' This function checks that dates are correctly formatted
  #' And it converts dates to a standard format
  #' @param dates List of dates to check
  notice<-NULL
  #convert date to Y-M-D format
  d<-strsplit(dates,"(/|-|\\.)")
  if(length(d[[1]])!=3) notice<-"Please supply a month, day, and year separated by /,-, or ."
  if(length(d[[1]])==3) {
    yr<-mo<-day<-NULL
    if(nchar(d[[1]][1])==4){
      yr<-d[[1]][1]
      if(d[[1]][2]>12) {
        day<-d[[1]][2]
        mo<-d[[1]][3]
      } else{
        day<-d[[1]][3]
        mo<-d[[1]][2]
      }
    } else if(nchar(d[[1]][3])==4){
      yr<-d[[1]][3]
      if(d[[1]][1]>12) {
        day<-d[[1]][1]
        mo<-d[[1]][2]
      } else {
        day<-d[[1]][2]
        mo<-d[[1]][1]
      }
    } else if(nchar(d[[1]][2])==4){
      yr<-d[[1]][2]
      if(d[[1]][1]>12) {
        day<-d[[1]][1]
        mo<-d[[1]][3]
      } else{
        day<-d[[1]][3]
        mo<-d[[1]][1]
      }
    } else {
      if(is.null(notice)) notice <- "Year must be a 4-digit number" else notice<-paste0(notice,"\nYear must be a 4-digit number")
    }
    date1<-paste(yr,mo,day,sep='-')
    #check if date is valid
    d1<-try(as.Date(paste(yr,mo,day,sep='-'),date.format="%y-%m-%d"), silent=TRUE)
    if(!class(d1)=="Date"){
      notice<-paste0(dates," is not a valid date")
    }
  }
  if(!is.null(notice)) stop(notice, call.=FALSE)
  if(nchar(mo)<2) mo<-paste0("0",mo)
  if(nchar(day)<2) day<-paste0("0",day)
  date1<-paste(yr,mo,day,sep='-')
  return(date1)
}

hlresults<-function(url){
  #' This function sends a request to the HL API
  #' @param url API URL address to call

  u<-try(jsonlite::fromJSON(url), silent=TRUE)
  if("try-error" %in% class(u)) stop("Problem with query")
  if(!length(u)) stop("No results found", call.=FALSE)
  return(u)
}


ck_entities<-function(url,entity.type) {
  #' This function will makes sure a valid entity is given
  #' @param url URL to pass back with entity.type info added
  #' @param entity.type Object to check
  #'
  for(i in 1:length(entity.type)){
    if(entity.type[i] %ni% c("countries", "persons", "topics")){
      notice <- "Acceptable entities are countries, topics, and/or persons"
      stop(notice, call.=FALSE)
    }
    if(entity.type[i]=="topics"){
      url<-paste(url,"topics.topic_name", sep=',')
    }
    if(entity.type[i]=="countries"){
      url<-paste(url,"countries.country_name", sep=',')
    }
    if(entity.type[i]=="persons"){
      url<-paste(url,"persons.full_name", sep=',')
    }

  }
  return(url)
}

configsearch<-function(s.text, or){
  #' This function configures the search text to make it compatible with the API
  #' @param s.text The text or list of texts to search
  #' @param or Whether to join text with 'or' or with 'and'
  #'
  search <-""
  slist = ""
  if(length(grep("\\s+",s.text))>0) {
    search<-ifelse(or==TRUE ,"or=(", "and=(")
    for(i in 1:length(s.text)) {
      slist<- ifelse(length(grep("\\s+",s.text[i]))>0, paste0(slist,paste0("full_text.phfts.",gsub("\\s+","%20",s.text[i]),sep=""), sep=","), paste0(slist,paste0("full_text.wfts.",s.text[i],sep=""), sep=","))
    }
    slist<-paste0(search,gsub(",$",")",slist),sep="")
  } else {
    slist<-ifelse(or==TRUE, paste0("full_text=wfts.",paste(unlist(s.text), collapse='%20or%20')) ,paste0("full_text=wfts.",paste(unlist(s.text), collapse='%20')))
  }

  return(slist)
}

config.ent<-function(entity.value){
  #' This function configures the entity values to make it compatible with the API
  #' @param entity.value The text or list of entities to search

  entlist<-""
  for(i in 1:length(entity.value)) {
    entlist<- ifelse(length(grep("\\s+",entity.value[i]))>0, paste0(entlist,gsub("\\s+","%20",entity.value[i]),sep=","), paste0(entlist,entity.value[i],sep=","))
  }
  entlist<-gsub(",$","",entlist)
  return(entlist)
}


find.entity.id<-function( value=NULL) {
  #' This function finds the API listing for a specific entity
  #' @param value The name or names of the entity to search
  search<-NULL
  notice<-NULL
  url<-"http://api.foiarchive.org/"

  if(missing(value)){
    notice <- "Please supply a value for the entity"
    stop(notice)
  }
  if(is.null(search)) search<-paste0("entities?entity=ilike.*",value,"*&select=entity,entgroup,wikidata_id,doc_cnt")

  url<-paste0(url,search)

  return(hlresults(url))
}

