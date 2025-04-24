
'%ni%'<-Negate('%in%')

ck_fields <- function(fields) {
  #' Check and validate fields before sending to API endpoint
  #' @param fields List of fields to check
  #' @keywords internal
  #' @return Error message if invalid fields are found, otherwise NULL

  valid_fields <- c("authored", "body", "wikidata_ids", "entgroups", "entities", "topic_names", "topic_scores", "topic_ids", "title", "classification", "corpus", "doc_id")

  invalid_fields <- setdiff(fields, valid_fields)

  if (length(invalid_fields) > 0) {
    verb <- ifelse(length(invalid_fields) > 1, "are", "is")
    return(paste("Field Error:", paste(invalid_fields, collapse = ','), verb, "not valid"))
  }

  return(NULL)
}

fmt_fields<-function(url,fields){
  #' Format fields for API request
  #' @param url Base API URL
  #' @param fields List of fields to include
  #' @keywords internal

  if(is.null(fields)){
    fields<- c('doc_id','authored','title')
  }
  # Check fields if fields entered
  if(!is.null(fields)){
    fields<-ck_list(fields)
    f<-ck_fields(fields)
    if(!is.null(f)) stop(f)
  }
  url<-paste0(url,"&select=",paste(unlist(fields), collapse=','))
  return(url)
}


ck_list<-function(x) {
  #' This function will ensure a valid format for options that allow multiple values such as collections, ids, fields
  #' @param x Object to check
  #' @keywords internal
  #'
  if(length(x)==1){
    #x<-gsub(',\\s+',',',x)
    #x<-gsub('\\s+',',',x)
    x<-gsub('\\s+','',x)
    x<-as.list(strsplit(x,",")[[1]])
  }
  return(x)
}

fmt_collnames<-function(url,coll.name){
  #' This function formats the collection names option when it is specified
  #' @param url API URL
  #' @param coll.name Name of collection or list of names of collections
  #' @keywords internal

  coll.name<-ck_list(coll.name)
  f<-ck_collections(coll.name)
  if(!is.null(f)) stop(f)
  if(length(coll.name)>1) url<-paste0(url,"&corpus=in.(",paste(unlist(coll.name), collapse=','),")") else url<-paste0(url,"&corpus=eq.",coll.name)
  return(url)
}

ck_collections<-function(collections){
  #' This function checks to make sure that the collections listed are correct
  #' @param collections List of collections to check
  #' @keywords internal
  #' Note: right now collections are hard-coded but will be moved to configuration file

  collection.list<-c("frus","cia","clinton","briefing","cfpf","kissinger","nato","un","worldbank","cabinet","cpdoc")
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
  #' @keywords internal
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
  #' @keywords internal
  u<-try(jsonlite::fromJSON(url), silent=TRUE)
  if("try-error" %in% class(u)) stop("Problem with query")
  if(!length(u)) stop("No results found", call.=FALSE)
  return(u)
}


configsearch<-function(s.text, or){
  #' This function configures the search text to make it compatible with the API
  #' @param s.text The text or list of texts to search
  #' @param or Whether to join text with 'or' or with 'and'
  #' @keywords internal
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


### ENTITIES
config.ent<-function(entity.value){
  #' This function configures the entity values to make it compatible with the API
  #' @param entity.value The text or list of entities to search
  #' @keywords internal
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
  url<-"http://api.foiarchive.org/"

  if(missing(value)){
    stop("Please supply a value for the entity", call.=FALSE)
  }
  search<-paste0("entities?entity=ilike.*",value,"*&select=entity,entgroup,wikidata_id,doc_cnt")

  url<-paste0(url,search)
  url <- paste0(url, "&order=doc_cnt.desc")
  return(hlresults(url))
}

ck_wikidata<-function(entity.values){
  #' This function will makes sure a valid entity is given
  #' @param entity.values Wikidata ID to validate
  #' @keywords internal
  #'
  notice<-NULL
  for(i in 1:length(entity.values)){
    if(!grepl("Q\\d+$",entity.values[i])){
      notice <- "Wikidata ID is not properly formatted"
    }
  }
  return(notice)
}

ck_entities<-function(url,entity.type) {
  #' This function will makes sure a valid entity is given
  #' @param url URL to pass back with entity.type info added
  #' @param entity.type Object to check
  #' @keywords internal
  valid_entities <- c("PERSON", "ORG", "LOC", "GOVT", "OTHER")

  if (any(entity.type %ni% valid_entities)) {
    stop("Acceptable entities are PERSON, ORG, LOC, GOVT, OTHER", call. = FALSE)
  }
  if(length(entity.type)>1) url<-paste0(url,"&entgroup=in.(",paste(unlist(entity.type), collapse=','),")") else url<-paste0(url,"&entgroup=eq.",entity.type)
  url<-paste0(url,"&select=entity,entgroup,wikidata_id,doc_cnt")
  return(url)
}


## TOPICS

config.topics<-function(topics.value){
  #' This function configures the topics values to make it compatible with the API
  #' @param topics.value The text or list of topics to search
  #' @keywords internal
  topicslist<-""
  for(i in 1:length(topics.value)) {
    topicslist<- ifelse(length(grep("\\s+",topics.value[i]))>0, paste0(topicslist,gsub("\\s+","%20",topics.value[i]),sep=","), paste0(topicslist,topics.value[i],sep=","))
  }
  topicslist<-gsub(",$","",topicslist)
  return(topicslist)
}


find.topics.id<-function( value=NULL) {
  #' This function finds the API listing for a topic containing a term
  #' @param value The name or names of the entity to search
  url<-"http://api.foiarchive.org/"

  if(missing(value)){
    stop("Please supply a value for the topic")
  }
  search<-paste0("topics?name=ilike.*",value,"*&select=corpus,topic_id,title,name")

  url<-paste0(url,search)
  return(hlresults(url))
}

ck_topics<-function(topics.values){
  #' This function will makes sure a valid entity is given
  #' @param topics.values Topic IDs to validate
  #' @keywords internal
  notice<-NULL
  for(i in 1:length(topics.values)){
    if(!grepl("^\\d+$",topics.values[i])){
      notice <- "Topics ID is not properly formatted"
    }
  }
  return(notice)
}
