#' @title History Lab API - Overview
#' @name hlapi_overview
#'
#' @description
#' This function allows an overview query of the History Lab API.
#' It will provide a list of fields and collections available to search as well as an overview of the entities in a collection.
#' If an entity type is mentioned but a date range is not specified using start.date and end.date, the function will return entities across the entire collection.
#'
#' Options to limit the results include collection and limit.
#'
#' @usage
#' hlapi_overview(collections=FALSE, fields=FALSE, sort = NULL,  coll.name=NULL,
#' entity.type=NULL, start.date=NULL, end.date=NULL, limit=25, run=FALSE,...)
#'
#' @param collections Show collections or overview of collections
#' @param fields Show the possible fields
#' @param entity.type Entity type to return (topics, persons, or countries)
#' @param sort Sort in (A)scending or (D)escending order. Default = No sort
#' @param coll.name History Lab collection to search
#' @param start.date Start date for a top entity search
#' @param end.date End date for a top entity search
#' @param limit Number of results to return (Default = 25)
#' @param run Run query or return API URL only (Default)
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_overview(collections=TRUE, run=TRUE)
#' hlapi_overview(coll.name='frus', entity.type = 'persons', sort = "D", run =TRUE)
#' hlapi_overview(coll.name='frus', entity.type = 'countries', run=TRUE)
#'

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_overview<-function(collections=FALSE, fields=FALSE, sort = NULL,  coll.name=NULL, entity.type=NULL, start.date=NULL, end.date=NULL, limit=25, run=FALSE,...){

  url<-"http://api.foiarchive.org/"
  notice<-NULL
  ##if((fields)&(!is.null(coll.name)|entity|!is.null(entity.type)|!is.null(start.date)|!is.null(end.date))) notice<-"Fields cannot be combined with any other option."
  ###We should be able to combine fields and collections so leaving collections out for now
  ##if((collections)&(!is.null(coll.name)|entity|!is.null(entity.type)|!is.null(start.date)|!is.null(end.date))) notice<-"Collections cannot be combined with any other option."

  if(!is.null(notice)){
    stop(notice)
  }

  if(fields){
    url<-paste0(url,"fields")
  }

  ### if(collections&is.null(coll.name)) {
  if(collections) {
    url<-paste0(url,"corpora")
  }

  if(!is.null(sort)) {
    if(sort!="D"& sort!="A"){
      print("Sort must be blank, A (Ascending), or D (Descending). Defaulting to no sort.")
      sort<-NULL
    } else {
      sort<-if(sort=="A") "&order=ref_cnt.asc" else "&order=ref_cnt.desc"
    }
  }

  if(!is.null(entity.type)){
    entity.type<-ck_list(entity.type)
    if(length(entity.type)>1) stop('The name of only one entity can be used with this function.')
    if(entity.type %ni% c("countries", "persons", "topics")){
      notice <- "Acceptable entities are countries, topics, or persons"
      stop(notice)
    }
    entity.type<-gsub("s$","",entity.type)
    entity.type<-gsub("ie$","y",entity.type)
  }

  if(!is.null(entity.type)) url<-paste0(url,"entities?entity_type=eq.",entity.type, sort)

  if(!is.null(coll.name)){
    coll.name<-ck_list(coll.name)
    f<-ck_collections(coll.name)
    if(!is.null(f)) stop(f)
    if(collections) url<-paste0(url,"?corpus=eq.",paste(unlist(coll.name), collapse=',')) else url<-paste0(url,"&corpus=eq.",paste(unlist(coll.name), collapse=','))
  }

  if(!is.null(start.date)|!is.null(end.date)){
    if(!is.null(start.date)&is.null(end.date)){
      if(is.null(notice)) notice<-"Please specify an end date." else notice<-paste0(notice,"\nPlease specify an end date.")
    }
    if(!is.null(end.date)&is.null(start.date)){
      if(is.null(notice)) notice<-"Please specify a start date." else notice<-paste0(notice,"\nPlease specify a start date.")
    }

    if(is.null(entity.type)){
      if(is.null(notice)) notice<-"Please specify an entity type for a top entity search." else notice<-paste0(notice,"\nPlease specify an entity type for a top entity search.")
    }

    if(!is.null(notice)){
      stop(notice)
    }

    s<-ck_date(start.date)
    url<-paste0(url,"&docs.authored=gte.",s)
    e<-ck_date(end.date)
    url<-paste0(url,"&docs.authored=lte.",e)
    if(as.Date(s,date.format="%y-%m-%d")>as.Date(e,date.format="%y-%m-%d")){
      stop('Start date must be less than or equal to end date.')
    }

  }
  if(!collections) {
    url<-paste0(url,"&limit=",limit)
  }
  if(run){
    u<-jsonlite::fromJSON(url)
    return(u)
  }
  if(!run){
    return(url)
  }


}


##hlapi_overview(coll.name='frus', entity.type = 'persons', sort = "A", run=TRUE)
##hlapi_overview(coll.name='frus', entity.type = 'persons', run=TRUE)
##hlapi_overview(coll.name='frus', entity.type = 'persons', sort = "As", run=TRUE)
##
##hlapi_overview(collections=TRUE, run=TRUE)
##
##hlapi_overview(coll.name='frus', entity.type = 'persons', sort = "D", run =TRUE)
##
##hlapi_overview(coll.name='frus', entity.type = 'persons', run = TRUE)
##
##hlapi_overview(collections=TRUE, coll.name='frus', run=TRUE)
##
##hlapi_overview(coll.name='frus,kissinger')
##
##hlapi_overview(coll.name='frus,kissinger', entity=TRUE)
##
##hlapi_overview(coll.name='frus', entity.type = 'persons,countries')
##
