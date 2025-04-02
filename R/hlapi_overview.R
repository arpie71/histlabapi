#' @title History Lab API - Overview
#' @name hlapi_overview
#'
#' @description
#' This function allows an overview query of the History Lab API.
#' It will provide a list of fields and collections available to search as well as an overview of the entities and topics  in a collection.
#'
#'
#' Options to limit the results include collection and limit.
#'
#' @usage
#' hlapi_overview(aspect=NULL,  sort = NULL,  coll.name=NULL,
#' entity.type=NULL, limit=250, run=TRUE,...)
#'
#' @param aspect Type of overview
#' @param entity.type Entity type to return (PERSON, ORG, LOC, GOVT, and OTHER)
#' @param sort Sort in (A)scending or (D)escending order. Default = No sort
#' @param coll.name History Lab collection to search
#' @param limit Number of results to return (Default = 250)
#' @param run Run query (Default) or return API URL only
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_overview(aspect='collections')
#' hlapi_overview(aspect='entities', entity.type = 'PERSON', sort = "D")
#' hlapi_overview(aspect='topics',coll.name='frus')
#' hlapi_overview(aspect='topics',coll.name='frus', run=FALSE)
#'

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_overview<-function(aspect=NULL, sort = NULL,  coll.name=NULL, entity.type=NULL, limit=250, run=TRUE,...){
  url<-"http://api.foiarchive.org/"

  if(aspect %ni% c("collections", "entities", "topics")){
    stop("Acceptable values are collections, entities, or topics")
}

  switch(aspect,
    "entities" = {
    ## Entities
    url<-paste0(url,"entities?")
    if(!is.null(coll.name)) message("Entities search cannot be combined with coll.name. Ignoring coll.name")
    if(!is.null(entity.type)){
      url<-ck_entities(url, entity.type)
    }
    if (!is.null(sort)) {
      if (sort %in% c("A", "D")) {
        url <- paste0(url, ifelse(sort == "A", "&order=doc_cnt.asc", "&order=doc_cnt.desc"))
      } else {
        message("Sort must be blank, A (Ascending), or D (Descending). Defaulting to no sort.")
      }
    }

    },

  ## TOPICS
  "topics" = {
    if(!is.null(entity.type)){
      print("Topics overview cannot be combined with entity.type. Ignoring entity.type")
    }
    url<-paste0(url,"topics?")
    if(!is.null(coll.name)){
      url<-fmt_collnames(url,coll.name)
    }
  },
  "collections" = {
    if(!is.null(entity.type)){
      print("Collections overview cannot be combined with entity.type. Ignoring entity.type")
    }
    url<-paste0(url,"corpora")
    url<-paste0(url,"?select=corpus,title,begin_date,end_date,doc_cnt,pg_cnt,word_cnt,topic_cnt")
    if(!is.null(coll.name)){
      url<-fmt_collnames(url,coll.name)
    }
  }
)
  if(!is.null(limit)) {
    url<-paste0(url,"&limit=",limit)
  }
  if(run){
    return(jsonlite::fromJSON(url))
  }
  return(url)
}
