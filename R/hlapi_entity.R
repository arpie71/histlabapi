#' @title History Lab API - Entity search
#' @name hlapi_entity
#' @description
#'
#' This program allows a search of the History Lab API by entity.
#'
#' @usage
#' hlapi_entity( entity.value, fields=NULL, coll.name=NULL,
#' date=NULL,start.date=NULL, end.date=NULL, or=FALSE, run=TRUE, limit = 25, ...)
#'
#' @param entity.value The entity Wikidata ID to be searched
#' @param fields Specific History Lab fields to return
#' @param coll.name History Lab collection to search
#' @param date Specific date to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param limit Number of results to return (Default = 25)
#' @param or Join entity list with 'or' rather than 'and'
#' @param run Run query (Default) or return API URL only
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' find.entity.id('Belize') # Belize Wikidata ID is Q242
#' hlapi_entity(entity.value='Q242', coll.name='frus')
#' find.entity.id('Nixon') # RMN Wikidata ID is Q9588
#' hlapi_entity(entity.value="Q9588", coll.name='frus')

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_entity<-function(entity.value=NULL, fields=NULL,coll.name=NULL,date=NULL,start.date=NULL,
                       end.date=NULL,or=FALSE, run=TRUE, limit = 25,...){
  url<-"https://api.foiarchive.org/"
  #search<-NULL
  if(missing(entity.value)){
    stop("Please supply an entity value")
  }

  # Prepare entity value
  notice<-ck_wikidata(entity.value)
  entity.value<-config.ent(entity.value)
  # Prepare OR list
  ors<-ifelse(or==TRUE,'ov', 'cs')
  search <- paste0("documents?wikidata_ids=", ors, ".{", entity.value, "}")

  url<-paste0(url,search)

  url<-fmt_fields(url,fields)

  #Check collections if collections entered
  if(!is.null(coll.name)){
    url<-fmt_collnames(url,coll.name)
  }

  if(!is.null(date)&(!is.null(start.date)|!is.null(end.date))){
    if(is.null(notice)) notice<-"You cannot specify both a date and a start or end date." else notice<-paste0(notice,"\nYou cannot specify both a date and a start or end date.")
  }
  if(!is.null(start.date)&is.null(end.date)){
    if(is.null(notice)) notice<-"Please specify an end date." else notice<-paste0(notice,"\nPlease specify an end date.")
  }
  if(!is.null(end.date)&is.null(start.date)){
    if(is.null(notice)) notice<-"Please specify a start date." else notice<-paste0(notice,"\nPlease specify a start date.")
  }

  if(!is.null(notice)){
    stop(notice)
  }
  if(!is.null(date)) {
    d<-ck_date(date)
    url<-paste0(url,"&authored=eq.",d)
  }

  if(!is.null(end.date)&!is.null(start.date)){
    s<-ck_date(start.date)
    e<-ck_date(end.date)

    if(as.Date(s,date.format="%y-%m-%d")>as.Date(e,date.format="%y-%m-%d")){
      stop('Start date must be less than or equal to end date.')
    }
    url<-paste0(url,"&authored=gte.",paste(s, collapse=','))
    url<-paste0(url,"&authored=lte.",paste(e, collapse=','))
  }

  url<-paste0(url,"&limit=",limit)

  if(run){
    return(jsonlite::fromJSON(url))
  }
    return(url)
}



