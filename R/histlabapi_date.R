#' @title History Lab API - Date search
#' @name hlapi_date
#' @description
#' This program allows a date or date range search using the History Lab API.
#' Options to limit the results include collection and fields.
#'
#' @usage
#' hlapi_date(date=NULL,start.date=NULL,end.date=NULL,fields=NULL,coll.name=NULL,
#' limit = 25,run=TRUE,...)
#'
#' @param date Specific date to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param coll.name Name of History Lab collection to search
#' @param fields Specific History Lab fields to return
#' @param limit Number of results to return (Default = 25)
#' @param run Run query (Default) or return API URL only
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_date(date='1980-09-15', fields=c("doc_id","classification","title"), coll.name="frus")
#' hlapi_date(start.date='1947-01-01', end.date='12/01/1948', limit=100)
#' hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","classification","title", "entities"), limit=10)

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_date<-function(date=NULL,start.date=NULL,end.date=NULL,fields=NULL,coll.name=NULL,  limit = 25,run=TRUE,...){
  url<-"http://api.foiarchive.org/documents?"
  notice<-NULL

  if(is.null(date)&(is.null(start.date)&is.null(end.date))) stop('You must supply either a date or start and end dates for a date search.')

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
    stop(notice, call.=FALSE)
  }
  if(!is.null(date)) {
    d<-ck_date(date)
    url<-paste0(url,"authored=eq.",d)
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

  url<-fmt_fields(url,fields)

  #Check collections if collections entered
  if(!is.null(coll.name)){
    url<-fmt_collnames(url,coll.name)
  }

  url<-paste0(url,"&limit=",limit)

  if(run){
    return(jsonlite::fromJSON(url))
  }
  return(url)
}

