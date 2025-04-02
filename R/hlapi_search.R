#' @title History Lab API - Full text search
#' @name hlapi_search
#' @description
#'
#' This program allows a full text search using the History Lab API.
#' In addition to a search term, collection and start.date and end.date are required fields.
#'
#' @usage
#' hlapi_search(s.text, fields=NULL, or = FALSE, start.date=NULL,end.date=NULL,
#' coll.name=NULL, limit = 25,run=TRUE,...)
#'
#' @param s.text Text to search--can be a single word, multiword phrase, or multiple variants
#' @param fields Specific History Lab fields to return
#' @param or Join text list with 'or' rather than 'and'
#' @param coll.name History Lab collection to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param limit Number of results to return (Default = 25)
#' @param run Run query (Default) or return API URL only
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_search('asean', coll.name=c('cfpf','frus'),
#' start.date="1974-01-01", end.date="1979-12-31")
#' \dontrun{
#' hlapi_search(c('udeac','asean'), run=TRUE)
#' }
#' hlapi_search(c('udeac','asean'), or=TRUE)
#' hlapi_search(c('league of nations'),  limit = 10, fields=c('doc_id','title'))

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_search<-function(s.text, fields=NULL, or = FALSE, start.date=NULL,end.date=NULL, coll.name=NULL, limit = 25,run=TRUE,...){
  url<-"http://api.foiarchive.org/documents?"
  search<-NULL
  notice<-NULL
  if(missing(s.text)){
    if(is.null(notice)) notice <- "Please supply a search term" else notice<-paste0(notice,"\nPlease supply a search term")
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

  url<-paste0(url,configsearch(s.text,or))
  url<-fmt_fields(url,fields)

  #Check collections if collections entered
  if(!is.null(coll.name)){
    url<-fmt_collnames(url,coll.name)
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
