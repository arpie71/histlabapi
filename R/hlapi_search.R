#' @title History Lab API - Full text search
#' @name hlapi_search
#' @description
#'
#' This program allows a full text search using the History Lab API.
#' In addition to a search term, collection and start.date and end.date are required fields.
#'
#' @usage
#' hlapi_search(s.text, fields=NULL, or = FALSE, start.date=NULL,end.date=NULL,
#' coll.name=NULL, limit = 25,run=NULL,...)
#'
#' @param s.text Text to search--can be a single word, multiword phrase, or multiple variants
#' @param fields Specific History Lab fields to return
#' @param or Join text list with 'or' rather than 'and'
#' @param coll.name History Lab collection to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param limit Number of results to return (Default = 25)
#' @param run Run query or return API URL only (Default)
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_search('asean', collection=c('statedeptcables','frus'),
#' start.date="1974-01-01", end.date="1979-12-31",run=TRUE)
#' \dontrun{
#' hlapi_search(c('udeac','asean'), run=TRUE)
#' }
#' hlapi_search(c('udeac','asean'), or=TRUE, run=TRUE)
#' hlapi_search(c('league of nations'),  limit = 10, fields=c('doc_id','title'), run=TRUE)

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_search<-function(s.text, fields=NULL, or = FALSE, start.date=NULL,end.date=NULL, coll.name=NULL, limit = 25,run=NULL,...){
  url<-"http://api.foiarchive.org/documents?"
  search<-NULL
  notice<-NULL
  if(missing(s.text)){
    if(is.null(notice)) notice <- "Please supply a search term" else notice<-paste0(notice,"\nPlease supply a search term")
  }

  if(is.null(fields)){
    fields = c('doc_id','authored','title')
  }
  if(!is.null(fields)){
    f<-ck_fields(fields)
    if(!is.null(f)) stop(f)
  }

  url<-paste0(url,configsearch(s.text,or))
  url<-paste0(url,"&select=",paste(unlist(fields), collapse=','))
  #Check collections if collections entered
  if(!is.null(coll.name)){
    coll.name<-ck_list(coll.name)
    f<-ck_collections(coll.name)
    if(!is.null(f)) stop(f, call.=FALSE)
    url<-paste0(url,"&corpus=in.(",paste(unlist(coll.name), collapse=','),paste(")"))
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

  if(isTRUE(run)){
    return(hlresults(url))
  }
  if(!isTRUE(run)){
    return(url)
  }
}

# # Error queries
#
# ##  start date < end date
# hlapi_search('udeac', coll.name=c('statedeptcables','frus'),  start.date="1974-01-01", end.date="1973-12-31", run='run')
#
#
# ## no search term
# hlapi_search( fields=c("doc_id","title","classification"))
#
# ## bad field
# hlapi_search('udeac', coll.name=c('statedeptcables','frus'), fields="doc")
#
#
# # Good queries
# hlapi_search('udeac', coll.name=c('statedeptcables','frus'))
#
# hlapi_search(c('udeac','asean'), coll.name=c('statedeptcables','frus'))
#
# hlapi_search(c('udeac','asean'), run=TRUE)
# hlapi_search(c('udeac','asean'), or=TRUE, run=TRUE)
# hlapi_search(c('ford','asean'), run=TRUE)
# hlapi_search(c('asean'), run=TRUE)
#
# hlapi_search(c('udeac','asean'), fields=c('doc_id','title','authored','topics'), run=TRUE)
# hlapi_search(c('udeac','asean'),  or=TRUE,fields=c('doc_id','title','authored','topics'), run=TRUE)
#
# h<-hlapi_search(c('united nations'), limit = 100000, fields=c('doc_id','title'),run=TRUE)
#
# h<-hlapi_search(c('league of nations'), run=TRUE, limit = 100000, fields=c('doc_id','title'))
# hlapi_search(c('league of nations'),  limit = 100000, fields=c('doc_id','title'))
#
# #hlapi_search('udeac', coll.name=c('statedeptcables','frus'), run='run')
#
# hlapi_search('udeac', coll.name=c('statedeptcables','frus'),  start.date="1950-01-01", end.date="2000-12-31", fields=c('body'))
#
# #hlapi_search('udeac', coll.name=c('statedeptcables','frus'),  start.date="1974-01-01", end.date="1979-12-31", run=TRUE)
#
#
# hlapi_search('united nations', coll.name=c('statedeptcables','frus'),  start.date="1974-01-01", end.date="1979-12-31")
#
# hlapi_search('united nations', coll.name=c('statedeptcables','frus'),  start.date="1974-01-01", end.date="1979-12-31", run=TRUE)
#
# hlapi_search('league of  nations', coll.name=c('statedeptcables','frus'),  start.date="1974-01-01", end.date="1979-12-31")
#
#
# hlapi_search(c('udeac','asean'))
# hlapi_search(c('ford','asean'), run=TRUE)
# hlapi_search(c('asean'), run=TRUE)
#
#
# h<-hlapi_search(c('udeac','asean'),  fields=c('doc_id','title','authored','topics'), or=TRUE, run=TRUE)
# hlapi_search(c('udeac','asean'), fields=c('doc_id','title','authored','topics'), or=TRUE,  start.date="1950-01-01", end.date="1970-12-31", run=TRUE)

#t<-hlapi_search(c('udeac','asean'), fields=c('doc_id','title','authored','countries'), or=TRUE,  start.date="1950-01-01", end.date="1970-12-31", run=TRUE)
#hlapi_search(c('udeac','asean'),fields=c('doc_id','title','authored','topics','countries'), or=TRUE,  start.date="1950-01-01", end.date="1970-12-31", run=TRUE)
