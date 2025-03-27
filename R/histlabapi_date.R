#' @title History Lab API - Date search
#' @name hlapi_date
#' @description
#' This program allows a date or date range search using the History Lab API.
#' Options to limit the results include collection and fields.
#'
#' @usage
#' hlapi_date(date=NULL,start.date=NULL,end.date=NULL,fields=NULL,coll.name=NULL,
#' limit = 25,run=FALSE,...)
#'
#' @param date Specific date to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param coll.name Name of History Lab collection to search
#' @param fields Specific History Lab fields to return
#' @param limit Number of results to return (Default = 25)
#' @param run Run query or return API URL only (Default)
#' @param ... Not used
#'
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_date(date='1980-09-15', fields=c("doc_id","classification","title"),
#' coll.name="frus", run=TRUE)
#' hlapi_date(start.date='1947-01-01', end.date='12/01/1948', limit=100,run=TRUE)
#' hlapi_date(start.date='1947-01-01', end.date='12/01/1948',
#' fields=c("doc_id","classification","title", "persons"), limit=10,run=TRUE)


#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_date<-function(date=NULL,start.date=NULL,end.date=NULL,fields=NULL,coll.name=NULL,  limit = 25,run=FALSE,...){
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

  if(!is.null(start.date)) {
    s<-ck_date(start.date)
    url<-paste0(url,"authored=gte.",s)

  }

  if(!is.null(end.date)) {
    e<-ck_date(end.date)
    url<-paste0(url,"&authored=lte.",e)
    if(as.Date(s,date.format="%y-%m-%d")>as.Date(e,date.format="%y-%m-%d")){
      stop('Start date must be less than or equal to end date.', call.=FALSE)
    }

  }

  if(is.null(fields)){
    fields = c('doc_id','authored','title')
  }

  # Check fields if fields entered
  if(!is.null(fields)){
    fields<-ck_list(fields)
    f<-ck_fields(fields)
    if(!is.null(f)) stop(f, call.=FALSE)
  }
  url<-paste0(url,"&select=",paste(unlist(fields), collapse=','))

  #Check collections if collections entered
  if(!is.null(coll.name)){
    coll.name<-ck_list(coll.name)
    f<-ck_collections(coll.name)
    if(!is.null(f)) stop(f, call.=FALSE)
    url<-paste0(url,"&corpus=eq.",paste(unlist(coll.name), collapse=','))
  }

  url<-paste0(url,"&limit=",limit)

  if(run){
    return(hlresults(url))
  }
  if(!run){
    return(url)
  }
}

# # Error queries
# ## end date and no start date
# hlapi_date(end.date='12/01/1948', fields=c("doc_id","classification","title"), run=FALSE)
#
# ## start date and no end date
# hlapi_date(start.date='12/01/1948', fields=c("doc_id","classification","title"), run=FALSE)
#
# ## date and end date
# hlapi_date(date='1980-09-15', end.date='12/01/1948', fields=c("doc_id","classification","title"), run=FALSE)
# ## date and start date
# hlapi_date(date='1980-09-15', start.date='12/01/1948', fields=c("doc_id","classification","title"), run=FALSE)
#
# ## end date before start date
# hlapi_date(start.date='1949-01-01', end.date='12/01/1948', limit=100,run=FALSE)
#
# ## malformed date
# hlapi_date(date='1980-13-15', fields=c("doc_id","classification","title"), run=FALSE)
#
#
# hlapi_date(start.date='1947-01-01', end.date='12/01/1948', run=TRUE)
#
# h<-hlapi_date(start.date='1947-01-01', end.date='12/01/1948', limit=100,run=TRUE)
# h<-hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","classification","title"), run=TRUE)
#
# hlapi_date(date='1980-09-15', fields=c("doc_id","classification","title"),coll.name="frus", run=TRUE)
#
# h<-hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","authored","title","topics") , limit=100,run=TRUE)
# h<-hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","authored","title","countries") , limit=100,run=TRUE)
# hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","authored","title","persons") , limit=100,run=TRUE)
#
# h<-hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","authored","title","topics","countries") , limit=100,run=TRUE)
#
#
# hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","classification","title"),run=TRUE)
#
# #histlabapi , options(date)  start(01/01/1947) end(12/01/1948)  limit(100)
# #histlabapi , options(date) collection(cpdoc kissinger) start(01/1/1975) end(12/01/1975)  limit(100)
# #histlabapi , options(date) start(01/1/1973) end(2/01/1973)  limit(100) collection(kissinger) fields(subject,body)
#
#
# hlapi_date(date='1980-09-17', fields=c("doc_id","classification","title"), run=TRUE)
