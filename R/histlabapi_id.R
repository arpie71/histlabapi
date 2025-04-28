#' @title History Lab API - Search by ID or IDs
#' @name hlapi_id
#' @description
#'
#' This program returns data for a document ID or list of document IDs.
#' Neither the date nor the collection parameters are accepted.
#' If an ID is not in the History Lab database, the function will not return results for that ID.
#'
#' @usage
#' hlapi_id(ids=NULL, fields=NULL, run = TRUE,...)
#'
#'
#' @param ids ID or list of IDs to search for
#' @param fields Specific History Lab fields to return
#' @param run Run query (Default) or return API URL only
#' @param ... Not used
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title',"entities"))
#' hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','title',"topic_names","entities"))
#' hlapi_id(ids=c('frus1969-76ve05p1d11','frus1958-60v03d45'), fields = c('doc_id','body','title'))

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_id<-function(ids=NULL, fields=NULL, run = TRUE,...) {

  if(is.null(ids)) stop("You selected the id option but provided no ids. Please list ids, separated by a comma.")
  ids<-gsub('\\s+','',ids)

  url <- sprintf("https://api.foiarchive.org/documents?doc_id=in.(%s)", paste(ids, collapse = ','))
  url<-fmt_fields(url,fields)

  if(run){
    return(jsonlite::fromJSON(url))
  }
    return(url)
}
