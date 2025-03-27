#' @title History Lab API - Search by ID or IDs
#' @name hlapi_id
#' @description
#'
#' This program returns a list of random IDs from across the collections.
#' Neither the date nor the collection parameters are accepted.
#' If an ID is not in the History Lab database, the function will not return results for that ID.
#'
#' @usage
#' hlapi_id(ids=NULL, fields=NULL, run = NULL,...)
#'
#'
#' @param ids ID or list of IDs to search for
#' @param fields Specific History Lab fields to return
#' @param run Run query or return API URL only (Default)
#' @param ... Not used
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export
#' @examples
#' hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title',"persons")
#' ,  run = TRUE)
#' hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','title',
#' "topics","countries"), run = TRUE)

#require(jsonlite)
#source('R/histlabapi_utils.R')

hlapi_id<-function(ids=NULL, fields=NULL, run = NULL,...) {
  url<-"http://api.foiarchive.org/documents?doc_id=in.("
  notice<-NULL

  if(is.null(ids)) stop("You selected the id option but provided no ids. Please list ids, separated by a comma.")

  ids<-gsub(',\\s+',',',ids)
  ids<-gsub('\\s+',',',ids)

  url<-paste0(url,paste(unlist(ids), collapse=','),")")
  if(is.null(fields)){
    fields = c('doc_id','authored','title')
  }
  if(!is.null(fields)){
    fields<-ck_list(fields)
    f<-ck_fields(fields)
    if(!is.null(f)) stop(f)
  }
  url<-paste0(url,"&select=",paste(unlist(fields), collapse=','))

  if(!is.null(run)){
    u<-hlresults(url)
    return(hlresults(url))
  }
  if(is.null(run)){
    return(url)
  }

}

# ## Single ID
# hlapi_id(ids='frus1969-76ve05p1d11', run = TRUE)
#
# hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title'), run = TRUE)
#
# ## With topics
# hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title','topics'), run = TRUE)
#
# ## With countries
# hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title','countries'), run = TRUE)
#
# hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title','persons'), run = TRUE)
#
# ## Topics & countries
# hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title','topics','countries'), run = TRUE)
#
# ## Multiple IDs
# hlapi_id(ids=c('frus1969-76ve05p1d11','frus1958-60v03d44'), fields = c('doc_id','body','title'), run=TRUE)
#
# hlapi_id(ids=c('frus1969-76ve05p1d11','frus1958-60v03d44'), run=TRUE)
#
#
# hlapi_id(ids=c('frus1969-76ve05p1d11','frus1958-60v03d44'), fields = c('doc_id','body','title','topics','countries'), run=TRUE)
#
# ## Wrong ID - only returns correct ID infor
# hlapi_id(ids=c('frus1969','frus1958-60v03d44'), run=TRUE)
#
# hlapi_id(ids=c('frus1969'), run=TRUE)
