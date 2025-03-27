#' @title History Lab API - Entity search
#' @name hlapi_entity
#' @description
#'
#' This program allows a search of the History Lab API by type of entity (countries, topics, or persons).
#'
#' @usage
#' hlapi_entity(entity.type=c('topics','persons','countries'), country.value, topic.value,
#' person.value, fields=NULL, coll.name=NULL,date=NULL,start.date=NULL, end.date=NULL,
#' summary= FALSE, orlist, run=NULL, limit = 25, ...)
#'
#' @param entity.type The type of entity to search (countries, topics, and/or persons)
#' @param country.value The specific country entity to be searched
#' @param topic.value The specific topic entity to be searched
#' @param person.value The specific person entity to be searched
#' @param fields Specific History Lab fields to return
#' @param coll.name History Lab collection to search
#' @param date Specific date to search
#' @param start.date Date range for the search
#' @param end.date Date range for the search
#' @param summary Aggregate the results by entity.type (Default = FALSE)
#' @param limit Number of results to return (Default = 25)
#' @param orlist Join entity list with 'or' rather than 'and'
#' @param run Run query or return API URL only (Default)
#' @param ... Not used
#'
#' @return Returns a json object (if run = TRUE) or API URL (if run=FALSE)
#' @export


#require(jsonlite)
#source('R/histlabapi_utils.R')
# hlapi_entity('countries', country.value='Belize',  run=TRUE, coll.name='frus', summary=TRUE)
# hlapi_entity('countries', country.value='Belize',  run=TRUE, coll.name='frus')
# find.entity.id('persons','Nixon')
# hlapi_entity('persons', person.value='109882',  run=TRUE, coll.name='frus')

hlapi_entity<-function(entity.type=c('topics','persons','countries'), country.value=NULL,
                       topic.value=NULL, person.value=NULL, fields=NULL,coll.name=NULL,date=NULL,start.date=NULL,
                       end.date=NULL,summary= FALSE,orlist=NULL, run=NULL, limit = 25,...){
  url<-"http://api.foiarchive.org/"
  search<-NULL
  notice<-NULL
  if(missing(entity.type)){
    notice <- "Please supply an entity: countries, topics, and/or persons"
    stop(notice)
  }

  entity.type<-ck_list(entity.type)
  orlist<-ck_list(orlist)

  for(i in 1:length(entity.type)){
    if(entity.type[i] %ni% c("countries", "persons", "topics")){
      notice <- "Acceptable entities are countries, topics, and/or persons"
      stop(notice)
    }
  }

  if(!is.null(orlist)){
    for(i in 1:length(orlist)){
      if(orlist[i] %ni% c("countries", "persons", "topics")){
        notice <- "Acceptable options for the or parameter are countries, topics, and/or persons"
        stop(notice)
      }
    }
  }
  for(i in 1:length(entity.type)){
    if(entity.type[i]=="countries" & missing(country.value)){
      notice<-paste(notice,"Please specify a country value or set of country values",sep="\n")
    }
    if(entity.type[i]=="topics" & missing(topic.value)){
      notice<-paste(notice,"Please specify a topic value or set of topic values",sep="\n")
    }
    if(entity.type[i]=="persons" & missing(person.value)){
      notice<-paste(notice,"Please specify a person value or set of person values",sep="\n")
    }
    if(!is.null(notice)) stop(notice)

    if(summary==FALSE) {
      if(entity.type[i]=="countries") {
        country.value<-config.ent(country.value)
        ors<-ifelse('countries' %in% orlist, 'ov','cs')
        #if(is.null(search)) search<-paste0("documents?countries=",ors,".{",country.value,"}")
        search<-ifelse(is.null(search),paste0("documents?entities=",ors,".{",country.value,"}"),paste0(search,"&entities=",ors,".{",country.value,"}"))
      }
      if(entity.type[i]=="topics"){
        topic.value<-config.ent(topic.value)
        ors<-ifelse('topics' %in% orlist, 'ov','cs')
        search<-ifelse(is.null(search),paste0("documents?topics=",ors,".{",topic.value,"}"),paste0(search,"&topics=",ors,".{",topic.value,"}"))
      }
      if(entity.type[i]=="persons"){
        person.value<-config.ent(person.value)
        ors<-ifelse('persons' %in% orlist, 'ov','cs')
        search<-ifelse(is.null(search),paste0("documents?entities=",ors,".{",person.value,"}"),paste0(search,"&entities=",ors,".{",person.value,"}"))
      }
    }
    if(summary==TRUE){
      if(is.null(fields)){
        fields = c('entity','entgroup','wikidata_id','doc_cnt')
      }
      if(length(country.value)+length(topic.value)+length(person.value)>1) stop("Only a single entity value may be used with a summary search")
      if(entity.type[i]=="countries"&is.null(search)) search<-paste0("entities?entgroup=eq.LOC&entity=ilike.*",country.value,"*")
      if(entity.type[i]=="topics"&is.null(search)) search<-paste0("entities?entgroup=topic&entity=ilike.*",topic.value,"*")
      if(entity.type[i]=="persons"&is.null(search)) search<-paste0("entities?entgroup=eq.PERSON&entity=ilike.*",person.value,"*")
      search<-paste0(search,"&select=",paste(unlist(fields), collapse=','))
          }

  }
  url<-paste0(url,search)

  if(summary==FALSE){
    if(is.null(fields)){
      fields = c('doc_id','authored','title')
    }
    # Check fields if fields entered
    if(!is.null(fields)){
      fields<-ck_list(fields)
      f<-ck_fields(fields)
      if(!is.null(f)) stop(f)
    }
    url<-paste0(url,"&select=",paste(unlist(fields), collapse=','))
  }

  #Check collections if collections entered
  if(!is.null(coll.name)){
    coll.name<-ck_list(coll.name)
    f<-ck_collections(coll.name)
    if(!is.null(f)) stop(f, call.=FALSE)
    if(summary==FALSE) url<-paste0(url,"&corpus=eq.",paste(unlist(coll.name), collapse=',')) else url<-paste0(url,"&corpus=eq.",paste(unlist(coll.name), collapse=','))
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

  if(!is.null(start.date)) {
    s<-ck_date(start.date)
    url<-paste0(url,"&authored=gte.",s)

  }

  if(!is.null(end.date)) {
    e<-ck_date(end.date)
    url<-paste0(url,"&authored=lte.",e)
    if(as.Date(s,date.format="%y-%m-%d")>as.Date(e,date.format="%y-%m-%d")){
      stop('Start date must be less than or equal to end date.')
    }
  }

  url<-paste0(url,"&limit=",limit)

  if(!is.null(run)){
    u<-jsonlite::fromJSON(url)
    return(u)
  }
  if(is.null(run)){
    return(url)
  }
}


# hlapi_entity('topics', value='1001,1000')
#
# hlapi_entity('countries', country.value='Belize', run=TRUE)
#
# hlapi_entity('countries', country.value='Belize', summary = TRUE)
#
# h<-hlapi_entity('countries', country.value='Belize',  run=TRUE, coll.name='frus')
#
# h<-hlapi_entity('countries', country.value='Belize', date="1960-03-21", run=TRUE)
#
#
# hlapi_entity('countries', country.value='Belize', date="1960-03-21")
# #hlapi_entity('topics', topic.value=c('1001,1000'))
#
#
# find.entity.id('Rumsfeld')
# hlapi_entity('persons',person.value='111727', run=TRUE)
# hlapi_entity('persons',person.value='Rumsfeld', run=TRUE, summary=TRUE)
