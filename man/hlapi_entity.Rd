% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hlapi_entity.R
\name{hlapi_entity}
\alias{hlapi_entity}
\title{History Lab API - Entity search}
\usage{
hlapi_entity( entity.value, fields=NULL, coll.name=NULL,
date=NULL,start.date=NULL, end.date=NULL, or=FALSE, run=TRUE, limit = 25, ...)
}
\arguments{
\item{entity.value}{The entity Wikidata ID to be searched}

\item{fields}{Specific History Lab fields to return}

\item{coll.name}{History Lab collection to search}

\item{date}{Specific date to search}

\item{start.date}{Date range for the search}

\item{end.date}{Date range for the search}

\item{or}{Join entity list with 'or' rather than 'and'}

\item{run}{Run query (Default) or return API URL only}

\item{limit}{Number of results to return (Default = 25)}

\item{...}{Not used}
}
\value{
Returns a json object (if run = TRUE) or API URL (if run=FALSE)
}
\description{
This program allows a search of the History Lab API by entity.
}
\examples{
find.entity.id('Belize') # Belize Wikidata ID is Q242
hlapi_entity(entity.value='Q242', coll.name='frus')
find.entity.id('Nixon') # RMN Wikidata ID is Q9588
hlapi_entity(entity.value="Q9588", coll.name='frus')
}
