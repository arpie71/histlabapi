
# Quick Start Guide

This page will walk through how to use the History Lab API to access relevant data. 

## Set up

The package can be installed from GitHub
```devtools::install_github('history-lab/histlabapi')```

The package requires the jsonlite package. 


## History Lab data
More information about the History Lab collections are available on [our website](https://lab.history.columbia.edu/content/collections).

## Functions

There are some options that overlap all or several functions. 

`coll.name` - name of the collection or collections to search. Multiple collections should be listed using R list notation (`c()`). Available collection names are: frus, cia, clinton, briefing, cfpf, kissinger, nato, un, worldbank, cabinet, cpdoc
 
`fields` - name of database column or list of columns to return. Available field names are: authored, body, wikidata_ids, entgroups, entities, topic_titles, topic_names, topic_scores, topic_ids, title, classification, corpus, doc_id

`start.date` - Used with the `end.date` option to restrict the documents retrieved to a certain date range. The start date specifies the initial date of a date range to be searched. Dates should be in numeric format and use either ".","-", or "/" as separators between month, day, and year. The function first looks for a "MDY" or a "YMD" format but it will recognize a "DMY" format for days greater than 12.
 
`end.date` - Used with the `start.date` option to restrict the documents retrieved to a certain date range. The end date specifies the end date of a date range to be searched. Dates should be in numeric format and use either ".","-", or "/" as separators between month, day, and year. The function first looks for a "MDY" or a "YMD" format but it will recognize a "DMY" format for days greater than 12.


`limit` - Number of results to return. The default in all functions except hlapi_overview is 25 results. The maximum number of results that can be returned is 10,000. Please note that queries returning a larger number of documents may time out.
 
`run` - Developer option. If set to FALSE, function will return the API call. 

### hlapi_overview()
 ``hlapi_overview(aspect=NULL,  sort = NULL,  coll.name=NULL, entity.type=NULL, limit=250, run=TRUE,...)``

`aspect` is the type of overview to return. There are 3 acceptable values: collections, entities, and topics. 

`sort` - only available with the entities aspect. If A is specified, entities will be returned in ascending order of appearance. If D is specified, entities will be returned in descending order of appearance.

`entity.type` - Limit entity search to specific entity type or types. Acceptable values are PERSON, ORG, LOC, GOVT, OTHER.

#### Examples

- Show all topics in the FRUS collection: `hlapi_overview(aspect='topics',coll.name='frus')`

- Show summary data for all collections: `hlapi_overview(aspect='collections')`

- Return all LOC and PERSON entities in descending order of appearance: `hlapi_overview(aspect='entities', entity.type = c('PERSON','LOC'), sort = "A")`

### hlapi_id()
This function will search the database for a document ID or list of documents IDs and return the resulting output. 

``hlapi_id(ids=NULL, fields=NULL, run = TRUE,...)``

`ids` - Document ID or list of document IDs to return. If an unknown ID is entered, the function will return nothing for the ID. 

#### Examples

- Return body and topics from specific FRUS document
`hlapi_id(ids='frus1969-76ve05p1d11', fields = c('doc_id','body','title','topic_titles'))`

- Return specific fields for two different document IDs
`hlapi_id(ids=c('frus1969-76ve05p1d11','frus1958-60v03d47'), fields = c('doc_id','body','title','topic_titles','entities'))`




### hlapi_entity()
This function will search the database for a particular entity of group of entities. History Lab uses Wikidata IDs to identify entities within the data so the entity.value option must contain a Wikidata ID. 

In order to find Wikidata IDs for entities in History Lab's databases, users can utilize the `find.entity.id` function. 

``hlapi_entity(entity.value=NULL, fields=NULL,coll.name=NULL,date=NULL,start.date=NULL, end.date=NULL,or=FALSE, run=TRUE, limit = 25,...)``


#### Examples

- Find Wikidata ID for China: `find.entity.id('China')`

- Find Wikidata ID for Henry Kissinger: `find.entity.id('Kissinger')`

- Return 1000 documents from FRUS and CFPF containing China: `hlapi_entity(entity.value=c('Q148'), coll.name='cfpf',fields = c('doc_id','authored','title','body'), limit=1000)`

- Find documents from FRUS containing both China and Kissinger: `hlapi_entity(entity.value=c('Q148','Q66107'), coll.name='frus')`

