
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

`date` - restrict the documents retrieved to a given date. Dates should be in numeric format and use either ".","-", or "/" as separators between month, day, and year. The function first looks for a "MDY" or a "YMD" format but it will recognize a "DMY" format for days greater than 12.

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

### hlapi_date()
 Function to search the collections for documents appearing on a specific date or range of dates. 

#### Syntax
``hlapi_date(date=NULL,start.date=NULL,end.date=NULL,fields=NULL,coll.name=NULL,  limit = 25,run=TRUE,...)``

#### Examples
- Return all documents written on September 15, 1980: `hlapi_date(date='1980-09-15', fields=c("doc_id","classification","title"),coll.name="frus")`

- Return 10 documents written between January 1, 1947 and December 1, 1948: `hlapi_date(start.date='1947-01-01', end.date='12/01/1948', fields=c("doc_id","authored","title","topic_names") , limit=10)`


### hlapi_search()
 This function performs a full-text search for a word or phrase across all collections or within specific collections. 

#### Syntax
``hlapi_search<-function(s.text, fields=NULL, or = FALSE, start.date=NULL,end.date=NULL, coll.name=NULL, limit = 25,run=TRUE,...)``

`s.text` - List of words or phrases to find using a full-text search.

`or` - Specifies whether a search should be an "or" search or an "and" search. By default, the function will run an "and" search. To specify a "or" search, set the value of this option to TRUE.

#### Examples
- Search for documents containing either UDEAC or ASEAN: `hlapi_search(c('udeac','asean'),  or=TRUE,fields=c('doc_id','title','authored','topic_names'))`

- Search for phrase 'League of Nations' and return 5000 matching documents: 
`hlapi_search(c('league of nations'),  limit = 20, fields=c('doc_id','title'))`

- Search for phrase 'United Nations' in the CFPF and FRUS collections for the years 1974-1979:
`hlapi_search('united nations', coll.name=c('cfpf','frus'),  start.date="1974-01-01", end.date="1979-12-31")`


### hlapi_entity()
This function will search the database for a particular entity of group of entities. History Lab uses Wikidata IDs to identify entities within the data so the entity.value option must contain a Wikidata ID. 

In order to find Wikidata IDs for entities in History Lab's databases, users can utilize the `find.entity.id` function. 

``hlapi_entity(entity.value=NULL, fields=NULL,coll.name=NULL,date=NULL,start.date=NULL, end.date=NULL,or=FALSE, run=TRUE, limit = 25,...)``


#### Examples

- Find Wikidata ID for China: `find.entity.id('China')`

- Find Wikidata ID for Henry Kissinger: `find.entity.id('Kissinger')`

- Return 1000 documents from FRUS and CFPF containing China: `hlapi_entity(entity.value=c('Q148'), coll.name='cfpf',fields = c('doc_id','authored','title','body'), limit=1000)`

- Find documents from FRUS containing both China and Kissinger: `hlapi_entity(entity.value=c('Q148','Q66107'), coll.name='frus')`

### hlapi_topics()
Function to search for documents containing specific topics across collections. 

The `find.topics.id` command will search for a given term and return a list of topics and topic IDs that contain the term. The topic IDs can then be used in the `hlapi_topics()` function. 

#### Syntax
``hlapi_topics(topics.value=NULL, fields=NULL,coll.name=NULL,date=NULL,start.date=NULL,end.date=NULL,or=TRUE, run=TRUE, limit = 25,...)``

`topics.value` - required value which specifies the topic ID of the topic to search for. A list of topic IDs can be obtained using the `find.topics.id` function. 

`or` - Specifies whether a search should be an "or" search or an "and" search. By default, the function will run an "and" search. To specify a "or" search, set the value of this option to TRUE.

#### Examples

- Find topics that include 'iraq': `find.topics.id('iraq')`

- Find documents from the UN collection with topics from topic ID 25 (which includes Iraq): `hlapi_topics(topics.value='25', coll.name='un', fields=c('doc_id','title','authored','topic_names'))`

- Find topics that include 'soviet': `find.topics.id('soviet')`

- Find documents from the CIA collection with topic IDs 80, 102, 103:
`hlapi_topics(topics.value=c('80','102','103'), coll.name='cia', fields=c('doc_id','title','authored','topic_names'), limit=10)`

