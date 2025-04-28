
# R interface for History Lab API 

This package for R offers a series of commands to use the History Lab API in order to download data from our collections. For instuctions on how to use the commands, see the [Quick Start guide](quickstart.MD).

## Collections

The History Lab API has documents available from 11 different collections, as detailed below. The R functions will search across all collections, unless a specific collection or set of collections is specified. 

|corpus|title|starts|ends|documents
|----|----|----|----|----|
|frus|Foreign Relations of the United States|1620|1989|311866|
|cia|CIA CREST Collection|1941|2005|935716|
|clinton|Clinton E-Mail|2009|2013|54149|
|briefing|Presidential Daily Briefings|1946|1977|9680|
|cfpf|State Department Central Foreign Policy Files|1973|1979|3214293|
|kissinger|Kissinger Telephone Conversations|1973|1976|4552|
|nato|NATO Archives|1949|2013|46002|
|un|United Nations Archives|1997|2016|192541|
|worldbank|World Bank McNamara Records|1942|2020|128254|
|cabinet|UK Cabinet Papers|1907|1990|42539|
|cpdoc|Azeredo da Silveira Papers|1973|1979|10279|

## Fields
The API can return many different fields which are described in the table below. By default, the API functions will only return _doc_id_, _authored_, and _title_. 

|field|title|
|----|----|
|authored|Document date|
|wikidata_ids|Wikidata IDs of entities in document|
|entgroups|Entity types appearing in document|
|entities|Standardized entity names in document|
|topic_names|Top 5 tokens for each topic|
|topic_scores|Topic score for each topic listed|
|topic_ids|ID number for each topic (used to search by topic)|
|title|Document title|
|classification|Original classification of document (if known)|
|corpus|Collection document is from|
|doc_id|Unique document identifier|


## Functions

There are a number of functions in this package 

``hlapi_overview`` - This function provides an overview of the collections, topics, and entities across our documents. 

``hlapi_id`` - This function will return data for a specific document ID or for a list of document IDs. Because document IDs are unique across all collections, documents can be returned from multiple collections. 

``hlapi_search`` - This function performs a full-text search for a word or phrase across all collections or within specific collections. 

``hlapi_date`` - Function to search the collections for documents appearing on a specific date or range of dates. 

``hlapi_entity`` - Function to search for entities across collection. (Note: the search requires Wikidata IDs)

``hlapi_topics`` - Function to search for documents cotaining specific topics across collections. 

