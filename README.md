Geoheap
=======

Geoheap is an aggregator and visualization tool for social
location-based services. Currently it supports collecting tweets from
Twitter (using Twitter's streaming HTTP API) and images from Instagram
(using pubsubhubbub).

This project is much a work in progress, but feel free to look
around. It has the following dependencies:

 * MongoDB - storage of items
 * Solr - filtering/facetting
 * Lager - erlang logging framework
 * Statz - collecting realtime statistics
 * Cowboy - HTTP webserver
 
The current web frontend is pure HTML/Javascript with a JSON backend.


Installation
------------
```
rebar get-deps
make
```

Then copy `sample.config` to `prod.config` and change its contents to
match your API credentials. Then run the aggregator as follows:

```
erl -pa ebin deps/*/ebin -boot start_sasl -config prod geoheap
```

A single-core Solr (>= 3.4) needs to be listening on its default port
on localhost with the schema as provided in `priv/schema.xml`.
