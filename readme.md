rerest - work in progress
=========================

----
This is a **work in progress**. Final application might be anything between description bellow and a flying saucer/cookie dispenser combo.

----

Rerest is a **RE**STful **Re**cord **St**ore applying the **HATEOAS** constraints. As such, it can be automaticaly browsed by end-users, developers and software alike without the need for out-of-band information, even if application and URL structures evolve.

It is meant to be used as an Open Data record store, or as a simple internal Json depot.

Open Data record store:
-----------------------
rerest can directly be used as a data storage back-end, while at the same time being browsable as HTML without the need for a separate application layer: being a HyperMedia application, the representation of the data depends on the media the client wishes to receive, be it JSON, XML, HTML or even CSV (depending on the media types offered by the server).

Other Uses:
-----------
rerest can of course be used even if access to data is to be limited, behind a proxy or with an additional authentication layer. It's HyperMedia capabilities make it possible to serve as backend for multiple applications needing access to the same data but in different formats, without any additional "translation" layer.

rerest can also be used as the only "translation" layer you need, acting as a buffer for data stored elsewhere in another format. HyperMedia capabilities also apply to data coming in: rerest can accept Json, XML, name-value pairs, CSV, you name it, depending on media types supported by the application.

Supported Media Types
---------------------
Currently, _text/html_ and _application/json_ media types are intended to be supported.

Adding support for other MIME's isn't very hard, just needing a function to convert the internal data format to the one needed (or vice-versa for input).

---

Todo:
-----
 - pretty much everything, but at least the app skeleton is there already
