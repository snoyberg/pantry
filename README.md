# pantry

## Tables

* Blobs
* Trees
* Hackage tarball
    * package name
    * version
    * tree id
* Hackage revision
    * hackage tarball ID
    * rev num
    * blob ID
* Archive
    * URL
    * subdir
    * tree id
    * blob id (cabal file)
* Repo
    * Type
    * URL
    * Commit
    * subdir
    * tree id
    * blob id (cabal file)

## Filler API

* Grab the latest commit from a certain branch/tag and fill Repo table
* Fill archive table
* Update Hackage tables from a Hackage 01-index.tar file

## Fetch API

* Populate local from a remote pantry, maybe pulling from actual sources if needed
* Batch
* Fetch by pantry IDs exclusively
* Return tree IDs for each thing

## Unpack API

* Return a Tree for a Tree ID, optionally fetching if needed, take
  optional revision info for Hackage

## Query API

## CLI

* Unpack (same as unpack API)
