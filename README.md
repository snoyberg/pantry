# pantry

## Design decisions

* We don't support all possible tarballs on Hackage. Some things are
  just silly and broken. If you want to use those tarballs, you have
  to download them as tarballs, not via pantry.

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

```
pantry --sqlite pantry.sqlite3 unpack-archive --url https://hackage.haskell.org/package/yesod-core-1.6.6/yesod-core-1.6.6.tar.gz --dest yesod-core-1.6.6
pantry --sqlite pantry.sqlite3 unpack-archive --url https://github.com/snoyberg/yaml/archive/yaml-0.8.32.tar.gz --dest yaml-0.8.32
pantry --sqlite pantry.sqlite3 unpack-archive --url https://github.com/yesodweb/yesod/archive/yesod-bin-1.6.0.3.tar.gz --subdir yesod-bin --dest yesod-bin-1.6.0.3
```
