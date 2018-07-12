# Pantry plans

Tying together:

* pantry
* Stackage
* Stack

## Goals

* Efficient, distributed package storage for Haskell
* Superset of existing storage mechanisms
* Security via content addressable storage
* Allow more Stackage-style snapshots to exist
* Allow authors to bypass Hackage for uploads
* Allow Stackage to create forks of packages on Hackage

## Next steps

* Get feedback from other interested parties on this proposal
* Invite those interested to a private Gitter chatroom for discussing
  implementation
* Continue development of the code

## Package definition

Pantry defines the following concepts:

* __Blob__: a raw byte sequence, identified by its key (SHA256 of the
  contents)
* __Tree entry__: contents of a single file (identified by blob key)
  and whether or not it is executable.
    * NOTE: existing package formats like tarballs support more
      sophisticated options. We explicitly do not support those. If
      such functionality is needed, fallback to those mechanism is
      required.
* __Tree__: mapping from relative path to a tree entry. Some basic
  sanity rules apply to the paths: no `.` or `..` directory
  components, no newlines in filepaths, does not begin with `/`, no
  `\\` (we normalize to POSIX-style paths). A tree is identified by a
  tree key (SHA256 of the tree's serialized format).
* __Package__: a tree key for the package contents, package name,
  version number, and cabal file blob key. Requirements: there must be
  a single file with a `.cabal` file extension at the root of the
  tree, and it must match the cabal file blob key. The cabal file must
  be located at `pkgname.cabal`. Each tree can be in at most one
  package, and therefore tree keys work as package keys too.

Note that with the above, a tree key is all the information necessary
to uniquely identify a package. However, including additional
information (package name, version, cabal key) in config files may be
useful for optimizations or user friendliness. If such extra
information is ever included, it must be validated to concur with the
package contents itself.

### Package location

Packages will optionally be sourced from some location:

* __Hackage__ requires the package name, version number, and revision
  number. Each revision of a package will end up with a different tree
  key.
* __Archive__ takes a URL pointing to a tarball (gzipped or not) or a
  ZIP file. An implicit assumption is that archives remain immutable
  over time. Use tree keys to verify this assumption. (Same applies to
  Hackage for that matter.)
* __Repository__ takes a repo type (Git or Mercurial), URL, and
  commit. Assuming the veracity of the cryptographic hashes on the
  repos, this should guarantee a unique set of files.

In order to deal with _megarepos_ (repos and archives containing more
than one package), there is also a subdirectory for the archive and
repository cases. An empty subdir `""` would be the case for a
standard repo/archive.

In order to meet the rules of a package listed above, the following
logic is applied to all three types above:

* Find all of the files in the raw location, and represent as `Map
  FilePath TreeEntry` (or equivalent).
* Remove a wrapper directory. If _all_ filepaths in that `Map` are
  contained within the same directory, strip it from all of the
  paths. For example, if the paths are `foo/bar` and `foo/baz`, the
  paths will be reduced to `bar` and `baz`.
* After this wrapper is removed, then subdirectory logic is applied,
  essentially applying `stripPrefix` to the filepaths. If the subdir
  is `yesod-bin` and files exist called `yesod-core/yesod-core.cabal`
  and `yesod-bin/yesod-bin.cabal`, the only file remaining after
  subdir stripping would be `yesod-bin.cabal`. Note that trailing
  slashes must be handled appropriately, and that an empty subdir
  string results in this step being a noop.

The result of all of this is that, given one of the three package
locations above, we can receive a tree key which will provide an
installable package. That tree key will remain immutable.

### How tooling refers to packages

We'll get to the caching mechanism for Pantry below. However, the
recommended approach for tooling is to support some kind of composite
of the Pantry keys, parsed info, and raw package location. This allows
for more efficient lookups when available, with a fallback when
mirrors don't have the needed information.

An example:

```yaml
extra-deps:
- name: foobar
  version: 1.2.3.4
  pantry: deadbeef # tree key
  cabal-file: 12345678 # blob key
  archive: https://example.com/foobar-1.2.3.4.tar.gz
```

It is also recommended that tooling provide an easy way to generate
such complete information from, e.g., just the URL of the tarball, and
that upon reading information, hashes, package names, and version
numbers are all checked for correctness.

## Stack

Stack currently supports Stackage snapshots and custom snapshots, as
well as modifications in the `stack.yaml` file. Stackage snapshots are
special-cased in that only `nightly-YYYY-MM-DD` and `lts-X.Y` are
supported. Both to ease migration to this new Pantry system, and as a
good generalization, we should allow for more distributed
specification. We also want to make more secure choices.

Snapshots will continue to be defined in a single YAML file, with an
exact format to be defined (based on the discussion above). Snapshots
can be referred to by their blob key (for higher security). They can
also be specified using the following rules:

* `lts-X.Y` is treated as `github:commercialhaskell/stackage-snapshots:lts/X/Y.yaml`
* `nightly-YYYY-MM-DD` is treated as `github:commercialhaskell/stackage-snapshots:nightly/YYYY/MM/DD.yaml`
* `service:org/repo:filepath` refers to a file located on a given
  service (Github, Gitlab, BitBucket, etc) on `org`'s account in the
  `repo` repo, at filepath `filepath`. So, for example, `lts-5.6`
  would end up referring to the file
  `https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/5/6.yaml`
* Any HTTP URL will validate as is
* All other values will be treated as local files relative to the `stack.yaml` file

Note that, ideally, both methods (blob key and YAML file reference)
would be used for clarity and security, e.g.:

```yaml
resolver:
  hash: 0987654
  snapshot: lts-10.1
```

Exact syntax is certainly up for debate.

## Stackage

There are two kinds of files relevant to the Stackage build process:

* Build constraints: specifies which packages are desired, where they
  come from, version restrictions on them, etc
* Snapshots: something consumable by Stack and other tooling

For a nightly build process, the basic goal is to:

* Convert build constraints into a snapshot file
* Verify version bounds specified by that snapshot file are met
* Perform a build/test/bench/etc cycle to make sure everything works
* Publish generated Haddocks and the snapshot file

For an LTS build process, there is one extra goal: produce a new build
constraints file to be used by the next minor run of an LTS
series. (Currently, this information is embedded in the snapshot
itself, which bloats the size of these files unnecessarily.)

A goal of this new Pantry-based system is to make the Stackage process
lightweight enough for arbitrary people to create their own snapshots.

## Pantry caching

One simplistic option for Pantry would be that, every time a piece of
data is needed, Pantry downloads the necessary tarball/Git
repo/etc. However, this would in practice be highly wasteful, since
downloading Git repos and archives just to get a single cabal file
(for plan construction purposes) is overkill. Instead, here's the
basic idea for how caching works:

* All data for Pantry can be stored in a SQL database. Local tools
  like Stack will use an SQLite database. Servers will use PostgreSQL.
* We'll define a network protocol (initially just HTTP, maybe
  extending to something more efficient if desired) for querying blobs
  and trees.
* When a blob or tree is needed, it is first checked for in the local
  SQLite cache. If it's not available there, a request to the Pantry
  mirrors (configurable) will be made for the data. Since everything
  is content addressable, it is safe to use untrusted mirrors.
* If the data is not available in a mirror, and a location is
  provided, the location will be downloaded and cached locally.

We may also allow these Pantry mirrors to provide some kind of query
interface to find out, e.g., the latest version of a package on
Hackage. That's still TBD.

## Rollout

There are many moving pieces here, and getting all the details right
to minimize disruption are important. Here's a sketch:

* Create a working Pantry library that allows checking out source code
* Run a Pantry mirror server which collects all packages from Hackage
* Define a new snapshot format which uses Pantry
* Create a branch of Stack that uses Pantry and the new snapshot format
* Create a new Stackage tool which produces the new snapshot format
* Release a new version of Stack with support for old and new snapshot formats
* Wait for users to upgrade (2 weeks?), and switch over to new Stackage tool
* Optional: convert all existing Stackage snapshots to the new format

## More advanced content discovery

There are three more advanced cases to consider:

* Providing fall-back locations for content, such as out of concern for a
  single URL being removed in the future
* Closed corporate setups, where access to the general internet may either be
  impossible or undesirable
* Automatic discovery of missing content by hash

The following extensions are possible to address these cases:

* Instead of a single package location, provide a list of package locations
  with fallback semantics.
* Corporate environments will be encouraged to run a local Pantry mirror, and
  configure clients like Stack to speak to these mirrors instead of the default
  ones (or in addition to).
* Provide some kind of federation protocol for Pantry where servers can
  registry with each other and requests for content can be pinged to each
  other.

Providing override at the client level for Pantry mirror locations is a
__MUST__. Making it easy to run in a corporate environment is a __SHOULD__.
Providing the fallback package locations seems easy enough that we should
include it initially, but falls under a __SHOULD__. The federated protocol
should be added on-demand.
