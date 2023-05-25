{-

This is a gargabe collected store for documents with an
eventlog for addition and deletion of documents.
The eventlog is append-only. It provides a stream of
events. Each event has the form

  (key, docId, version, hash, status)

- key is increasing
- docId is the ID of the document, version its version,
  and hash the hash of the content
- status is either 'add' or 'delete'.

The content is stored on the filesystem so that garbage
collection can remove the contents of old version. The
filename docId_version.ext, where ext is the file extension
the file store was initialized with.

Implementation notes:

There is a database table with columns:

- key (int, autoincrement)
- docId (string, ID of the document)
- version (int)
- hash (word128, stored hex-encoding as string)
- status ('add' or 'delete')

This DB is append only (event log)
On write of a document with ID i with content b:

- Compute hash h of i and b.
- If DB does not contain i:
  - Write content with version 1 to disk
  - Add row (i, 1, h, 'add') to DB
- If DB contains i with version v:
  - If hash stored for i is equal to b: done
  - Otherwise:
    - Write content with version v+1 to disk
    - Add row (i, v+1, h, 'add')
    - Delete content with version v from disk

This automically sets previous version to 'delete'.

On delete of document with ID i:

- Get version v for i from DB.
- Set status of (i, v) to 'delete'.
- Delete document from disk

Clients reading a document in some version might get an error
because the version might have been deleted. This is ok because
the following events tell the client which version to read.
-}
module Control.Computations.FlowImpls.FileStore () where
