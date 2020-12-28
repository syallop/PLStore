# PLStore - experimental

This repository includes several packages, defining interfaces and a variety of implementations for storing key-value associations:
- Traditionally
  - Store `key = value`
  - Lookup `key` get `value`
- By key prefix
  - Store `key = value`
  - Lookup `short key` get `value`
- By hash `hash value = value`
  - Store `value` implicitly by it's `hash value`
  - Lookup `hash value` get `value`

This functionality is currently required by the [PL](https://github.com/syallop/PL) project.

## Interfaces
Interfaces are defined in `PLStore` (/Store) including:
- `Store` (Things which can be stored and looked up by key)
- `ShortStore` (`Store`s whose keys have 'short' forms which may be used for lookup).

## Implementations
Several implementations exist:
- `Memory` (store key-value associations in memory)
- `File` (store values in the file system, where the path is defined by a pattern on the key)
- `DHT` (store values against their key in a Distributed Hash Table)
- `Nested` (store key-values in nested `Stores` preferring the top but deferring to the bottom where neccessary)

`Hash` is built ontop of `Store` & `ShortStore` and associates values to their hashes. I.E. The key is the hash of the value.

