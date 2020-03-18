# Changelog for concurrent-resource-map

## Unreleased changes

## 0.2.0.0

* Add `withInitialisedResource` function for querying possibly-missing
  resources.

* Abstract away the underlying map type. You're no longer forced to
  use `Data.Map`. The downside is that you're forced to choose now via
  an instance. This hapens to keep dependencies even more minimal.

## 0.1.0.0

* Initial release.
