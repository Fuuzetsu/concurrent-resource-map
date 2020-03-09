# concurrent-resource-map [![Build Status](https://travis-ci.com/Fuuzetsu/concurrent-resource-map.svg?branch=master)](https://travis-ci.com/Fuuzetsu/concurrent-resource-map)

User-counted resource map with automatic resource collection, aimed to
be used in concurrent setting.

Consider having some sort of resource that you need properly
initialised and cleaned up but only once and only for as long as there
are interested users (threads). Further, instead of only a single
resource, you want a collection of such resources, keyed on some
value.

This package implements a simple bracket-based scheme that manipulates
a resource map by counting number of initalisations and clean-ups: if
it detects that the cleanup is from the last user, it removes the
resource from the map all together. See the code/hackage for
documentation.
