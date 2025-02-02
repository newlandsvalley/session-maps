session-maps
============

DEPRECATED.  This was an experimental project and purescript-openlayers has not been maintained beyond purescript 0.14.

Given that all our music-making takes place remotely thse days, I suppose this project is over-optimistic.  Provide StreetMap views of the venues for all the major Scandi sessions that take place in the UK.  The idea is that eventually it might be incorporated into [tradtunedb](http://www.tradtunedb.org.uk/#/).

This is a very simple application that takes a static list of venues, queries the [postcodes.io](https://postcodes.io/) service to get the latitude and longitude for each venue from the postcode and then centres the map appropriately.

Dependencies
------------

The parcel bundler:

```
    npm install -g parcel-bundler
```

The openlayers javascript API:

```
    npm install ol
```

To Build
--------

Continous builds of the purescript component can be enabled using:


```
    spago build --watch
```

Development builds into the _dist_ directory of the entire module can be enabled using:

```
    npm run dev
```

and then navigating to the appropriate localhost URL as indicated by parcel.

Production builds are enabled using:

```
    npm run build
```

