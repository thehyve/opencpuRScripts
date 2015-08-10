# opencpuRScripts

This package provides the server-side prototype of how a tranSMART advanced
analysis could be described in R and served through a RESTful API.

The goal is to provided an easy way to build an an advanced analysis plugin
as an R package using a UI definition language library.

An example for a heatmap analysis is provided in the *example* folder.

## Describing the UI of an analysis

The functions contained in the *uiDefinition.R* file demonstrate how R can be
used to create a simple UI definition language. The functions can be embedded
within each other, such as:

```
Analysis(
  Step(
    DropdownInput(...),
    InfoTextOutput(...),
    ...  
  ),
  Step(
    ...  
  ),
  ...
)
```

The `Analysis()` function, when called, exposes in the package another function
called "produceUI" which can be called by the front-end to fetch the R object
describing the user interface for the analysis. This R object is an array of
named lists which can be easily converted to JSON by the openCPU server.

```
Analysis(title = "Heatmap",

  Step( title = "Fetch data",
        func = "fetchData",
        package = "opencpuRScripts",

    DropdownInput(title = "Select a projection",
                  param = "projection",
                  options = list("zscore", "default_real_projection")),

    InfoTextOutput( when = "RUNNING",
                    message = "Fetching data..."),
  ...

```

## Describing the steps of an analysis

Each step of an analysis if represented by a function. The name of this function
must be provided as the `func` parameter of the `Step()` function. The output of
a function is expected to be provided as the `data` parameter of the subsequent
function.

## Serving the analysis through openCPU

The OpenCPU server provides a clean and simple API interface to R. The
functions of an R package installed on the server are available through the API.

```
/ocpu/library/[packageName]/R/[functionName]
```

Once a function is called with a POST request, the result can retrieved with the
returned unique token.

```
/ocpu/tmp/[session key]/
```

Thus when an R package representing an analysis is available on the server, the
`produceUI()` can be called by the front-end to fetch a JSON file describing the
UI for the analysis. The steps of the analysis can be executed in a similar way.
