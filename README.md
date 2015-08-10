# opencpuRScripts

This package provides the server-side prototype of how a tranSMART advanced
analysis could be described in R and served through a RESTful API. An example
for a heatmap analysis is provided in the *example* folder.

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
describing the user interface for the analysis.

## Describing the steps of an analysis

Each step of an analysis if represented by a function. The name of this function
must be provided as the `func` parameter of the `Step()` function.
