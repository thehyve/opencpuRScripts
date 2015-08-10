# Copyright 2015 The Hyve B.V.
#
# This file is part of OpenCPU R Scripts
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version, along with the following terms:
#
#   1. You may convey a work based on this program in accordance with
#      section 5, provided that you retain the above notices.
#   2. You may convey verbatim copies of this program code as you receive
#      it, in any medium, provided that you retain the above notices.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

require(gplots)
require(reshape2)

Analysis(title = "Heatmap",
  Step( title = "Fetch data", func = "fetchData", package = "opencpuRScripts",

    ConceptInput( title = "Select a highdimensional concept",
                  param = list("apiUrl", "auth.token", "study.name", "concept.link")),

    DropdownInput(title = "Select a projection",
                  param = "projection",
                  options = list("zscore", "default_real_projection")),

    InfoTextOutput( when = "RUNNING",
                    message = "Fetching data..."),

    InfoTextOutput( when = "DONE",
                    message = "Data fetched, proceed to next step.")
  ),

  Step(title = "Pre-process data", func = "preprocessDataHeatmap", package = "opencpuRScripts",

    DropdownInput(title = "Select a preprocessing option",
                  param = "preprocess",
                  options = list("zscore", "logfold")),

    InfoTextOutput( when = "DONE",
                    message = "Done."),

    InfoTextOutput( when = "RUNNING",
                    message = "Pre-processing data...")

  ),
  Step(title = "Produce heatmap", func = "generateArtefactsHeatmap", package = "opencpuRScripts",

    ImageOutput(filename = "heatmap.png"),

    InfoTextOutput( when = "DONE",
                    message = ""),

    InfoTextOutput( when = "RUNNING",
                    message = "Producing heatmap...")
  )
)

# Step 1: fetch the data
# for instance: fetchData("CELL-LINE", "Normalised ratios", "default_real_projection")
fetchData <- function(apiUrl, auth.token, study.name, concept.match = NULL, concept.link = NULL, projection = NULL) {
    res <- getHighdimData(apiUrl, auth.token, study.name, concept.match, concept.link, projection)
    # ignore these lines, is possible implementation for keeping objects within session accessible between calls
    #resultKey <- paste(sample(c(letters, LETTERS, 1:9), 10, replace = T), collapse = "")
    #assign(resultKey, res, envir = .GlobalEnv)
    #resultKey
    res
}

# step 2: preprocess the data, data-argument is result from step 1
preprocessDataHeatmap <- function(data, preprocess = c("zscore", "logfold")) {
    # keep the relevant values as its own matrix
    colorValues <- data$data[ , -c(1:5), drop = FALSE]
    colorValues <- colorValues[ , c(1:100), drop = FALSE] # keep the matrix tiny
    if (preprocess == "zscore") {
        colorValues <- apply(colorValues, 2, scale)
    } else if (preprocess == "log") {
        colorValues <- log(colorValues, base = 10)
    }
    colorValues
}

toDataFrame <- function(x) {
  as.data.frame(x)
}

# step 3: generate artefacts, data-argument is result from step 2
generateArtefactsHeatmap <- function(data, type = c("png", "svg")) {
    fileName <- "heatmap"
    if (type == "svg") {
        fileName <- paste(fileName, ".svg", sep = "")
        svg(fileName)
    } else {
        fileName <- paste(fileName, ".png", sep = "")
        png(fileName)
    }
    heatmap.2(data, dendrogram = "column", Rowv = NA, scale = "none", trace = "none", margins = c(8,8))
    dev.off()
    fileName
}
