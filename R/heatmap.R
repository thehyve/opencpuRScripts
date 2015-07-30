require(gplots)
require(reshape2)

produceUI <- function () {
  step1 <- StepUI$new("Fetch data", "fetchData", "opencpuRScripts")

  step1$add_input(InConceptUI$new("Select a highdimensional concept", list("apiUrl", "auth.token", "study.name", "concept.link")))
  step1$add_input(InDropdownUI$new("Select a projection", "projection", list("zscore", "default_real_projection")))
  step1$add_ouput(OutInfoText$new("RUNNING", "Fetching data..."))
  step1$add_ouput(OutInfoText$new("DONE", "Data fetched, proceed to next step."))

  step2 <- StepUI$new("Pre-process data", "preprocessDataHeatmap", "opencpuRScripts")

  step2$add_input(InDropdownUI$new("Select a preprocessing option", "preprocess", list("zscore", "logfold")))
  step2$add_ouput(OutInfoText$new("RUNNING", "Pre-processing data..."))
  step2$add_ouput(OutInfoText$new("DONE", "Done."))

  step3 <- StepUI$new("Produce heatmap", "generateArtefactsHeatmap", "opencpuRScripts", final = TRUE)

  step3$add_ouput(OutInfoText$new("RUNNING", "Producing heatmap..."))
  step3$add_ouput(OutInfoText$new("DONE", ""))
  step3$add_ouput(OutImage$new("heatmap.png"))

  analysis <- AnalysisUI$new()
  analysis$add_step(step1)
  analysis$add_step(step2)
  analysis$add_step(step3)

  return(analysis$produce())
}

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
