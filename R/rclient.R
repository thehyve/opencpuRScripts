# Copyright 2014 Janssen Research & Development, LLC.
#
# This file is part of tranSMART R Client: R package allowing access to
# tranSMART's data via its RESTful API.
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

# This file contains some potentially useful commands for people who wish to develop with this package

.transmartServerGetRequest <- function(apiCall, auth_token, accept.type)  {

  httpHeaderFields <- c(Authorization = paste("Bearer ", auth_token, sep=""))
  fullCall <- paste("http://transmart-gb.thehyve.net/transmart", apiCall, sep="")

  tryCatch(result <- .serverMessageExchange(fullCall, httpHeaderFields, accept.type),
           error = function(e) {
             message("Sorry, the R client was unable to carry out your request.",
                     "Please make sure that the transmart server is still running. \n\n",
                     "If the server is not down, you've encountered a bug.\n",
                     "You can help fix it by contacting us. Type ?transmartRClient for contact details.\n",
                     "Optional: type options(verbose = TRUE) and replicate the bug to find out more details.")
             stop(e)
           })
  result
}

.serverMessageExchange <-
function(apiCall, httpHeaderFields, accept.type) {
  if (any(accept.type == c("default", "hal"))) {
        if (accept.type == "hal") { httpHeaderFields <- c(httpHeaderFields, accept = "application/hal+json") }
        result <- list()
        h <- basicTextGatherer()
        result$content <- getURL(apiCall,
                verbose = getOption("verbose"),
                .opts = list(headerfunction = h$update),
                httpheader = httpHeaderFields)
        result$header <- parseHTTPHeader(h$value())
        if (getOption("verbose")) { message("Server response:\n\n", result, "\n") }
        if (is.null(result$content) || result$content == "null" || nchar(result$content) == 0) { return(NULL) }
        result$content <- RJSONIO::fromJSON(result$content, asText = TRUE, nullValue = NA)
        if (!result$header[which(names(result$header)=="status")] %in% c("200", "302")) {
            message("There was a problem with your request to the server:")
            message(result)
            return(result$content)
        }
        if (accept.type == "hal") { return(.simplifyHalList(result$content)) }
        return(result$content)
    } else if (accept.type == "binary") {
        result <- list()
        h <- basicTextGatherer()
        result$content <- getBinaryURL(apiCall,
                .opts = list(headerfunction = h$update),
                httpheader = httpHeaderFields)
        result$header <- parseHTTPHeader(h$value())
        if (getOption("verbose")) {
            message(paste("Server binary response header:", as.character(data.frame(result$header)), "", sep="\n"))
        }
        if (!result$header[which(names(result$header)=="status")] %in% c("200", "302")) {
            message("There was a problem with your request to the server:")
            message(result)
            return(result)
        }
        return(result)
    }
    return(NULL)
}

getStudies <- function(auth.token, name.match = "", as.data.frame = TRUE, cull.columns = TRUE) {

  serverResult <- .transmartServerGetRequest("/studies", auth.token, accept.type = "hal")
  listOfStudies <- serverResult$studies

  studyNames <- sapply(listOfStudies, FUN = function(x) { x[["id"]] })
  names(listOfStudies) <- studyNames
  listOfStudies <- listOfStudies[ grep(name.match, studyNames) ]

  if (as.data.frame) {
    dataFrameStudies <- .listToDataFrame(listOfStudies)
    if (cull.columns) {
      columnsToKeep <- match(c("id", "api.link.self.href", "ontologyTerm.fullName"), names(dataFrameStudies))
      if (any(is.na(columnsToKeep))) {
        warning("There was a problem culling columns. You can try again with cull.columns = FALSE.")
        message("Sorry. You've encountered a bug.\n",
                "You can help fix it by contacting us. Type ?transmartRClient for contact details.\n",
                "Optional: type options(verbose = TRUE) and replicate the bug to find out more details.")
      }
      return(dataFrameStudies[ , columnsToKeep])
    }
    return(dataFrameStudies)
  }

  listOfStudies
}

getHighdimData <- function(auth.token, study.name, concept.match = NULL, concept.link = NULL, projection = NULL) {


  if (is.null(concept.link) && !is.null(concept.match)) {
    studyConcepts <- getConcepts(auth.token, study.name)
    conceptFound <- grep(concept.match, studyConcepts$name)[1]
    if (is.na(conceptFound)) {
      warning(paste("No match found for:", concept.match))
    } else { concept.link <- studyConcepts$api.link.self.href[conceptFound] }
  }

  if (length(concept.link) == 0L) {
    warning("No concepts selected or found to match your arguments.")
    return(NULL)
  }

  serverResult <- .transmartServerGetRequest(paste(concept.link, "/highdim", sep=""), auth.token, accept.type = "hal")
  if (length(serverResult$dataTypes) == 0) {
    warning("This high dimensional concept contains no data.")
    return(NULL)
  }
  listOfHighdimDataTypes <- serverResult$dataTypes[[1]]

  if (!is.null(projection)) {
    matchingProjectionIndex <- which(names(listOfHighdimDataTypes$api.link) == projection)
    if (length(matchingProjectionIndex) > 0) {
      projectionLink <- listOfHighdimDataTypes$api.link[[matchingProjectionIndex]]
    } else { projection <- NULL }
  } else { projection <- NULL }

  if (is.null(projection)) {
    warning("No valid projection selected.\nSet the projection argument to one of the following options:\n",
            paste(listOfHighdimDataTypes$supportedProjections, "\n"))
    return(listOfHighdimDataTypes$supportedProjections)
  }
  message("Retrieving data from server. This can take some time, depending on your network connection speed. ",
          as.character(Sys.time()))
  serverResult <- .transmartServerGetRequest(projectionLink, auth.token, accept.type = "binary")
  if (length(serverResult$content) == 0) {
    warning("No data could be found. The server yielded an empty dataset. Returning NULL.")
    return(NULL)
  }

  return(.parseHighdimData(serverResult$content))
}

getConcepts <- function(auth.token, study.name, as.data.frame = TRUE, cull.columns = TRUE) {

  serverResult <- .transmartServerGetRequest(
    paste("/studies/", study.name, "/concepts", sep = ""), auth.token ,accept.type = "hal")
  if (is.null(serverResult)) return(NULL)
  listOfConcepts <- serverResult$ontology_terms

  if (as.data.frame) {
    dataFrameConcepts <- .listToDataFrame(listOfConcepts)
    if (cull.columns) {
      columnsToKeep <- match(c("name", "fullName", "api.link.self.href"), names(dataFrameConcepts))
      if (any(is.na(columnsToKeep))) {
        warning("There was a problem culling columns. You can try again with cull.columns = FALSE.")
        message("Sorry. You've encountered a bug.\n",
                "You can help fix it by contacting us. Type ?transmartRClient for contact details.\n",
                "Optional: type options(verbose = TRUE) and replicate the bug to find out more details.")
      }
      return(dataFrameConcepts[, columnsToKeep])
    }
    return(dataFrameConcepts)
  }

  listOfConcepts
}

.listToDataFrame <- function(list) {
  # TODO: (timdo) dependency on 'plyr' package removed; figure out whether dependency is present elsewhere, or remove dependency
  # add each list-element as a new row to a matrix, in two passes
  # first pass: go through each list element, unlist it and remember future column names
  columnNames <- c()
  for (i in 1:(length(list))) {
    list[[i]] <- unlist(list[[i]])
    columnNames <- union(columnNames, names(list[[i]]))
  }

  # second pass: go through each list element and add its elements to correct column
  df <- matrix(nrow = length(list), ncol = length(columnNames))
  for (i in 1:(length(list))) {
    df[i, match(names(list[[i]]), columnNames)] <- list[[i]]
  }
  colnames(df) <- columnNames

  # check whether list contains valid row names, and if true; use them
  if (is.null(names(list)) || is.na(names(list)) || length(names(list)) != length(list)) {
    rownames(df) <- NULL
  } else { rownames(df) <- names(list) }
  # convert matrix to data.frame
  as.data.frame(df, stringsAsFactors = FALSE)
}

.parseHighdimData <-
function(rawVector, .to.data.frame.converter=.as.data.frame.fast) {
    dataChopper <- .messageChopper(rawVector)

    message <- dataChopper$getNextMessage()
    header <- read(highdim.HighDimHeader, message)
    columnSpec <- header$columnSpec
    assays <- header$assay
    DOUBLE <- highdim.ColumnSpec$ColumnType$DOUBLE
    STRING <- highdim.ColumnSpec$ColumnType$STRING

    columns <- .expandingList(1000)

    noAssays <- length(assays)

    assayLabels <- .DollarNames(assays[[1]])[1:length(assays[[1]])]

    for (label in assayLabels) {

        # RProtoBuf does not support int64 on all platforms that we need to
        # support. Specifically, binaries from CRAN don't support it, which
        # means Windows platforms and the default OSX R distribution. Linux and
        # OSX with Homebrew download source packages from CRAN and compile
        # locally, so they don't have problems with int64.
        #
        # The problem actually is in Rcpp. CRAN does not allow use of 'long
        # long' types as it is considered non portable. Rcpp works around this
        # by conditionally including 64 bit int support if it is compiled
        # somewhere other than CRAN.
        #
        # The serialization format uses an int64 field for
        # highdim.Assay.assayId. Reading that field on platforms that do not
        # have int64 support results in an error. As a workaround we parse the
        # assayId field from the string representation of the assay. (This works
        # because the as.character() conversion calls a DebugString method in
        # the C++ protobuf library, so the RProtoBuf C++ code that CRAN sees
        # never touches the 64 bit integers directly.)

        if (label == "assayId") {
            columns$add(label, sapply(assays, function(a) sub("assayId: ", "", grep(label, strsplit(as.character(a),
                    split = "\n")[[1]], value = TRUE))))
        } else {
            columns$add(label, sapply(assays, function(a) a[[label]]))
        }
    }

    message("Received data for ", noAssays, " assays. Unpacking data. ", as.character(Sys.time()))

    totalsize <- length(rawVector)

    labelToBioMarker <- hash() #biomarker info is optional, but should not be omitted, as it is also part of the data

    while (!is.null(message <- dataChopper$getNextMessage())) {
        row <- read(highdim.Row, message)
        rowlabel <- row$label

        labelToBioMarker[[rowlabel]] <- (if(is.null(row$bioMarker)) NA_character_ else row$bioMarker)
        rowValues <- row$value

        if(length(rowValues) == 1) {
            # if only one value, don't add the columnSpec name to the rowlabel.
            columns$add(rowlabel, rowValues[[1]]$doubleValue)
            next
        }

        # Multiple columns, add the columnSpec name to the labels to differentiate them.
        for(i in 1:length(rowValues)) {
            entryName <- paste(rowlabel, columnSpec[[i]]$name, sep=".")
            type <- columnSpec[[i]]$type
            if(type == STRING) {
                columns$add(entryName, rowValues[[i]]$stringValue)
            } else if(type == DOUBLE) {
                columns$add(entryName, rowValues[[i]]$doubleValue)
            } else {
                warning("Unknown row type: ", type)
            }
        }

    }

    message("Data unpacked. Converting to data.frame. ", as.character(Sys.time()))

    data <- .to.data.frame.converter(columns$as.list(), stringsAsFactors=FALSE)

    if(all(is.na(values(labelToBioMarker)))) {
        message("No biomarker information available.")
        labelToBioMarker <- "No biomarker information is available for this dataset"
        return(list(data = data))
    } else {
        message("Additional biomarker information is available.\nThis function will return a list containing a dataframe ",
                "containing the high dimensional data and a hash describing which (column) labels refer to which bioMarker")
        return(list(data = data, labelToBioMarkerMap = labelToBioMarker))
    }
}


.make.progresscallback.parse <- function() {
    pb <- NULL
    lst <- list()
    lst$start <- function(total) {
        pb <<- txtProgressBar(min = 0, max = total, style = 3)
    }
    lst$update <- function(current, .total) {
        setTxtProgressBar(pb, current)
    }
    lst$end <- function() {
        close(pb)
    }

    lst
}

.messageChopper <- function(rawVector, endOfLastMessage = 0) {
    msbSetToOne <- as.raw(128)
    progressTotal <- length(rawVector)
    pb <- c()

    getNextMessage <- function() {
        # The protobuf messages are written using writeDelimited in the Java
        # protobuf library. Unfortunately the C++ and R versions don't support
        # that function natively. We manually read a varint32 from the
        # connection that indicates the size of the next message.

        if (endOfLastMessage >= length(rawVector)) { return(NULL) }
        # The last byte of the varint32 has its most significant bit set to one.
        varint32Size <- min(which((msbSetToOne & rawVector[(endOfLastMessage+1):(endOfLastMessage+5)]) == as.raw(0)))
        varint32Connection <- rawConnection(rawVector[(endOfLastMessage+1):(endOfLastMessage+varint32Size)],
                open = "r+b")
        class(varint32Connection) <- "connection"
        connection <- ConnectionInputStream(varint32Connection)
        close(varint32Connection)
        messageSize <- tryCatch(ReadVarint32(connection), error= function(e) NULL)
        if (is.null(messageSize)) return(NULL)
        endOfThisMessage <- endOfLastMessage + varint32Size + messageSize
        message <- rawVector[(endOfLastMessage+1+varint32Size):endOfThisMessage]
        endOfLastMessage <<- endOfThisMessage
        return(message)
    }

    getRawVectorIndex <- function() { return(endOfLastMessage) }

    return(list(getNextMessage = getNextMessage, getRawVectorIndex = getRawVectorIndex))
}


# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
.expandingList <- function(capacity = 10) {
    buffer <- vector('list', capacity)
    names <- character(capacity)
    length <- 0

    methods <- list()

    methods$double.size <- function() {
        buffer <<- c(buffer, vector('list', capacity))
        names <<- c(names, character(capacity))
        capacity <<- capacity * 2
    }

    methods$add <- function(name, val) {
        if(length == capacity) {
            methods$double.size()
        }

        length <<- length + 1
        buffer[[length]] <<- val
        names[length] <<- name
    }

    methods$as.list <- function() {
        b <- buffer[0:length]
        names(b) <- names[0:length]
        return(b)
    }

    methods
}


.as.data.frame.fast <- function(data, ...) {
    # Add X'es to column names that start with numbers or other strange characters
    colnames <- names(data)
    rowDataIndexes <- -grep('^([a-zA-Z]|\\.[^0-9])', colnames)
    colnames[rowDataIndexes] <- paste('X', colnames[rowDataIndexes], sep='')

    names(data) <- colnames
    attr(data, 'row.names') <- 1:length(data[[1]])
    class(data) <- 'data.frame'

    data
}

.simplifyHalList <- function(halList) {
  # rename _links element to api.link
  names(halList)[which(names(halList) == "_links")] <- "api.link"
  # remove embedded intermediate element and add its sub-elements to this level
  if ("_embedded" %in% names(halList)) {
    halList <- as.list(c(halList, halList[["_embedded"]]))
    halList[["_embedded"]] <- NULL
  }
  # recursion: apply this function to list-elements of current list
  if (length(halList) > 0) {
    for (elementIndex in 1:length(halList)) {
      if (is.list(halList[[elementIndex]])) {
        halList[[elementIndex]] <- .simplifyHalList(halList[[elementIndex]])
      }
    }
  }
  return(halList)
}
