library(RCurl)
library(httr)

execscript <- function(text){

  writeLines(text, con="input.R")
  source('input.R', local = TRUE)
}

.transmartServerGetRequestUi <- function(apiCall, auth_token)  {

  httpHeaderFields <- c(Authorization = paste("Bearer ", auth_token, sep=""))

  tryCatch(result <- .serverMessageExchangeUi(apiCall, httpHeaderFields),
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

.serverMessageExchangeUi <-
function(apiCall, httpHeaderFields, accept.type = "default", progress = .make.progresscallback.download()) {
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
        progress$start(NA_integer_)
        result <- list()
        h <- basicTextGatherer()
        result$content <- getBinaryURL(paste(sep="", transmartClientEnv$db_access_url, apiCall),
                .opts = list(headerfunction = h$update),
                noprogress = FALSE,
                progressfunction = function(down, up) {up[which(up == 0)] <- NA; progress$update(down, up) },
                httpheader = httpHeaderFields)
        progress$end()
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
