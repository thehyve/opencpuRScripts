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

library(R6)
require(jsonlite)

# Classes for defining the UI of an analysis

AnalysisUI <- R6Class("AnalysisUI",
  public = list(
    steps = list(),
    initialize = function() {
    },
    add_step = function(step) {
      self$steps <- append(self$steps, step, after = length(self$steps))
    },
    produce = function(){
      return(lapply(self$steps, function(x){x$convert()}))
    }
  )
)

StepUI <- R6Class("StepUI",
  public = list(
    name = NA,
    final = NA,
    inputs = list(),
    outputs = list(),
    func = NA,
    package = NA,
    initialize = function(name, func, package, final = FALSE) {
      self$name <- name
      self$final <- final
      self$func <- func
      self$package <- package
    },
    add_input = function(inp) {
      self$inputs <- append(self$inputs, inp, after = length(self$inputs))
    },
    add_ouput = function(out) {
      self$outputs <- append(self$outputs, out, after = length(self$outputs))
    },
    convert = function(){
      return(list(name=self$name,
                  final=self$final,
                  func=self$func,
                  package=self$package,
                  inputs=lapply(self$inputs, function(x){x$convert()}),
                  outputs=lapply(self$outputs, function(x){x$convert()})))
    }
  )
)

InConceptUI <- R6Class("InConceptUI",
  public = list(
    title = NA,
    param = NA,
    type = "CONCEPT",
    initialize = function(title, param) {
      self$title <- title
      self$param <- param
    },
    convert = function(){
      return(list(title=self$title,
                  param=self$param,
                  type=self$type))
    }
  )
)

InFixedUI <- R6Class("InFixed",
 public = list(
   param = NA,
   value = NA,
   type = "INFIXED",
   initialize = function(param, value) {
     self$param <- param
     self$value <- value
   },
   convert = function(){
     return(list(param=self$param,
                 value=self$value,
                 type=self$type))
   }
 )
)

InDropdownUI <- R6Class("InDropdownUI",
   public = list(
     title = NA,
     param = NA,
     options = list(),
     type = "DROPDOWN",
     initialize = function(title, param, options) {
       self$title <- title
       self$param <- param
       self$options <- options
     },
     convert = function(){
       return(list(title=self$title,
                   param=self$param,
                   options=self$options,
                   type=self$type))
     }
   )
)

OutInfoText <- R6Class("OutInfoText",
  public = list(
    when = NA,
    message = NA,
    type = "INFOTEXT",
    initialize = function(when = list("RUNNING", "DONE"), message) {
      self$when <- when
      self$message <- message
    },
    convert = function(){
      return(list(when=self$when,
                  message=self$message,
                  type=self$type))
    }
  )
)

OutImage <- R6Class("OutImage",
  public = list(
   filename = NA,
   type = "IMAGE",
   initialize = function(filename) {
     self$filename <- filename
   },
   convert = function(){
     return(list(filename=self$filename,
                 type=self$type))
   }
  )
)
