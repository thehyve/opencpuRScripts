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

#' Analysis
#'
#' Takes a list of steps and exposes a function "produceUI" that returns an R
#' object describing the UI.
#'
#' @param title Title of the analysis
#' @param ... A list of steps (the Step function should be used for this)
#' @return NULL
Analysis <- function (title, ...) {
  steps <- list(...)
  assign("produceUI", function(){return(steps)}, envir=parent.env(environment()))
  return(NULL)
}

#' Step
#'
#' Takes a list of inputs and outputs and returns an R object (list)
#' describing the step.
#'
#' @param title Title of the step.
#' @param func The function to be called at the end of this step.
#' @param package The package that contains the function \code{func}.
#' @param ... A list of inputs and/or outputs (the provided functions should
#' be used for this)
#' @return A list describing the step.
Step <- function (title, func, package, ...) {
  inputs <- list()
  outputs <- list()
  steps <- list(...)

  for(x in steps){
    if(x$io == "INPUT"){
      inputs <- append(inputs, list(x), after = length(inputs))
    }else if(x$io == "OUTPUT"){
      outputs <- append(outputs, list(x), after = length(outputs))
    }
  }

  return(list(
    title=title,
    func=func,
    package=package,
    outputs=outputs,
    inputs=inputs
  ))
}

#' ConceptInput
#'
#' Creates a list describing a concept input.
#'
#' @param title Title of the input.
#' @param param The name of the parameter to which the value of the input will
#' be associated in the function call of it's parent step.
#' @return A list describing the input.
ConceptInput <- function(title, param) {
  return(list(
    title=title,
    param=param,
    io="INPUT",
    type="CONCEPT"
  ))
}

#' DropdownInput
#'
#' Creates a list describing a dropdown input.
#'
#' @param title Title of the input.
#' @param param The name of the parameter to which the value of the input will
#' be associated in the function call of it's parent step.
#' @param options Options which will be displayed in the dropdown.
#' @return A list describing the input.
DropdownInput <- function(title, param, options) {
  return(list(
    title=title,
    param=param,
    options=options,
    io="INPUT",
    type="DROPDOWN"
  ))
}

#' InfoTextOutput
#'
#' Creates a list describing a text output.
#'
#' @param message The message to be displayed when the chosen event is triggered.
#' @param when The event triggering the display of the message.
#' @return A list describing the output.
InfoTextOutput <- function(message, when = list("RUNNING", "DONE")){
  return(list(
    when=when,
    message=message,
    io="OUTPUT",
    type="INFOTEXT"
  ))
}

#' ImageOutput
#'
#' Creates a list describing an image output.
#'
#' @param filename The filename of the image.
#' @return A list describing the output.
ImageOutput <- function(filename) {
  return(list(
    filename=filename,
    io="OUTPUT",
    type="IMAGE"
  ))
}
