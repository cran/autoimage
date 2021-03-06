#' @name narccap
#' @aliases lon lat tasmax
#' @title Maximum daily surface air temperatures on a grid.
#' @description These data are taken from the North American Regional 
#'   Climate Change Assessment Program (NARCCAP). Specifically, the data 
#'   provide maximum daily surface air temperature (K) (abbreviated 
#'   tasmax) for locations in the United States, Mexico, and Canada 
#'   for the five consecutive days of May 15, 2041 to May 19, 2041
#'   simulated using the Canadian Regional Climate Model (Caya and
#'   Laprise, 1999) forced by the Community Climate System Model
#'   atmosphere-ocean general circular model (Collins et al., 2006)
#' @docType data
#' @usage data(narccap)
#'   
#' @format Contains: \describe{ \item{lon}{A 140\eqn{\times}115 matrix
#'   of longitude coordinates.} \item{lat}{A 140\eqn{\times}115 matrix
#'   of latitude coordinates.} \item{tasmax}{A
#'   140\eqn{\times}115\eqn{\times}5 array of tasmax values.} }
#' @source The National Center for Atmospheric Research (NCAR) through the 
#'   North American Regional Climate Change Assessment Program (NARCCAP) 
#'   <doi:10.5065/D6RN35ST>.
#' @references Mearns, L.O., et al., 2007, updated 2012. The North
#'   American Regional Climate Change Assessment Program dataset,
#'   National Center for Atmospheric Research Earth System Grid data
#'   portal, Boulder, CO. Data downloaded 2016-08-12.
#'   <doi:10.5065/D6RN35ST>.
#'   
#'   Mearns, L. O., W. J. Gutowski, R. Jones, L.-Y. Leung, S.
#'   McGinnis, A. M. B. Nunes, and Y. Qian: A regional climate change
#'   assessment program for North America. EOS, Vol. 90, No. 36, 8
#'   September 2009, pp. 311-312.
#'   
#'   D. Caya and R. Laprise. A semi-implicit semi-Lagrangian regional
#'   climate model: The Canadian RCM. Monthly Weather Review,
#'   127(3):341-362, 1999. 
#'   
#'   M. Collins, B. B. Booth, G. R. Harris, J.M. Murphy, D. M. Sexton, 
#'   and M. J. Webb. Towards quantifying uncertainty in transient 
#'   climate change. Climate Dynamics, 27(2-3):127-147, 2006.
NULL
