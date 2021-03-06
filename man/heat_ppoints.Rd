% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heat_ppoints.R
\name{heat_ppoints}
\alias{heat_ppoints}
\title{Display colored scatterplot for projected coordinates}
\usage{
heat_ppoints(
  x,
  y,
  z,
  legend = "horizontal",
  proj = "none",
  parameters,
  orientation,
  lratio = 0.2,
  map = "none",
  n = 5,
  ...
)
}
\arguments{
\item{x, y}{Numeric vectors of coordinates at which the
values in \code{z} are measured.}

\item{z}{A numeric vector containing the values to be
plotted.}

\item{legend}{A character string indicating where the
color scale should be placed.  The default is
\code{"horizontal"}.  The other valid options are
\code{"none"} and \code{"vertical"}.}

\item{proj}{A character string indicating what projection
should be used for the included \code{x} and \code{y}
coordinates.  The default is \code{"none"}.  The other
valid choices correspond to the \code{"projection"}
argument in the \code{\link[mapproj]{mapproject}}
function, which is used for the projection.}

\item{parameters}{A numeric vector specifying the values
of the \code{parameters} argument in the
\code{\link[mapproj]{mapproject}}.  This may be
necessary when \code{proj != "none"}.}

\item{orientation}{A vector
\code{c(latitude,longitude,rotation)} which describes
where the "North Pole" should be when computing the
projection.  See \code{\link[mapproj]{mapproject}} for
more details.}

\item{lratio}{A numeric value indicating the ratio of the
smaller dimension of the legend scale to the width of
the image.  Default is \code{lratio = 0.2}.}

\item{map}{The name of the map to draw on the image.
Default is \code{"none"}.  Other options include
\code{"world"}, \code{"usa"}, \code{"state"},
\code{"county"}, \code{"france"}, \code{"nz"} (New
Zealand), \code{"italy"}, \code{"lakes"}, and
\code{"world2"}, all from the \code{maps} package.}

\item{n}{integer giving the \emph{desired} number of
    intervals.  Non-integer values are rounded down.}

\item{...}{Additional arguments passed to the
\code{\link[graphics]{plot}} function.  e.g.,
\code{xlab}, \code{ylab}, \code{xlim}, \code{ylim},
\code{zlim}, etc.  Additionally, arguments that can be
used to further customize the plot (like adding lines
or points), as described in Details and Examples.}
}
\description{
\code{heat_ppoints} plots a "heated" scatterplot for
(potentially) projected locations. A color scale is
automatically provided with the scatterplot. The function
is similar in purpose to \code{\link[autoimage]{pimage}},
but the \code{z}-values are not interpolated. The color
scale can be changed by passing a vector of colors to
the \code{col} argument.
}
\details{
When \code{proj != "none"}, the
\code{\link[mapproj]{mapproject}} function is used to
project the \code{x} and \code{y} coordinates. In that
case, \code{proj} must correspond to one of the choices
for the \code{projection} argument in the
\code{\link[mapproj]{mapproject}} function.  Necessary
arguments for \code{\link[mapproj]{mapproject}} should be
provided via the \code{parameters} and \code{orientation}
arguments. See Examples and the
\code{\link[mapproj]{mapproject}} function.

Valid options for \code{legend} are \code{"none"},
\code{"horizontal"}, and \code{"vertical"}.  If
\code{legend = "none"}, then no color scale is provided.
If \code{legend = "horizontal"}, then a color scale is
included under the plot.  If \code{legend = "vertical"},
then a color scale is added to the right of the plot.

Lines can be added to each plot by passing the
\code{lines} argument through \code{...}.  In that case,
\code{lines} should be a list with components \code{x}
and \code{y} specifying the locations to draw the lines.
The appearance of the plotted lines can be customized by
passing a named list called \code{lines.args} through
\code{...}. The components of \code{lines.args} should
match the arguments of \code{\link[graphics]{lines}}.
See Examples.

Points can be added to each image by passing the
\code{points} argument through \code{...}.  In that case,
\code{points} should be a list with components \code{x}
and \code{y} specifying the locations to draw the points.
The appearance of the plotted points can be customized by
passing a named list called \code{points.args} through
\code{...}. The components of \code{points.args} should
match the components of \code{\link[graphics]{points}}.
See Examples.

Text can be added to each image by passing the
\code{text} argument through \code{...}.  In that case,
\code{text} should be a list with components \code{x} and
\code{y} specifying the locations to draw the text, and
\code{labels}, a component specifying the actual text to
write.  The appearance of the plotted text can be
customized by passing a named list called
\code{text.args} through \code{...}. The components of
\code{text.args} should match the components of
\code{\link[graphics]{text}}.  See Examples.

The legend scale can be modified by passing
\code{legend.axis.args} through \code{...}.  The argument
should be a named list corresponding to the arguments of
the \code{\link[graphics]{axis}} function.  See Examples.

The plot axes can be modified by passing
\code{axis.args} through \code{...}.  The argument should
be a named list corresponding to the arguments of the
\code{\link[graphics]{axis}} function.  The exception to
this is that arguments \code{xat} and \code{yat} can be
specified (instead of \code{at}) to specify the location
of the x and y ticks.  If \code{xat} or \code{yat} are
specified, then this overrides the \code{xaxt} and
\code{yaxt} arguments, respectively.  See the
\code{\link[autoimage]{paxes}} function to see how
\code{axis.args} can be used.

The legend margin can be customized by passing
\code{legend.mar} to \code{heat_ppoints} through \code{...}.
This should be a numeric vector indicating the margins of
the legend, identical to how \code{par("mar")} is
specified.

The various options of the labeling, axes, and legend are
largely independent.  e.g., passing \code{col.axis}
through \code{...} will not affect the axis unless it is
passed as part of the named list \code{axis.args}.
However, one can set the various \code{par} options prior
to plotting to simultaneously affect the appearance of
multiple aspects of the plot.  See Examples.  After
plotting, \code{reset.par()} can be used to reset the
graphics device options to their default values.
}
\examples{
data(co, package = "gear")
# heated scatterplot for data on an irregular grid
heat_ppoints(co$lon, co$lat, co$Al, legend = "v", map = "state")
reset.par()

# change color scale
heat_ppoints(co$lon, co$lat, co$Al, col = cm.colors(5))
reset.par()

# Use custom border, x and y limits, breaks for legend axis
data(copoly)
heat_ppoints(co$lon, co$lat, co$Al, legend = "h",
       xlab = "longitude", ylab = "latitude",
       proj = "bonne", parameters = 40,
       lines = copoly,
       lines.args = list(lwd = 2, col = "grey"),
       xlim = c(-109.1, -102),
       ylim = c(36.8, 41.1),
       breaks = seq(0, 10, len = 6))
reset.par()
}
\seealso{
\code{\link[graphics]{plot}},
  \code{\link[graphics]{axis}},
  \code{\link[autoimage]{pimage}}
}
