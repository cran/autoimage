#' Setup main argument for autoimage, autopoints, etc.
#'
#' @param arglist_main The main argument of arglist (i.e., arglist$main)
#' @param nz The number of images/plots to display
#' @noRd
auto_main_setup = function(arglist_main, nz) {
  if (is.null(arglist_main)) {
    arglist_main = ""
  }
  if (nz != length(arglist_main) & length(arglist_main) != 1) {
    stop("length of main does not match number of plots to construct")
  }
  if (length(arglist_main) == 1) {
    main <- rep(arglist_main, nz)
  } else {
    main <- arglist_main
  }
  return(main)
}

#' Setup zlim argument for autopoints
#'
#' @param arglist_zlim The zlim eleemnt of arglist, i.e., arglist$zlim
#' @param z The z matrix of responses
#' @param common.legend A logical value indicating whether a common legend should be utilized
#' @noRd
autopoints_zlim_setup = function(arglist_zlim, z, common.legend) {
  # if arglist_zlim is null, set to the range of z
  # if there should be a common.legend
  # or a list of the ranges of the columns z if it's not
  # a common legend
  if (is.null(arglist_zlim)) {
    if (common.legend) {
      zlim = range(z, na.rm = TRUE)
    } else {
      zlim = lapply(seq_len(ncol(z)), function(i) {
        range(z[,i], na.rm = TRUE)
      })
    }
  } else {# if arglist_zlim isn't null
    if (!is.list(arglist_zlim) & !is.vector(arglist_zlim)) {
      stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
    }
    if (is.list(arglist_zlim)) {
      if (length(arglist_zlim) != ncol(z)) {
        stop("length of zlim list does not match number of plots to construct")
      }
      for (i in seq_along(arglist_zlim)) {
        if (length(arglist_zlim[[i]]) != 2) {
          stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
        }
      }
      zlim <- arglist_zlim
    } else {
      if (length(arglist_zlim) != 2) {
        stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
      }
      zlim <- rep(list(arglist_zlim), ncol(z))
    }
  }
  return(zlim)
}

#' Setup map information for autoimage, autpoints, etc.
#'
#' @param map A single character string specifying a map
#'            from the maps package
#' @noRd
map_setup = function(map) {
  if (length(map) != 1) {
    stop("map should be a single character string")
  }
  if (!is.character(map)) {
    stop("map should be a single character string")
  }
  if (!is.element(map, c("none", "county", "france", "nz", "state", "usa", 
                         "world", "world2", "italy", "lakes"))) {
    # future maps to add "china", "japan", "nzHires", "rivers",
    # "world2Hires", # "worldHires"
    stop("invalid map choice")
  } else {
    if (map == "county") {
      utils::data("countyMapEnv", package = "maps")
      map_lines <- maps::map("county", plot = FALSE)
    } else if (map == "france") {
      utils::data("franceMapEnv", package = "maps")
      map_lines <- maps::map("france", plot = FALSE)
    } else if (map == "nz") {
      utils::data("nzMapEnv", package = "maps")
      map_lines <- maps::map("nz", plot = FALSE)
    } else if (map == "state") {
      utils::data("stateMapEnv", package = "maps")
      map_lines <- maps::map("state", plot = FALSE)
    } else if (map == "usa") {
      utils::data("usaMapEnv", package = "maps")
      map_lines <- maps::map("usa", plot = FALSE)
    } else if (map == "world") {
      utils::data("worldMapEnv", package = "maps")
      map_lines <- maps::map("world", plot = FALSE)
    } else if (map == "world2") {
      utils::data("world2MapEnv", package = "maps")
      map_lines <- maps::map("world2", plot = FALSE)
    } else if (map == "italy") {
      utils::data("italyMapEnv", package = "maps")
      map_lines <- maps::map("italy", plot = FALSE)
    } else if (map == "lakes") {
      utils::data("lakesMapEnv", package = "maps")
      map_lines <- maps::map("lakes", plot = FALSE)
    }
  }
  return(map_lines)
}

#' Setup lines.arg for autoimage, autopoints, ec.
#'
#' @param arglist Argument list
#' @param proj Projection string
#' @noRd
lines_args_setup = function(arglist, proj) {
  # check if there are lines to plot
  arglist_lines <- arglist$lines
  if (!is.null(arglist_lines)) {
    if (!is.list(arglist_lines)) {
      stop("lines must be of class list with vectors x and y")
    }
    if (is.null(arglist_lines$x) | is.null(arglist_lines$y)) {
      stop("lines must be a list with vectors x and y")
    }
    if (length(arglist_lines$x) != length(arglist_lines$y)) {
      stop("The x and y vectors in lines must have he same length")
    }
  }
  lines.args <- arglist$lines.args
  lines.args$proj <- proj
  lines.args$x <- arglist_lines
  return(lines.args)
}

#' Setup text.args for autoimage, autopoints functions
#'
#' @param arglist Argument list
#' @param proj Projection string
#' @noRd
text_args_setup = function(arglist, proj) {
  text <- arglist$text
  arglist$text <- NULL
  if (!is.null(text)) {
    if (!is.list(text)) {
      stop("text must be a list")
    }
    if (is.null(text$x) | is.null(text$y)) {
      stop("text must be a list with vectors x, y, and (possibly) labels")
    }
    if (length(text$x) != length(text$y)) {
      stop("The x and y vectors in text should have the same length")
    }
    if (is.null(text$labels)) {
      text$labels <- seq_along(text$x)
    }
    if (length(text$x) != length(text$labels)) {
      stop("The x, y, and labels vectors in text should have the same length")
    }
    if (is.data.frame(text)) {
      text <- as.list(text)
    }
  }
  text.args <- arglist$text.args
  arglist$text.args <- NULL
  if (!is.null(text.args)) { 
    if (!is.list(text.args)) {
      stop("text.args must be a list")
    }
  }
  text.args$proj <- proj
  text.args$x <- text$x
  text.args$y <- text$y
  text.args$labels <- text$labels
  return(text.args)
}

#' Setup paxes.args for autoimage, autopoints, ets
#'
#' @param arglist List of arguments
#' @param proj projection
#' @noRd
paxes_args_setup = function(arglist, proj) {
  paxes.args <- arglist$paxes.args
  paxes.args$xlim <- arglist$xlim
  paxes.args$ylim <- arglist$ylim
  paxes.args$xaxp <- arglist$xaxp
  paxes.args$yaxp <- arglist$yaxp
  paxes.args$proj <- proj
  paxes.args$axis.args <- arglist$axis.args
  return(paxes.args)
}

#' Setup points.args for autoimage, autpoints, etc.
#'
#' @param arglist Argument list
#' @param proj Projection
#' @noRd
points_args_setup = function(arglist, proj) {
  # check if there are points to plot
  arglist_points <- arglist$points
  if (!is.null(arglist_points)) {
    if (!is.list(arglist_points)) {
      stop("points must be a list with vectors x and y")
    }
    if (is.null(arglist_points$x) | is.null(arglist_points$y)) {
      stop("points must be a list with vectors x and y")
    }
    if (length(arglist_points$x) != length(arglist_points$y)) {
      stop("The x and y vectors in points should have the same length")
    }
  }
  points.args <- arglist$points.args
  points.args$proj <- proj
  points.args$x <- arglist_points
  return(points.args)
}

#' Setup axes
#'
#' @param arglist List of arguments
#' @noRd
axes_setup = function(arglist) {
  if (is.null(arglist$axes)) {
    axes <- TRUE
  } else {
    axes <- arglist$axes
  }
  return(axes)
}


#' Clear arglist of information no longer needed
#'
#' @param arglist List of arguments
#' @noRd
arglist_clean = function(arglist, image = FALSE) {
  arglist$points.args <- NULL
  arglist$points <- NULL
  arglist$lines <- NULL
  arglist$lines.args <- NULL
  arglist$text <- NULL
  arglist$text.args <- NULL
  arglist$paxes.args <- NULL  
  arglist$axis.args <- NULL
  # will plot axes manually, if necessary
  arglist$axes <- FALSE
  # 
  # remove zlim and breaks since not relevant for plot
  if (!image) {
    arglist$zlim <- NULL
    arglist$breaks <- NULL
  }
  return(arglist)
}
