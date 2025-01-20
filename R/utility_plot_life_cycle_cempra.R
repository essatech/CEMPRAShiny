#' Plot a life cycle diagram from a matrix population model
#'
#' Plots the life cycle diagram illustrated by a matrix population model. This
#' function processes the matrix model and passes the information to the
#' graphViz function in DiagrammeR. Original function written by Owen R. Jones
#' in Rage and modified here
#' \url{https://rich-iannone.github.io/DiagrammeR/}.
#' \url{https://github.com/jonesor/Rage/blob/main/R/plot_life_cycle.R}.
#'
#' @param matA A matrix population model (i.e., a square projection matrix)
#' @param stages Optional vector of stage class labels. If missing, it first
#'   attempts to infer them from \code{dimnames(matA)}. If these are also
#'   \code{NULL}, then reverts to integers \code{1:ncol(A)}.
#' @param anadrmous (TRUE/FALSE) Using CEMPRA anadromous life history.
#' @param title Optional title for the plot. Defaults to \code{NULL}.
#' @param shape The shape to be used for the stages of the diagram. Any node
#'   shape accepted by \code{graphViz} is acceptable.
#' @param fontsize Size of the font used in the diagram.
#' @param nodefontsize Size of the font used in the node part of the diagram.
#' @param edgecol Colour of the arrows in the diagram.
#' @param node_order An optional numeric vector giving the order that the nodes
#'   should be presented in the plot. Default is `NULL` whereby the order is the
#'   same as `stages`, or row/column names, of the matrix.
#'
#' @return An object of class \code{grViz} representing the life cycle diagram
#'
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family visualisation
#'
#' @importFrom DiagrammeR grViz
#' @export utility_plot_life_cycle_cempra
utility_plot_life_cycle_cempra <- function(matA,
                                           stages,
                                           anadromous = FALSE,
                                           title = NULL,
                                           shape = "egg",
                                           fontsize = 10,
                                           nodefontsize = 12,
                                           edgecol = "grey",
                                           node_order = NULL) {
  # Identify stages
  if (missing(stages) && is.null(dimnames(matA))) {
    stages <- seq_len(ncol(matA))
  } else if (missing(stages) && !is.null(dimnames(matA))) {
    stages <- dimnames(matA)[[1]]
    
    if (!identical(dimnames(matA)[[1]], dimnames(matA)[[2]])) {
      message(
        strwrap(
          prefix = " ",
          initial = "",
          "Dimension names of 'matA' are not identical
      for rows and columns. Using row names."
        )
      )
    }
  }
  
  if (length(stages) != nrow(matA)) {
    stop("The length of stages does not equal the dimension of matA")
  }
  
  if (!is.null(node_order)) {
    if (length(node_order) != nrow(matA)) {
      stop("The length of node_order does not equal the dimension of matA")
    }
  }
  # Construct a "from" -> "to" graph dataset (edges)
  graph <- expand.grid(to = stages, from = stages)
  graph$trans <- round(c(matA), 3)
  
  # Subset to only include those where the trans > 0
  graph <- graph[graph$trans > 0, ]
  
  # Create vector of node names (add semicolon for use by graphViz)
  if (is.null(node_order)) {
    nodes <- paste(paste0("'", stages, "'"), collapse = " -> ")
  } else {
    stages <- stages[order(node_order)]
    nodes <- paste(paste0("'", stages, "'"), collapse = " -> ")
  }
  nodes <- paste0(nodes, " [style=invis]")
  
  # Manipulate minimim length of edge to make the plot pretty (experimental!)
  graph$min_len <- (as.numeric(graph$to) - as.numeric(graph$from)) * 3
  # Create the edges argument for graphviz by pasting commands together
  edges <- paste0(
    "'",
    graph$from,
    "'",
    " -> ",
    "'",
    graph$to,
    "'",
    "[minlen=",
    graph$min_len,
    ",fontsize=",
    fontsize,
    ",color=",
    edgecol,
    ",xlabel=",
    paste("\"", graph$trans),
    "\"]\n",
    collapse = ""
  )
  
  # ==============================================
  # Custom code for non-anadromous species
  # ==============================================
  
  if (!(anadromous)) {
    # The graphviz argument, pasted together
    return(grViz(
      paste(
        "
digraph {
  {
    graph[overlap=false];
    rank=same;
    node [shape=",
        shape,
        ", fontsize=",
        nodefontsize,
        "];",
        nodes,
        "
  }",
        "ordering=out
  x [style=invis]
  x -> {",
        nodes,
        "} [style=invis]",
        edges,
        "labelloc=\"t\";
  label=\"",
        title,
        "\"
}"
      )
    ))
  }
  
  # ==============================================
  # Custom code extension for anadromous species
  # ==============================================
  
  if (anadromous) {
    # Separate nodes by prefix
    stage_B_nodes <- grep("^stage_B_", stages, value = TRUE)
    stage_Pb_nodes <- grep("^stage_Pb_", stages, value = TRUE)
    
    # Construct the subgraph definitions
    cluster_top <- paste(
      "  subgraph cluster_top {",
      "    rank=same;",
      "    style=invis;",
      "    node [shape=egg, fontsize=12, color=red, fontcolor=red];",
      paste(sprintf("    '%s';", stage_B_nodes), collapse = "\n"),
      "  }",
      sep = "\n"
    )
    
    cluster_bottom <- paste(
      "  subgraph cluster_bottom {",
      "    rank=same;",
      "    style=invis;",
      "    node [shape=egg, fontsize=12, color=blue, fontcolor=blue];",
      paste(sprintf("    '%s';", stage_Pb_nodes), collapse = "\n"),
      "  }",
      sep = "\n"
    )
    
    return(grViz(
      paste(
        "
digraph {
  graph[overlap=false];

  // Top row: stage_B_ nodes
  ",
        cluster_top,
        "

  // Bottom row: stage_Pb_ nodes
  ",
        cluster_bottom,
        "

  // Define nodes and their attributes
  node [shape=",
        shape,
        ", fontsize=",
        nodefontsize,
        "];

  // Define edges
  ",
        edges,
        "

  // General graph settings
  labelloc=\"t\";
  label=\"",
        title,
        "\";
}
    "
      )
    ))
    
  }


}
