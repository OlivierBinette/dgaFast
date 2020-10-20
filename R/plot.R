#' UpSetR plot of MSE data
#'
#' @param mse_data todo
#' @param sets.bar.color todo
#' @param main.bar.color todo
#' @param matrix.color todo
#' @param mainbar.y.label todo
#' @param sets.x.label todo
#' @param ... todo
#'
#' @export
plotMSE <- function(mse_data,
                         sets.bar.color = "cornflowerblue",
                         main.bar.color = "cornflowerblue",
                         matrix.color = "cornflowerblue",
                         mainbar.y.label = "Count",
                         sets.x.label = "",
                         ...) {
  assert(is.MSEdata(mse_data))

  n = length(list.names(mse_data))
  dat = tidyr::uncount(mse_data[, list.names(mse_data)], mse_data$count)
  UpSetR::upset(data.frame(dat), nsets=n,
                sets.bar.color = sets.bar.color,
                main.bar.color = main.bar.color,
                matrix.color = matrix.color,
                mainbar.y.label = mainbar.y.label,
                sets.x.label = sets.x.label,
                ...)
}

#' @export
plotPosteriorN <- function(weights, N, main = NULL) {
  graphics::plot(N, colSums(weights), type = "l", col = "black", lwd = 3, ylab = "Posterior Probability of N", xlab = "N", ylim = c(0, 1.25 * max(colSums(weights))))
  graphics::title(main)
  wts <- rowSums(weights)
  for (i in 1:nrow(weights)) {
    graphics::lines(N, weights[i, ], lwd = wts[i] * 3, lty = "dashed")
  }
  graphics::legend("topright", legend = c("Averaged Post. Prob.", "Post. Prob. By Model"), lty = c(1, 2), cex = .75)
}

#' Plot decomposable graphical model
#'
#' @param graph decomposable graph as in `data(graphs5)`.
#' @param labels Parameter passed to `qgraph::qgraph` with default TRUE.
#' @param layout Parameter passed to `qgraph::qgraph` with default "circle".
#' @param normalize Parameter passed to `qgraph::qgraph` with default FALSE.
#' @param edge.color Parameter passed to `qgraph::qgraph` with default "black".
#' @param edge.width Parameter passed to `qgraph::qgraph` with default 1.5.
#' @param label.font Parameter passed to `qgraph::qgraph` with default 2.
#' @param label.norm Parameter passed to `qgraph::qgraph` with default "OO".
#' @param bg Parameter passed to `qgraph::qgraph` with default `rgb(1,1,1,0)` for a transparent background.
#' @param ... Further arguments passed to `qgraph::qgraph`.
#'
#' @export
plotGraph <- function(graph,
                      labels=TRUE,
                      layout="circle",
                      normalize=FALSE,
                      edge.color="black",
                      edge.width=1.5,
                      label.font=2,
                      label.norm="OO",
                      bg=grDevices::rgb(1,1,1,0),
                      ...) {
  p = length(unique(unlist(graph$C)))

  qgraph::qgraph(adjMat(graph, p),
         labels=labels,
         layout=layout,
         normalize=normalize,
         edge.color=edge.color,
         edge.width=edge.width,
         label.font=label.font,
         label.norm=label.norm,
         bg=bg,
         ...)
}


