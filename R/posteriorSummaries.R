#' Table summary of posterior distributions
#'
#' This function computes the population size Bayes estimator, posterior mode and posterior quantiles for models with top posterior probability. Requires the `graphbox` and `booktabs` LaTeX package.
#'
#' @usage posteriorSummaryTable(weights, N, levels=c(0.025, 0.975), nrows = 10, ndigits=3)
#'
#' @param weights Matrix of posterior weights for the population size. Each row should be a (possibly un-normalized) posterior distribution on the parameter `N`, as in the result of `bma.cr`.
#' @param N Population sizes under consideration. Each row of `weights` represents a distribution on `N`.
#' @param levels Posterior quantiles. Default is `c(0.025, 0.975)`.
#' @param nrows Posterior summaries are given for the `nrows` models with top posterior probability. Default is 10.
#' @param ndigits Rounding number of digits for the model-wise posterior probabilities. Default is 3.
#'
#' @return Dataframe with columns `Model` for the model indices (as rows of the `weights` argument), `Prob.` for the posterior probabilities, `Bayes. est` for the Bayes estimators, `Mode` for the posterior and columns corresponding to posterior quantile levels.
#'
#' @examples
#' delta <- .5
#' Y <- c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
#' Y <- array(Y, dim=c(2,2,2,2,2))
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#' data(graphs5)
#' weights <- bma.cr(Y,  Nmissing, delta, graphs5)
#'
#' # Compute summaries for the top 5 models
#' posteriorSummaryTable(weights, N, nrows=5)
#'
#' # Compute summaries for the full posterior distribution
#' posteriorSummaryTable(colSums(weights), N)
#'
#' @export
posteriorSummaryTable <- function(weights, N, levels=c(0.025, 0.975), nrows = 10, ndigits=3) {
  if (is.vector(weights)) {
    weights = matrix(weights, ncol=length(weights))
  }

  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  models = order(-rowSums(weights))[1:min(nrows, nrow(weights))]
  weights = weights[models, , drop=FALSE]

  cols = c("Model", "Prob.", "Bayes est.", "Mode", levels)

  df = data.frame(models,
                  round(rowSums(weights), ndigits),
                  round(bayesEstimator(weights, N)),
                  round(posteriorMode(weights, N)),
                  round(posteriorQuantiles(weights, N, levels)))
  colnames(df) = cols

  return(df)
}

#' Save summary to html with plots and graphs
#'
#' @usage htmlSummary(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10, size=60, subplots=TRUE, bg=grDevices::rgb(1,1,1,0), auto=TRUE, ...)
#'
#' @param filepath Filepath (without ".html" file extension) at which to save the html table. Figures are saved in the same folder.
#' @param weights Matrix of posterior weights for the population size. Each row should be a (possibly un-normalized) posterior distribution on the parameter `N`, as in the result of `bma.cr`.
#' @param N Population sizes under consideration. Each row of `weights` represents a distribution on `N`.
#' @param graphs Optional list of decomposable graphs corresponding to the model for each row of `weights`.
#' @param levels Posterior quantiles. Default is `c(0.025, 0.975)`.
#' @param nrows Posterior summaries are given for the `nrows` models with top posterior probability. Default is 10.
#' @param size Pixel height of the svg figures included in the html table. Default is 60.
#' @param subplots Set to TRUE to plot posterior distributions in the table, FALSE otherwise. Default is true.
#' @param bg Figures background color. Default is `rgb(1,1,1,0)` for a transparent background.
#' @param auto Parameter passed to `xtable::xtable`. Set to TRUE for auto-formatting of table entries.
#' @param ... Other parameters passed to xtable::xtable in order to format the table.
#'
#' @export
htmlSummary <- function(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10, size=60, subplots=TRUE, bg=grDevices::rgb(1,1,1,0), auto=TRUE, ...) {

  postSummary = posteriorSummaryTable(weights, N, levels, nrows)

  cols = colnames(postSummary)
  # Prepare space for posterior plots
  if (subplots) {
    postSummary$Posterior = postSummary$Model
    postSummary = postSummary[, c(cols[1], "Posterior", cols[2:length(cols)])]
  }

  table = xtable::xtable(postSummary, auto=auto, ...)

  if (!is.null(graphs)) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, ".fig", i, ".svg")
      grDevices::svg(file, width=3, height=3, bg=bg)
      plotGraph(graphs[[model_index]], bg=bg)
      grDevices::dev.off()

      table[i,"Model"] = paste0("<img src='", file, "' width=", size, ">")
    }
  }
  if (subplots) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, ".posterior", i, ".svg")
      grDevices::svg(file, width=2, height=3/2, bg=bg)
      graphics::par(mar=c(2,0,2,0))
      graphics::plot(N, weights[model_index,]/sum(weights[model_index,]),
           lwd=2, type="l", xlab="", ylab="", axes=F)
      graphics::axis(1)
      grDevices::dev.off()

      table[i,"Posterior"] = paste0("<img src='", file, "' width=", size, ">")
    }
  }

  xtable::print.xtable(table, file=paste0(filepath, ".html"), type="html", sanitize.text.function=function(x) x, include.rownames=FALSE, html.table.attributes="")
}

#' Save summary to LaTeX with plots and graphs
#'
#' LaTex plots and summaries. Requires the `graphbox` and `booktabs` packages.
#'
#' @note Please include the `graphbox` and `booktabs` packages in your LaTex document.
#'
#' @usage latexSummary(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10, height="0.5in", subplots=TRUE, bg=grDevices::rgb(1,1,1,0), auto=TRUE, ...)
#'
#' @param filepath Filepath (without ".tex" file extension) at which to save the LaTeX table. PDF figures are saved in the same folder.
#' @param weights Matrix of posterior weights for the population size. Each row should be a (possibly un-normalized) posterior distribution on the parameter `N`, as in the result of `bma.cr`.
#' @param N Population sizes under consideration. Each row of `weights` represents a distribution on `N`.
#' @param graphs Optional list of decomposable graphs corresponding to the model for each row of `weights`.
#' @param levels Posterior quantiles. Default is `c(0.025, 0.975)`.
#' @param nrows Posterior summaries are given for the `nrows` models with top posterior probability. Default is 10.
#' @param height Height of the pdf figures included in the LaTeX table. Default is "0.5in".
#' @param subplots Set to TRUE to plot posterior distributions in the table, FALSE otherwise. Default is true.
#' @param bg Figures background color. Default is `rgb(1,1,1,0)` for a transparent background.
#' @param auto Parameter passed to `xtable::xtable`. Set to TRUE for auto-formatting of table entries.
#' @param ... Other parameters passed to xtable::xtable in order to format the table.
#'
#' @export
latexSummary <- function(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10, height="0.5in", subplots=TRUE, bg=grDevices::rgb(1,1,1,0), auto=TRUE, ...) {

  postSummary = posteriorSummaryTable(weights, N, levels, nrows)

  cols = colnames(postSummary)
  # Prepare space for posterior plots
  if (subplots) {
    postSummary$Posterior = postSummary$Model
    postSummary = postSummary[, c(cols[1], "Posterior", cols[2:length(cols)])]
  }

  table = xtable::xtable(postSummary, auto=auto, ...)

  if (!is.null(graphs)) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, "_fig", i, ".pdf")
      grDevices::pdf(file, width=3, height=3, bg=bg)
      plotGraph(graphs[[model_index]], bg=bg)
      grDevices::dev.off()

      table[i,"Model"] = paste0("\\includegraphics[height=",height,",align=c]{", file, "}")
    }
  }
  if (subplots) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, "_posterior", i, ".pdf")
      grDevices::pdf(file, width=2, height=3/2, bg=bg)
      graphics::par(mar=c(2,0,2,0))
      graphics::plot(N, weights[model_index,]/sum(weights[model_index,]),
                     lwd=2, type="l", xlab="", ylab="", axes=F)
      graphics::axis(1)
      grDevices::dev.off()

      table[i,"Posterior"] = paste0("\\includegraphics[height=",height,",align=c]{", file, "}")
    }
  }

  xtable::print.xtable(table, file=paste0(filepath, ".tex"), type="latex", sanitize.text.function=function(x) x, include.rownames=FALSE, booktabs=TRUE)
}

