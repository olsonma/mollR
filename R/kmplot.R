#' Create a Kaplan Meier plot
#'
#' Creates a Kaplan Meier plot with the option of risk and cumulative event tables. This is essentially my favorite KM plot using the `survminer` package. Default is my favorite settings.
#' @param df a data set used to fit survival curves
#' @param km a survfit object
#' @param colors a vector of colors for survival curves, length equal to number of curves.
#' @param conf.int logical. if TRUE, plots confidence interval. Default is TRUE.
#' @param p.val logical. if TRUE, adds p-value to the plot. Default is FALSE.
#' @param break.int numeric. controls when time axis ticks are presented. Defaults to 6 units (often used for 6 months).
#' @param risk.table logical, or string. allowed values are those allowed in the `risk.table` in `survminer::ggsurvplot`. Default is "nrisk_cumcensor".
#' @param cumevents logical. logical value specifying whether to show or not the table of the cumulative number of events. Default is TRUE.
#' @param lgd character specifying the legend location. Default is "top"
#' @param ttl legend title
#' @param textsize text size. default is 4
#' @param ylab y-axis label
#' @param xlab x-axis label
#' @param height height of the survival plot. Default is 10. Ignored when risk.table = FALSE
#' @param surv.median.line a character for how median survival lines should be drawn. Allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal.
#' @param labs legend labels. If not specified, will use strata names.
#' @param text.size text size for axis text
#' @param title.size text size for plot titles
#' @param legend.size text size for legend
#' @details
#' @keywords kaplan meier, ggplot
#' @export
#' @examples
#' kmplot(df = lung, km =survfit(Surv(time, status) ~ sex, data = lung), surv.median.line = "hv", break.int = 365)
#'
#' survObj <- survfit(Surv(time, status) ~ ph.ecog, data = lung)
#' kmplot(df = lung, km = survObj, surv.median.line = "hv", break.int = 180, colors = c("red", "blue", "orange", "black"), labs = gsub("ph.ecog=", "", attr(survObj$strata, "names")))

kmplot <- function(df
                   , km
                   , colors
                   , conf.int = T
                   , p.val = F
                   , break.int = 6
                   , risk.table = "nrisk_cumcensor"
                   , cumevents = T
                   , lgd = "top"
                   , ttl = ""
                   , textsize = 4
                   , ylab = ""
                   , xlab = ""
                   , height = 10
                   , surv.median.line = "hv"
                   , labs
                   , text.size = 14
                   , title.size = 16
                   , legend.size = 12
){
  require(survminer)
  require(ggplot2)
  if(missing(km)) stop("km must be specified")
  if(class(km) != "survfit") stop("km must be of class survfit")
  if(missing(colors)) colors <- rep("black", length(km$strata))
  if(missing(labs)) labs <- attr(km$strata, "names")

  ggsurv <- ggsurvplot(km,
                       data = df,
                       break.time.by = break.int,
                       conf.int = conf.int,
                       palette = colors,
                       pval = p.val,
                       surv.plot.height = height,
                       surv.median.line = surv.median.line,
                       xlab = xlab,
                       ylab = ylab,
                       legend = lgd,
                       fontsize = textsize,
                       legend.labs = labs,
                       risk.table = risk.table,
                       cumevents = cumevents,
                       legend.title = "",
                       risk.table.height = 0.25,
                       tables.y.text = FALSE,
                       title = ttl,
                       ggtheme = theme_bw()
  )

  ggsurv$plot <- ggsurv$plot +
    theme(plot.title = element_text(size = title.size, color = "black", face = "plain"),
          axis.text = element_text(size = text.size),
          axis.title = element_text(size = text.size),
          legend.text = element_text(size = legend.size))

  ggsurv$table <- ggsurv$table +
    theme(plot.title = element_text(size = title.size, color = "black", face = "plain"),
          axis.text = element_text(size = text.size),
          axis.title = element_text(size = text.size),
          legend.text = element_text(size = text.size))

  ggsurv$cumevents <- ggsurv$cumevents +
    theme(plot.title = element_text(size = title.size, color = "black", face = "plain"),
          axis.text = element_text(size = text.size),
          axis.title = element_text(size = text.size),
          legend.text = element_text(size = text.size))

  ggsurv

}
