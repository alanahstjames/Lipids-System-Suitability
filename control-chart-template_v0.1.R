#' ---
#' title: Targeted Lipids Control Chart Report
#' author: ANPC
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: html_document
#' ---
#'   
#' ### Project Summary
knitr::opts_chunk$set(
  echo = FALSE,
  fig.height = 25,
  fig.width = 16, 
  fig.align = 'center' 
)
#' 
knitr::kable(master_list$summary_tables$project_summary)
#' 
#' 
#' ### SIL ISTD Peak Areas
knitr::opts_chunk$set(
  echo = FALSE,
  fig.height = 25,
  fig.width = 16, 
  fig.align = 'center' 
)
fig_ISTD
#' 
#' ### Analogue Analyte Peak Areas
knitr::opts_chunk$set(
  echo = FALSE,
  fig.height = 25,
  fig.width = 16, 
  fig.align = 'center' 
)
figAnalytes
#' 
#' ### Response Ratios
knitr::opts_chunk$set(
  echo = FALSE,
  fig.height = 25,
  fig.width = 16, 
  fig.align = 'center' 
)
figResponseRatio