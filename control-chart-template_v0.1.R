#' ---
#' title: Targeted Lipids Control Chart Report
#' author: ANPC
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: html_document
#' ---
#'   
#' ### Project Summary
if(master_list$project_details$is_ver == "v1") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 25,
    fig.width = 16, 
    fig.align = 'center' 
  )
}

if(master_list$project_details$is_ver == "v2") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 32,
    fig.width = 16, 
    fig.align = 'center' 
  )
}
#' 
knitr::kable(master_list$summary_tables$project_summary)
#' 
#' 
#' ### SIL ISTD Peak Areas
if(master_list$project_details$is_ver == "v1") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 25,
    fig.width = 16, 
    fig.align = 'center' 
  )
}

if(master_list$project_details$is_ver == "v2") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 32,
    fig.width = 16, 
    fig.align = 'center' 
  )
}
fig_ISTD
#' 
#' ### Analogue Analyte Peak Areas
if(master_list$project_details$is_ver == "v1") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 25,
    fig.width = 16, 
    fig.align = 'center' 
  )
}

if(master_list$project_details$is_ver == "v2") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 32,
    fig.width = 16, 
    fig.align = 'center' 
  )
}
figAnalytes
#' 
#' ### Response Ratios
if(master_list$project_details$is_ver == "v1") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 25,
    fig.width = 16, 
    fig.align = 'center' 
  )
}

if(master_list$project_details$is_ver == "v2") {
  knitr::opts_chunk$set(
    echo = FALSE,
    fig.height = 32,
    fig.width = 16, 
    fig.align = 'center' 
  )
}
figResponseRatio