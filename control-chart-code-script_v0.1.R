## Load packages if needed

list_of_required_packages <- c("readxl", "dplyr", "tibble", "plotly", "mzR", "svDialogs", "janitor", "tidyverse")

packages_to_load <- list_of_required_packages[!(list_of_required_packages %in% (.packages()))]

if(length(packages_to_load)) {
  lapply(packages_to_load, library, character.only = TRUE)
}

##Read in data

#read in rda if not already in environment
if(!exists("master_list")){
  dlg_message("Welcome to Control CharteR! :-)", type = 'ok'); 
  dlg_message("Please run lipid SkylineR notebook prior to running this notebook.", type = 'ok'); 
  dlg_message("Open the .rda file produced by the SkylineR chunk.", type = 'ok')
  # load rda file
  load(file = file.choose())
}
##Define functions

#define 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

##Organise data
#arrange into the chronological run order
run_order <- vector("list", length(master_list$project_details$mzml_plate_list))
run_order_full <- data.frame()
timestamp <- vector("list", length(master_list$project_details$mzml_plate_list))

for (i in 1:length(master_list$project_details$mzml_plate_list)) {
  timestamp[[i]] <- data.frame(file_name = NULL, timepoint = NULL)
  names(timestamp)[[i]] <- master_list$project_details$mzml_plate_list[i]
  names(run_order)[[i]] <- master_list$project_details$mzml_plate_list[i]
  for (j in 1:length(master_list$data$mzR[[i]])) {
    timestamp[[i]][j, 1] <- names(master_list$data$mzR[[i]])[j]
    timestamp[[i]][j, 2] <- master_list$data$mzR[[i]][[j]]$mzR_timestamp
  }
  timestamp[[i]]$run_order <- timestamp[[i]] %>%
    arrange(timestamp[[i]]$V2)
  run_order[[i]] <- data.frame(file_name = timestamp[[i]]$run_order$V1, timestamp = timestamp[[i]]$run_order$V2)
  run_order_full <- rbind(run_order_full, run_order[[i]])
}

run_order_full <- run_order_full %>% arrange(run_order_full$timestamp)

skyline_data_ordered <- master_list$data$skyline_report %>%
  arrange(factor(master_list$data$skyline_report$file_name, levels = run_order_full$file_name))

##Identify positive and negative ISTDs
if(master_list$project_details$is_ver == "v1") {
transition_list <- read.csv(
  file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v1.csv",
  header = TRUE) %>%
  clean_names()
}
if(master_list$project_details$is_ver == "v2") {
  transition_list <- read.csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v2.csv",
    header = TRUE) %>%
    clean_names()
}
pos_SILs <- na.omit(unique(transition_list$note[transition_list$product_charge==1]))
neg_SILs <- na.omit(unique(transition_list$note[transition_list$product_charge==-1]))

#set plate parameters for plate_lines in graphs
n_plates <- length(master_list$project_details$mzml_plate_list)
n_samples_per_plate <- NULL
n_QCs_per_plate <- NULL
first_timestamp <- data.frame()
for (x in 1:n_plates) {
  #order by first timepoint in each plate
  first_timestamp <- rbind(first_timestamp, data.frame(plate_name = master_list$project_details$mzml_plate_list[x], timestamp = timestamp[[x]]$run_order$V2[1]))
}

first_timestamp <- first_timestamp %>% arrange(first_timestamp$timestamp)

for (x in first_timestamp$plate_name) {
  n_samples_in_plate <- length(master_list$data$mzR[[x]])
  n_samples_per_plate <- c(n_samples_per_plate, n_samples_in_plate)
  
  n_QCs_in_plate <- length(master_list$data$mzR[[x]][grepl(master_list$project_details$qc_type, names(master_list$data$mzR[[x]]))==TRUE])
  n_QCs_per_plate <- c(n_QCs_per_plate, n_QCs_in_plate)
}

#keep only QC samples
master <- skyline_data_ordered %>% add_column(type = FALSE)

master$type[grepl(master_list$project_details$qc_type, master$file_name)] <- "QC"
master$type[master$type==FALSE] <- "sample"

QCs <- master[master$type == "QC", ]
#keep only internal standards values
master_SIL <- master[grepl("SIL", master$molecule_name)==TRUE,]
QCs_SIL <- QCs[grepl("SIL", QCs$molecule_name)==TRUE,]

#list of all ISTD
ISTDs <- unique(QCs_SIL$molecule_name)
#find analogue names from list of ISTDs
if(master_list$project_details$is_ver == "v1") {
  analogue_names1 <- gsub('SIL_','', ISTDs)
  analogue_names_final <- gsub('_d.*', '', analogue_names1)
  
  #print ISTDs that don't have analogues in the method
  missing_analogues <- setdiff(analogue_names_final, unique(master$molecule_name))
  message("indices of missing analogues")
  missing_analogues_idx <- which(analogue_names_final %!in% unique(master$molecule_name))
  missing_analogues_idx
  message("ISTDs that have no analogue")
  ISTDs_missing_analogues <- ISTDs[missing_analogues_idx]
  ISTDs_missing_analogues
  
  #make array of ISTDs and their analogues
  ISTD_analogue_array <- data.frame(ISTD = ISTDs, Analogue = analogue_names_final)
  
  #manually change analyte for those ISTDs with no analogue
  for (x in 1:length(ISTDs)) {
    if (x %in% missing_analogues_idx) {
      ISTD_analogue_array[x, 2] <- 'remove'
    }
  }
  #replace any 'remove' for NA
  for (x in 1:nrow(ISTD_analogue_array)) {
    if (ISTD_analogue_array[x, 2] %in% c("remove", "'remove'")) {
      ISTD_analogue_array[x, 2] <- NA
    }
  }
  
  #replace SIL_SM(18:1)_d9_SPLASH row with NA as there are two ISTDs for this analyte
  ISTD_analogue_array[which(ISTD_analogue_array[, 1]=="SIL_SM(18:1)_d9_SPLASH"), ] =NA
}

if(master_list$project_details$is_ver == "v2") {
  #hard code analogue analytes for ISTDs
  ISTD_analogue_array <- data.frame(ISTD = ISTDs, Analogue = NA)
  ISTD_analogue_array[1, 2] <- "CE(14:0)"
  ISTD_analogue_array[2, 2] <- "CE(16:1)"
  ISTD_analogue_array[3, 2] <- "CE(18:1)"
  ISTD_analogue_array[4, 2] <- "CE(20:3)"
  ISTD_analogue_array[5, 2] <- "CE(22:4)"
  
  ISTD_analogue_array[6, 2] <- "CER(14:0)"
  ISTD_analogue_array[7, 2] <- "CER(16:0)"
  ISTD_analogue_array[8, 2] <- "CER(18:1)"
  ISTD_analogue_array[9, 2] <- "CER(20:1)"
  ISTD_analogue_array[10, 2] <- "CER(22:1)"
  ISTD_analogue_array[11, 2] <- "CER(24:1)"
  
  ISTD_analogue_array[12, 2] <- "DAG(14:0_14:0)"
  ISTD_analogue_array[13, 2] <- "DAG(16:0_16:1)"
  ISTD_analogue_array[14, 2] <- "DAG(16:0_18:1)"
  ISTD_analogue_array[15, 2] <- "DAG(16:0_20:3)"
  ISTD_analogue_array[16, 2] <- "DAG(16:0_22:5)"
  
  ISTD_analogue_array[17, 2] <- "DCER(18:0)"
  
  ISTD_analogue_array[18, 2] <- "FFA(18:0)"
  ISTD_analogue_array[19, 2] <- "FFA(18:1)"
  ISTD_analogue_array[20, 2] <- "FFA(18:2)"
  ISTD_analogue_array[21, 2] <- "FFA(20:4)"
  ISTD_analogue_array[22, 2] <- "FFA(16:0)"
  
  ISTD_analogue_array[23, 2] <- "HCER(d18:0_18:0)"
  
  ISTD_analogue_array[24, 2] <- "LCER(d18:0_18:0)"
  
  ISTD_analogue_array[25, 2] <- "LPC(16:0)"
  ISTD_analogue_array[26, 2] <- "LPC(18:0)"
  ISTD_analogue_array[27, 2] <- "LPC(20:0)" 
  
  ISTD_analogue_array[28, 2] <- "LPE(16:0)"
  ISTD_analogue_array[29, 2] <- "LPE(18:0)"
  ISTD_analogue_array[30, 2] <- "LPE(20:0)"  
  
  ISTD_analogue_array[31, 2] <- "LPG(16:0)"
  ISTD_analogue_array[32, 2] <- "LPG(18:0)"
  ISTD_analogue_array[33, 2] <- "LPG(20:0)"
  
  ISTD_analogue_array[34, 2] <- "LPI(16:0)"
  ISTD_analogue_array[35, 2] <- "LPI(18:0)"
  ISTD_analogue_array[36, 2] <- "LPI(20:0)"  
  
  ISTD_analogue_array[37, 2] <- "LPS(16:0)"
  ISTD_analogue_array[38, 2] <- "LPS(18:0)"
  ISTD_analogue_array[39, 2] <- NA
  ISTD_analogue_array[40, 2] <- "LPS(20:0)"
  
  ISTD_analogue_array[41, 2] <- "MAG(18:1)"
  
  ISTD_analogue_array[42, 2] <- "PC(18:0_16:1)"
  ISTD_analogue_array[43, 2] <- "PC(18:0_18:1)"
  ISTD_analogue_array[44, 2] <- "PC(18:0_20:3)"
  ISTD_analogue_array[45, 2] <- "PC(18:0_22:4)"  
  
  ISTD_analogue_array[46, 2] <- "PE(16:0_14:0)"  
  ISTD_analogue_array[47, 2] <- "PE(16:0_16:1)"
  ISTD_analogue_array[48, 2] <- "PE(16:0_18:1)"
  ISTD_analogue_array[49, 2] <- "PE(16:0_20:3)"
  ISTD_analogue_array[50, 2] <- "PE(16:0_22:4)" 
  
  ISTD_analogue_array[51, 2] <- "PG(16:0_14:0)"  
  ISTD_analogue_array[52, 2] <- "PG(16:0_16:1)"
  ISTD_analogue_array[53, 2] <- "PG(16:0_18:1)"
  ISTD_analogue_array[54, 2] <- "PG(16:0_20:3)"
  ISTD_analogue_array[55, 2] <- "PG(16:0_22:4)"
  
  ISTD_analogue_array[56, 2] <- "PI(16:0_14:0)"  
  ISTD_analogue_array[57, 2] <- "PI(16:0_16:1)"
  ISTD_analogue_array[58, 2] <- "PI(16:0_18:1)"
  ISTD_analogue_array[59, 2] <- "PI(16:0_20:3)"
  ISTD_analogue_array[60, 2] <- "PI(16:0_22:4)"
  
  ISTD_analogue_array[61, 2] <- "PS(16:0_14:0)"  
  ISTD_analogue_array[62, 2] <- "PS(16:0_16:1)"
  ISTD_analogue_array[63, 2] <- "PS(16:0_18:1)"
  ISTD_analogue_array[64, 2] <- "PS(16:0_20:3)"
  ISTD_analogue_array[65, 2] <- "PS(16:0_22:4)" 
  
  ISTD_analogue_array[66, 2] <- "SM(14:0)"  
  ISTD_analogue_array[67, 2] <- "SM(16:0)"
  ISTD_analogue_array[68, 2] <- "SM(18:1)"
  ISTD_analogue_array[69, 2] <- "SM(20:1)"
  ISTD_analogue_array[70, 2] <- "SM(22:1)"
  ISTD_analogue_array[71, 2] <- "SM(24:1)"
  
  ISTD_analogue_array[72, 2] <- "TAG(40:0_FA14:0)"
  ISTD_analogue_array[73, 2] <- "TAG(44:1_FA14:0)"
  ISTD_analogue_array[74, 2] <- "TAG(45:1_FA16:0)" 
  ISTD_analogue_array[75, 2] <- "TAG(47:1_FA16:0)"  
  ISTD_analogue_array[76, 2] <- "TAG(49:1_FA16:0)"
  ISTD_analogue_array[77, 2] <- "TAG(51:2_FA16:0)"
  ISTD_analogue_array[78, 2] <- "TAG(53:3_FA18:2)"
  ISTD_analogue_array[79, 2] <- "TAG(55:4_FA18:1)"
  ISTD_analogue_array[80, 2] <- "TAG(57:3_FA18:2)"
}
#remove NA rows
ISTD_analogue_array <- na.omit(ISTD_analogue_array)

##ISTD figure

samples <- unique(master$file_name)
SILs <- unique(ISTD_analogue_array$ISTD)
#adjust name of ISTDs to make them shorter
SILs_edited <- gsub("_d5", "", SILs)
SILs_edited <- gsub("_d7", "", SILs_edited)
SILs_edited <- gsub("_d9", "", SILs_edited)
SILs_edited <- gsub("SIL_", "", SILs_edited)
sample_id <- c(1:length(samples))

plot_list <- vector(mode = "list", length = 0)
plate_lines <- vector(mode = "list", length = 0)

for (x in 1:length(SILs)) { #for each ISTD
  temp_data <- master_SIL[master_SIL$molecule_name==SILs[x],]
  
  #find any missing rows in temp_data (samples that don't have a value for the SIL)
  missing_sample_index <- which(samples %!in% unique(temp_data$file_name))
  #insert NA row for this sample in temp_data
  if (length(missing_sample_index > 0)) {
    missing_row <- data.frame(file_name = samples[samples %!in% unique(temp_data$file_name)==TRUE],
                              molecule_list_name = unique(temp_data$molecule_list_name),
                              molecule_name = unique(temp_data$molecule_name),
                              precursor_mz = unique(temp_data$precursor_mz),
                              product_mz = unique(temp_data$product_mz),
                              retention_time = NA,
                              start_time = NA,
                              end_time = NA,
                              area = NA,
                              height = NA
    )
    temp_data <- temp_data %>% add_row(missing_row, .before = missing_sample_index)
  }
  missing_sample_index = NULL
  
  sample_count <- 0
  
  for (y in 1:n_plates) {
    sample_count = sample_count + n_samples_per_plate[y]
    plate_lines[[y]] <- list(type='line', 
                             x0=sample_count+0.5, 
                             x1=sample_count+0.5, 
                             y0=0, 
                             y1=max(as.numeric(temp_data$area), na.rm = TRUE),
                             line=list(dash='dot', width=2, color = '#808080'))
  }
  color_value <- ifelse(SILs_edited[x] %in% pos_SILs, '#FF0000', '#0000FF')
  
  plot <- plot_ly(x = sample_id, 
                  y = temp_data$area, 
                  type = 'scatter', 
                  mode = 'markers',
                  color = temp_data$type,
                  text = paste(temp_data$file_name),
                  showlegend = FALSE
  )%>%
    layout(title = "ISTD peak area",
           shapes = plate_lines, 
           annotations = list(x = 0.5 , y = 1.1, showarrow = F, 
                              xref='paper', yref='paper',
                              xanchor='center',
                              text = SILs[x], 
                              font = list(color = color_value)),
           yaxis = list(rangemode = 'tozero'))
  
  plot_list = c(plot_list, list(plot))
}
if(master_list$project_details$is_ver == "v1") {
  fig_ISTD <- subplot(plot_list, nrows = 14)
}

if(master_list$project_details$is_ver == "v2") {
  fig_ISTD <- subplot(plot_list, nrows = 20)
}


##Analogue Analyte figure
#use only analogue analytes
analogues <- master[master$molecule_name %in% ISTD_analogue_array$Analogue, ]
# plot peak area of analogue analytes in LTRs
sample_id <- c(1:sum(n_samples_per_plate))

plot_list <- vector(mode = "list", length = 0)
plate_lines <- vector(mode = "list", length = 0)

for (x in 1:length(SILs)) { #for each ISTD
  temp_data<- analogues[analogues$molecule_name==ISTD_analogue_array$Analogue[x],]
  temp_data$missing <- FALSE
  # put in NULL row if any samples are missing this analogue
  if (nrow(temp_data) != length(sample_id)) {
    missing_file_names <- setdiff(unique(analogues$file_name), temp_data$file_name)
    for (z in 1:length(missing_file_names)) {
      row <- which(unique(analogues$file_name) == missing_file_names[z])
      temp_data <- temp_data %>% add_row(file_name = missing_file_names[z], 
                                         molecule_list_name = temp_data$molecule_list_name[1], 
                                         molecule_name = temp_data$molecule_name[1], 
                                         precursor_mz = temp_data$precursor_mz[1], 
                                         product_mz = temp_data$product_mz[1], 
                                         retention_time = NULL, 
                                         start_time = NULL, 
                                         end_time = NULL, 
                                         area = NULL, 
                                         height = NULL, 
                                         missing = TRUE, .before = row+1)
    }
  }
  # #reorder p011 to be at the end as this was the order of acquisition
  # temp_data <- rbind(temp_data_unordered[9:80,], temp_data_unordered[1:8,])
  sample_count <- 0
  
  for (y in 1:n_plates) {
    sample_count = sample_count + n_samples_per_plate[y]
    plate_lines[[y]] <- list(type='line', 
                             x0=sample_count+0.5, 
                             x1=sample_count+0.5, 
                             y0=0, 
                             y1=max(as.numeric(temp_data$area), na.rm = TRUE),
                             line=list(dash='dot', width=2, color = '#808080'))
  }
  color_value <- ifelse(SILs_edited[x] %in% pos_SILs, '#FF0000', '#0000FF')
  
  plot <- plot_ly(x = sample_id, 
                  y = temp_data$area, 
                  type = 'scatter', 
                  mode = 'markers', 
                  text = paste(temp_data$file_name),
                  color = temp_data$type,
                  showlegend = FALSE
  )%>%
    layout(title = "Analogue analyte peak area",
           shapes = plate_lines, 
           annotations = list(x = 0.5 , y = 1.1, showarrow = F, 
                              xref='paper', yref='paper',
                              xanchor='center',
                              text = ISTD_analogue_array$Analogue[x], 
                              font = list(color = color_value)),
           yaxis = list(rangemode = 'tozero'))
  
  plot_list = c(plot_list, list(plot))
}
if(master_list$project_details$is_ver == "v1") {
  figAnalytes <- subplot(plot_list, nrows = 14)
}

if(master_list$project_details$is_ver == "v2") {
  figAnalytes <- subplot(plot_list, nrows = 20)
}


##Compute response ratios

#make data frame for all QCs, with peak area for analogue and corresponding ISTD
analogue_response_ratio_array <- data.frame(analogues)
#rename 'area' as 'analogue_area'
colnames(analogue_response_ratio_array)[colnames(analogue_response_ratio_array)=='area'] <- "analogue_area"
for (x in 1:nrow(analogues)) {
  #fill in ISTD column with each analogue's corresponding ISTD
  analogue_response_ratio_array$ISTD[x] <- ISTD_analogue_array$ISTD[which(ISTD_analogue_array$Analogue==analogues$molecule_name[x])] 
  #fill in ISTD_area column with the corresponding ISTD's peak area
  analogue_response_ratio_array$ISTD_area[x] <- ifelse(length(which(master_SIL$file_name == analogue_response_ratio_array$file_name[x] & master_SIL$molecule_name == analogue_response_ratio_array$ISTD[x])) > 0,
                                                       master_SIL[which(master_SIL$file_name == analogue_response_ratio_array$file_name[x] & master_SIL$molecule_name == analogue_response_ratio_array$ISTD[x]), 'area'],
                                                       0)
}

#fill in response_ratio column with analogue_area/ISTD_area
analogue_response_ratio_array$response_ratio <- analogue_response_ratio_array$analogue_area/as.numeric(analogue_response_ratio_array$ISTD_area)

#change Inf to NULL
analogue_response_ratio_array$response_ratio[is.infinite(analogue_response_ratio_array$response_ratio)] <- NA

##Plot response ratios
sample_id <- c(1:sum(n_samples_per_plate))

plot_list <- vector(mode = "list", length = 0)
plate_lines <- vector(mode = "list", length = 0)

for (x in 1:length(SILs)) { #for each analogue
  temp_data <- analogue_response_ratio_array[analogue_response_ratio_array$molecule_name==ISTD_analogue_array$Analogue[x],]
  temp_data$missing <- FALSE
  # put in NULL row if any samples are missing this analogue
  if (nrow(temp_data) != length(sample_id)) {
    missing_file_names <- setdiff(unique(analogues$file_name), temp_data$file_name)
    for (z in 1:length(missing_file_names)) {
      row <- which(unique(analogues$file_name) == missing_file_names[z])
      temp_data <- temp_data %>% add_row(file_name = missing_file_names[z], 
                                         molecule_list_name = temp_data$molecule_list_name[1], 
                                         molecule_name = temp_data$molecule_name[1], 
                                         precursor_mz = temp_data$precursor_mz[1], 
                                         product_mz = temp_data$product_mz[1], 
                                         retention_time = NULL, 
                                         start_time = NULL, 
                                         end_time = NULL, 
                                         analogue_area = NULL, 
                                         height = NULL, 
                                         ISTD = temp_data$ISTD[1], 
                                         ISTD_area = NULL, 
                                         response_ratio = NULL,
                                         missing = TRUE, .before = row+1)
    }
  }
  sample_count <- 0
  
  for (y in 1:n_plates) {
    sample_count = sample_count + n_samples_per_plate[y]
    plate_lines[[y]] <- list(type='line',
                             x0=sample_count+0.5,
                             x1=sample_count+0.5,
                             y0=0,
                             y1=max(as.numeric(temp_data$response_ratio), na.rm = TRUE),
                             line=list(dash='dot', width=2, color = '#808080'))
  }
  color_value <- ifelse(SILs_edited[x] %in% pos_SILs, '#FF0000', '#0000FF')
  
  plot <- plot_ly(x = sample_id, 
                  y = as.numeric(temp_data$response_ratio), 
                  type = 'scatter', 
                  mode = 'markers', 
                  text = paste(temp_data$file_name),
                  color = temp_data$type,
                  showlegend = FALSE
  )%>%
    layout(title = paste("Response Ratios"),
           shapes = plate_lines, 
           annotations = list(x = 0.5 , y = 1.1, showarrow = F, 
                              xref='paper', yref='paper',
                              xanchor='center',
                              text = paste(unique(analogue_response_ratio_array$molecule_name)[x], " with ", unique(analogue_response_ratio_array$ISTD)[x]),
                              font = list(color = color_value)
           ),
           yaxis = list(rangemode = 'tozero'))
  
  plot_list = c(plot_list, list(plot))
}

if(master_list$project_details$is_ver == "v1") {
  figResponseRatio <- subplot(plot_list, nrows = 14)
}

if(master_list$project_details$is_ver == "v2") {
  figResponseRatio <- subplot(plot_list, nrows = 20)
}

##render html
fileConn<-file(paste0(master_list$project_details$project_dir, "/html_report/lipid_control_charteR_report_templatev3.23.R"))
writeLines(httr::GET(url = "https://raw.githubusercontent.com/alanahstjames/Lipids-System-Suitability/master/control-chart-template_v0.1.R") %>%
             httr::content(as = "text"), fileConn)
close(fileConn)

rmarkdown::render(input = paste0(master_list$project_details$project_dir, "/html_report/lipid_control_charteR_report_templatev3.23.R"),
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(Sys.Date(), "_", master_list$project_details$project_name, "_lipid_control_charteR_report_v3.23.html")
)

browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$project_name, "_lipid_control_charteR_report_v3.23.html")
)

rm(list = c(ls()[which(ls() != "master_list")]))
