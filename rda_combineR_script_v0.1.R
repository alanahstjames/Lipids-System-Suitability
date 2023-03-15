list_of_required_packages <- c("svDialogs", "dplyr", "utils")

packages_to_load <- list_of_required_packages[!(list_of_required_packages %in% (.packages()))]

if(length(packages_to_load)) {
  lapply(packages_to_load, library, character.only = TRUE)
}

dlg_message("Welcome to rda combineR! :-)", type = 'ok'); 
dlg_message("Put all SkylineR rda files into one folder called 'rda', and put that folder in a folder called 'data' in the project directory.", type = 'ok'); 
dir <- choose.dir(default = "", caption = "Select project directory.")
proj_name <- dlgInput(message = "What would you like to call this project?", default = sapply(strsplit(dir, split= "\\", fixed = TRUE), tail, 1L))$res
rdas <- list.files(path = paste(dir, "/data/rda", sep = ""))
full_list <- vector("list", length(rdas))
for (n in 1:length(rdas)) {
  load(paste(dir, '/data/rda/', rdas[n], sep = ""))
  full_list[[n]] <- master_list
}
rm(master_list)

#order rdas based on the first timestamp in each rda
timestamps <- matrix(nrow = length(rdas), ncol = 2) %>% as.data.frame()
for (n in 1:length(full_list)) {
  timestamps[n, 1] <- n
  timestamps[n, 2] <- full_list[[n]]$data$mzR[[1]][[1]]$mzR_timestamp
}
timestamps <- timestamps[order(timestamps[, 2]), ]

#make final list
final_list <- vector("list")
final_list$environment <- full_list[[1]]$environment
final_list$templates <- full_list[[1]]$templates
final_list$project_details <- full_list[[1]]$project_details

final_list$project_details$project_dir <- dir
final_list$project_details$project_name <- proj_name

#concatenate plate_list and sample_list in project_details
##empty vectors
final_list$project_details$mzml_plate_list <- NA
final_list$project_details$mzml_sample_list <- NA
for (n in timestamps[, 1]) {
  final_list$project_details$mzml_plate_list <- c(final_list$project_details$mzml_plate_list, full_list[[n]]$project_details$mzml_plate_list)
  final_list$project_details$mzml_sample_list <- c(final_list$project_details$mzml_sample_list, full_list[[n]]$project_details$mzml_sample_list)
}
##remove NA at start of vector
final_list$project_details$mzml_plate_list <- final_list$project_details$mzml_plate_list[-1]
final_list$project_details$mzml_sample_list <- final_list$project_details$mzml_sample_list[-1]
##rename plates
for (n in 1:length(final_list$project_details$mzml_plate_list)) {
  final_list$project_details$mzml_plate_list[n] <- paste("plate_", n, sep = "")
}

#concatenate data section
##mzR
final_list$data$mzR <- list()
for (n in timestamps[, 1]) {
  final_list$data$mzR <- c(final_list$data$mzR, full_list[[n]]$data$mzR)
}
###rename plates
for (n in 1:length(final_list$data$mzR)) {
  names(final_list$data$mzR)[n] <- paste("plate_", n, sep = "")
}

##skyline_report
final_list$data$skyline_report <- list()
for (n in timestamps[, 1]) {
  final_list$data$skyline_report <- rbind(final_list$data$skyline_report, full_list[[n]]$data$skyline_report)
}

#summary tables
final_list$summary_tables <- full_list[[1]]$summary_tables
final_list$summary_tables$project_summary$value[1] <- dir
final_list$summary_tables$project_summary$value[4] <- proj_name

#process lists
final_list$process_lists <- list()

master_list <- final_list
#save master_list
save(master_list, file = paste(dir, "/", Sys.Date(), "_", "rdaCombineR", "_", proj_name, ".rda", sep = ""))

dir.create(paste0(dir, "/html_report"))