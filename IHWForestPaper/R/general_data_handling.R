#general data handling

#' Extract and Combine Latest Files Based on File Ending
#'
#' @description This function identifies and combines the latest files in a specific directory based on file ending.
#' @param directory A character string specifying the path of the directory where the files are located.
#' @param file_ending A character string specifying the file ending to look for in the directory.
#' @return A combined data frame of all the files with the latest date and the specific file ending in the directory.
#' @importFrom stringr str_extract
#' @importFrom purrr map_dfr
#' @examples
#' # For "_eval_high_dim_sim.Rds"
#' high_dim_res_sim <- latest_files("simulation/data", "_eval_high_dim_sim.Rds")
#' # For "_eval_high_dim_sim_adapt.Rds"
#' high_dim_res_adapt <- latest_files("simulation/data", "_eval_high_dim_sim_adapt.Rds")
#' @export

latest_files <- function(directory, file_ending) {
  
  # List all the files in the directory
  files <- list.files(directory, full.names = TRUE)
  
  # Extract the date from the file names
  dates <- stringr::str_extract(files, "\\d{4}-\\d{2}-\\d{2}")
  dates <- as.Date(dates)
  
  # Identify the latest date for files ending with the specific file ending
  latest_date <- max(dates[grep(file_ending, files)], na.rm = TRUE)
  
  # Filter files with the latest date for the specific file ending
  latest_files <- files[dates == latest_date & grep(file_ending, files)]
  latest_files <- na.omit(latest_files)
  
  # Read and combine the files with the latest date for the specific file ending
  high_dim_res <- purrr::map_dfr(latest_files, readRDS)
  
  return(high_dim_res)
}
