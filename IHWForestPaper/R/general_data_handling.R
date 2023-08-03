#general data handling

#' Create a data.frame by expanding the grid of a list of vectors
#'
#' @param list_of_vectors A list of numeric vectors.
#'
#' @return A data.frame where each row is a combination of elements from each vector in the input list.
#' @export
#' 
#' @examples
#' v1 <- c(1, 2, 3)
#' v2 <- c(4, 5, 6)
#' v3 <- c(7, 8, 9)
#' list_of_vectors <- list(v1, v2, v3)
#' df <- expand_vectors(list_of_vectors)
#' print(df)
expand_vectors <- function(list_of_vectors) {
  expand.grid(list_of_vectors)
}

#' Expand each vector in a list with the first element of all other vectors
#'
#' @param list_of_vectors A list of numeric vectors.
#'
#' @return A data.frame where each row is a combination of each vector in the input list
#'         with the first element of all other vectors.
#' @export
#' 
#' @examples
#' v1 <- c(1, 2, 3)
#' v2 <- c(4, 5, 6)
#' v3 <- c(7, 8, 9)
#' list_of_vectors <- list(v1, v2, v3)
#' df <- create_dataframe(list_of_vectors)
#' print(df)
create_dataframe <- function(list_of_vectors) {
  requireNamespace("purrr", quietly = TRUE)
  
  purrr::map_dfr(list_of_vectors, function(current_vector) {
    # Create a copy of the list and remove the current vector
    other_vectors <- list_of_vectors[-which(list_of_vectors == current_vector)]
    
    # Get the first element of all other vectors
    first_elements <- unlist(lapply(other_vectors, function(x) x[1]))
    
    # Expand.grid the current vector with the first element of all other vectors
    expand.grid(current_vector, first_elements)
  })
}

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
