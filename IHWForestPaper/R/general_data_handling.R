#general data handling
library(magrittr)
library(dplyr)
#' Filter a data frame based on a vector of values
#'
#' This function filters rows of a data frame such that for each column, the value in that column is equal to
#' the corresponding value in a provided vector. The vector should be named with names corresponding to 
#' column names in the data frame.
#'
#' @param df A data frame to be filtered.
#' @param vector A named vector where names correspond to columns in df. Each value in the vector represents
#'        the value to filter on for the corresponding column in df.
#'
#' @return A data frame containing only the rows where for each column, the value in that column is equal to
#'         the corresponding value in the input vector.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
#' vector <- c(a = 2, b = 7)
#' filter_df(df, vector)
filter_df <- function(df, vec) {
  for (col in names(vec)) {
    if(col %in% colnames(df)) {
      df <- df[df[[col]] == vec[col], ]
    }
  }
  return(df)
}

#' This function iteratively creates a data.frame from a list of vectors.
#'
#' It iterates over all vectors in the list, applies expand.grid to each vector 
#' with the first element of all other vectors, row binds all the data frames 
#' and then returns the resulting data frame.
#' 
#' @param vec_list List of vectors.
#' @return Data frame resulting from the operation.
#'
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(4,5,6)
#' vec3 <- c(7,8,9)
#' vec_list <- list(vec1, vec2, vec3)
#' create_dataframe(vec_list)
#' @export
create_dataframe <- function(vec_list) {
  
  # Create an empty list to store data frames
  df_list <- list()
  
  # Loop over the vector list
  for(i in seq_along(vec_list)) {
    
    # Create a copy of vec_list to avoid mutation
    temp_list <- vec_list
    
    # Replace the current vector with its first element
    temp_list[-i] <- lapply(temp_list[-i], function(x) x[1])
    
    # Expand.grid and add the result to df_list
    df_list[[i]] <- do.call(expand.grid, temp_list)
  }
  
  result <- do.call(rbind, df_list)
  result <- result %>% distinct()
  # rbind all data frames and return
  return(result)
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



#' Split a dataframe into smaller parts and retrieve a specific split by index.
#'
#' This function divides a dataframe (`df`) into a specified number of smaller splits 
#' and returns one of the splits based on the provided index.
#'
#' @param df A dataframe that needs to be split.
#' @param num_splits The number of splits you want to divide the dataframe into.
#' @param split_index The index of the split you want to retrieve.
#'
#' @return A dataframe corresponding to the specified split index.
#' 
#' @examples
#' \dontrun{
#' data <- data.frame(a = rnorm(100), b = rnorm(100))
#' result_df <- get_split(data, num_splits = 10, split_index = 3)
#' }
#' 
#' @export
get_split <- function(df, num_splits, split_index) {
  # Calculating the size of each smaller data.frame
  split_size <- ceiling(nrow(df) / num_splits)
  
  # Adding a new column for split indices
  df_sub <- df %>% 
    mutate(split_index_del = rep(seq_len(num_splits), each = split_size, length.out = n()))
  
  # Extracting the desired smaller data.frame based on split_index
  df_sub <- df_sub %>%
    filter(split_index_del == split_index) %>%
    select(-split_index_del)
  
  return(df_sub)
}

#' Create a formula from a data frame
#'
#' This function takes a data frame and constructs a formula
#' based on its column names.
#'
#' @param Xs A data frame whose column names will be used to create the formula.
#'
#' @return A formula object, with the right-hand side containing all the column names.
#' @export
create_formula <- function(Xs) {
  formula_rhs <- paste(colnames(Xs), collapse = "+")
  formula_rhs <- paste0("~", formula_rhs)
  formula_rhs
  #as.formula(formula_rhs)
}

