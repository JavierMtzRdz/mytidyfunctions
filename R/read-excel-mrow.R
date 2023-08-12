#' Read Excel File with Multi-Row Titles (read_excel_mul)
#'
#' Read an Excel file containing data with multi-row titles and extract the data into a data frame. The titles
#' are assumed to be displayed consecutive rows, and the data is read from the row following the titles.
#'
#' @param path The path to the Excel file to be read.
#' @param range_title A numeric vector of length 2 indicating the range of rows that contain the title information.
#'   The titles should be displayed in these rows, and the data starts from the row following \code{range_title[2]}.
#' @param sheet The name or index of the sheet in the Excel file to read. If \code{NULL}, the default sheet is used.
#' @param ... Additional arguments to be passed to \code{\link[readxl::read_excel]{readxl::read_excel}}.
#'
#' @return A data frame containing the data from the Excel file with multi-row titles.
#'
#'
#' @export
read_excel_mrow <- function(path, 
                           range_title,
                           sheet = NULL) {
  max_range <- range_title[2] - range_title[1] + 1
  
  title <- readxl::read_excel(path,
                              skip = range_title[1] - 1, 
                              col_names = FALSE,
                              n_max = max_range,
                              .name_repair = "minimal") |> 
    data.frame() |> 
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~ paste(na.omit(.x), collapse = "_"))) 
  
  title_vec <- as.vector(title[1,])
  
  data <- readxl::read_excel(path,
                             skip = range_title[2], col_names = FALSE,
                             .name_repair = "minimal",
                             ...) |> 
    setNames(title_vec) 
  
  return(data)
}