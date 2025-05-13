#' Load municipality reference table
#'
#' Reads a CSV file with municipality data, cleans column names, and selects
#' key columns.
#'
#' @param path A character string with the file path to the CSV.
#'
#' @return A data frame with municipality codes and names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_municipios <- load_municipios("data/municipios.csv")
#' }
load_municipios <- function(path) {
    read_csv2(path, locale = locale(encoding = "latin1")) |>
        clean_names() |>
        mutate(cod_ibge = as.character(cod_ibge)) |>
        select(cod_ibge, municipio) |> 
        arrange(cod_ibge)
}