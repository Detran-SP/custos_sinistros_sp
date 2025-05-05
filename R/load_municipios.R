load_municipios <- function(path) {
    read_csv2(path, locale = locale(encoding = "latin1")) |>
        clean_names() |>
        mutate(cod_ibge = as.character(cod_ibge)) |>
        select(cod_ibge, municipio) |> 
        arrange(cod_ibge)
}