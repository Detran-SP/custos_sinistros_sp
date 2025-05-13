#' Filter crash records by date and road type
#'
#' This function filters the crash records based on the specified road type 
#' and the reference date range.
#'
#' @param df_sinistros A data frame containing crash records.
#' @param tp_via A character vector specifying the road types to filter.
#' Defaults to c("Rodovias", "Vias municipais").
#' @param date_start A character string representing the start date 
#' in "yyyy-mm-dd" format.
#' @param date_end A character string representing the end date 
#' in "yyyy-mm-dd" format.
#'
#' @return A filtered data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     filtered <- extract_sinistros(
#'         crash_data, "Rodovias", "2024-01-01", "2024-12-31"
#'     )
#' }
extract_sinistros <- function(
    df_sinistros,
    tp_via = c("Rodovias", "Vias municipais"),
    date_start,
    date_end
) {
    df_sinistros |> 
        filter(
            data_sinistro >= as.Date(date_start),
            data_sinistro <= as.Date(date_end),
            tipo_via == tp_via,
            tipo_registro != "Notificação"
        )
}


#' Calculate person-related crash costs
#'
#' Calculates costs based on victim severity levels in crash records.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with cost definitions by severity.
#' @param group Grouping variables. Default is c(cod_ibge, tipo_registro).
#'
#' @return A summarized data frame with person-related costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' costs <- calc_custos_pessoas(
#'     crash_data,
#'     cost_table,
#'     cod_ibge
#' )
#' }
calc_custos_pessoas <- function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro)
) {
    df_wide = df_custos |> 
        select(-custos) |> 
        filter(tipo_vitimas != "Ileso") |> 
        pivot_wider(
            names_from = tipo_vitimas, 
            values_from = custos_atual, 
            names_prefix = "custos_"
        ) |> 
        clean_names()
    
    df_sinistros |> 
        left_join(
            df_wide,
            by = c("tipo_registro" = "tp_sinistros")
        ) |> 
        mutate(
            custos_pessoas = 
                gravidade_leve * custos_leve + 
                    gravidade_grave * custos_grave + 
                        gravidade_fatal * custos_fatal
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_pessoas = sum(custos_pessoas))
}

#' Calculate vehicle-related crash costs
#'
#' Calculates costs based on vehicle types involved in crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with vehicle cost definitions.
#' @param group Grouping variables. Default is c(cod_ibge, tipo_registro).
#'
#' @return A summarized data frame with vehicle-related costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' costs <- calc_custos_veiculos(
#'     crash_data,
#'     cost_table
#' )
#' }
calc_custos_veiculos = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro)
) {
    df_wide = df_custos |> 
        select(-custos) |> 
        pivot_wider(
            names_from = tipo_veiculos, 
            values_from = custos_atual, 
            names_prefix = "custos_"
        ) |> 
        clean_names()

    df_sinistros |> 
        left_join(
            df_wide,
            by = c("tipo_registro" = "tp_sinistros")
        ) |> 
        mutate(
            custos_veiculos = 
                tp_veiculo_bicicleta * custos_bicicleta +
                    tp_veiculo_motocicleta * custos_motocicleta +
                    tp_veiculo_automovel * custos_automovel +
                    tp_veiculo_caminhao * custos_caminhao +
                    tp_veiculo_onibus * custos_onibus +
                    tp_veiculo_outros * custos_outros
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_veiculos = sum(custos_veiculos))
}

#' Calculate institutional crash costs
#'
#' Calculates institutional response costs for crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with institutional cost definitions.
#' @param group Grouping variables. Default is c(cod_ibge, tipo_registro).
#'
#' @return A summarized data frame with institutional costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' costs <- calc_custos_inst(
#'     crash_data,
#'     institutional_costs
#' )
#' }
calc_custos_inst = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro)
) {
    df_sinistros |> 
        left_join(
            df_custos |> select(-custos),
            by = c("tipo_registro" = "tipo_sinistro")
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_inst = sum(custos_atual))
}


#' Calculate urban crash costs
#'
#' Calculates crash costs for urban road segments.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with urban cost definitions.
#' @param group Grouping variables. Default is c(cod_ibge, tipo_registro).
#'
#' @return A summarized data frame with urban crash costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' costs <- calc_custos_urbanos(
#'     crash_data,
#'     urban_costs
#' )
#' }
calc_custos_urbanos = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro)
) {
    df_sinistros |> 
        left_join(
            df_custos |> select(-custos),
            by = c("tipo_registro" = "tipo_sinistro")
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_urbanos = sum(custos_atual))
}

#' Join highway cost components
#'
#' Combines person, vehicle, and institutional costs for highways.
#'
#' @param df_municipios A data frame with municipality identifiers.
#' @param custos_pessoas A data frame with person-related costs.
#' @param custos_veiculos A data frame with vehicle-related costs.
#' @param custos_inst A data frame with institutional costs.
#'
#' @return A data frame with total highway costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- join_custos_rodovias(
#'     municipalities,
#'     person_costs,
#'     vehicle_costs,
#'     inst_costs
#' )
#' }
join_custos_rodovias = function(
    df_municipios, custos_pessoas, custos_veiculos, custos_inst
) {
    df_municipios |> 
        left_join(custos_pessoas, by = "cod_ibge") |> 
        left_join(custos_veiculos, by = "cod_ibge") |> 
        left_join(custos_inst, by = "cod_ibge") |> 
        mutate(
            across(starts_with("custos"), ~if_else(is.na(.x), 0, .x)),
            custos_rodovias = custos_pessoas + custos_veiculos + custos_inst
        )
}

#' Join all cost components
#'
#' Combines highway, urban, and undefined road type costs.
#'
#' @param df_municipios A data frame with municipality identifiers.
#' @param custos_rodovias A data frame with highway costs.
#' @param custos_urbanos A data frame with urban costs.
#' @param custos_vias_na A data frame with undefined road type costs.
#'
#' @return A data frame with total costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- join_all_custos(
#'     municipalities,
#'     rodovias_costs,
#'     urbanos_costs,
#'     na_costs
#' )
#' }
join_all_custos <- function(
    df_municipios, custos_rodovias, custos_urbanos, custos_vias_na
) {
    df_municipios |> 
        left_join(custos_rodovias |> select(-municipio), by = "cod_ibge") |> 
        left_join(custos_urbanos, by = "cod_ibge") |>
        left_join(custos_vias_na, by = "cod_ibge") |> 
        mutate(
            across(starts_with("custos"), ~if_else(is.na(.x), 0, .x)),
            custos_totais = custos_rodovias + custos_urbanos + custos_na
        )
}

#' Join rodovias costs by crash type
#'
#' Combines person, vehicle, and institutional costs by crash type.
#'
#' @param custos_pessoas A data frame with person-related costs.
#' @param custos_veiculos A data frame with vehicle-related costs.
#' @param custos_inst A data frame with institutional costs.
#'
#' @return A data frame with total costs by crash type.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- join_tipo_registro_rodovias_custos(
#'     person_costs,
#'     vehicle_costs,
#'     inst_costs
#' )
#' }
join_tipo_registro_rodovias_custos = function(
    custos_pessoas, custos_veiculos, custos_inst
) {
    custos_pessoas |> 
        left_join(custos_veiculos, by = "tipo_registro") |> 
        left_join(custos_inst, by = "tipo_registro") |> 
        mutate(
            custos_totais_rodovias = 
                custos_pessoas + custos_veiculos + custos_inst
        )
}

#' Calculate costs for undefined road type
#'
#' Estimates costs for crashes with undefined road types.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos_tipo_registro A data frame with cost definitions by crash type.
#' @param tipo A character vector with assumed road types.
#'
#' @return A data frame with estimated costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- calc_custos_na(
#'     na_crash_data,
#'     cost_by_type
#' )
#' }
calc_custos_na <- function(
    df_sinistros, 
    df_custos_tipo_registro, 
    tipo = c("Rodovias", "Vias urbanas")
) {
    df = df_sinistros |> 
        count(tipo_registro) |> 
        left_join(
            df_custos_tipo_registro |> select(1, last_col()),
            by = "tipo_registro"
        ) |> 
        mutate(tipo_via = tipo)

    colnames(df) = c("tipo_registro", "sinistros", "custos", "tipo")
    return(df)
}

#' Extract crashes with undefined road type
#'
#' Filters crashes with missing road type information.
#'
#' @param df_sinistros A data frame with crash records.
#' @param date_start The start date in "yyyy-mm-dd" format.
#' @param date_end The end date in "yyyy-mm-dd" format.
#'
#' @return A filtered data frame of crashes with undefined road type.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- extract_sinistros_tipo_via_na(
#'     crash_data,
#'     "2024-01-01",
#'     "2024-12-31"
#' )
#' }
extract_sinistros_tipo_via_na <- function(df_sinistros, date_start, date_end) {
    df_sinistros |> 
        filter(
            data_sinistro >= as.Date(date_start),
            data_sinistro <= as.Date(date_end),
            is.na(tipo_via),
            tipo_registro != "Notificação"
        )
}

#' Estimate costs for undefined road type crashes
#'
#' Estimates costs using average costs from known road types.
#'
#' @param df_sinistros_na A data frame with undefined road type crashes.
#' @param custos_urbano A data frame with urban costs by crash type.
#' @param custos_rodovias A data frame with highway costs by crash type.
#'
#' @return A data frame with estimated costs by municipality.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- calc_custos_sinistros_na(
#'     na_crashes,
#'     urban_costs,
#'     rodovias_costs
#' )
#' }
calc_custos_sinistros_na <- function(
    df_sinistros_na, custos_urbano, custos_rodovias
) {
    df_custos = custos_rodovias |> 
        bind_rows(custos_urbano) |> 
        mutate(custo_medio = custos / sinistros) |> 
        group_by(tipo_registro) |> 
        summarise(custo_medio = mean(custo_medio))

    df_sinistros_na |> 
        left_join(df_custos, by = "tipo_registro") |> 
        group_by(cod_ibge) |> 
        summarise(custos_na = sum(custo_medio))
}



