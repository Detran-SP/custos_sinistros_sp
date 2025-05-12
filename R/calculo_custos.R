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

calc_custos_pessoas <- function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro), df_ipca
) {
    df_wide = df_custos |> 
        #select(-custos) |> 
        filter(tipo_vitimas != "Ileso") |> 
        pivot_wider(
            names_from = tipo_vitimas, 
            values_from = custos_atual, 
            names_prefix = "custos_"
        ) |> 
        clean_names()
    
    df_sinistros |> 
        mutate(
            date = ym(paste0(year(data_sinistro), "-", month(data_sinistro)))
        ) |> 
        left_join(
            df_wide,
            by = c("tipo_registro" = "tp_sinistros", "date")
        ) |> 
        mutate(
            custos_pessoas_raw = 
                gravidade_leve * custos_leve + 
                    gravidade_grave * custos_grave + 
                        gravidade_fatal * custos_fatal
        ) |> 
        left_join(df_ipca |> select(date, valor_ipca), by = "date") |> 
        mutate(
            ipca_last = df_ipca$valor_ipca[12],
            custos_pessoas = ipca_last / valor_ipca * custos_pessoas_raw
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_pessoas = sum(custos_pessoas))
}

calc_custos_veiculos = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro), df_ipca
) {
    df_wide = df_custos |> 
        #select(-custos) |> 
        pivot_wider(
            names_from = tipo_veiculos, 
            values_from = custos_atual, 
            names_prefix = "custos_"
        ) |> 
        clean_names()

    df_sinistros |>
        mutate(
            date = ym(paste0(year(data_sinistro), "-", month(data_sinistro)))
        ) |> 
        left_join(
            df_wide,
            by = c("tipo_registro" = "tp_sinistros", "date")
        ) |> 
        mutate(
            custos_veiculos_raw = 
                tp_veiculo_bicicleta * custos_bicicleta +
                    tp_veiculo_motocicleta * custos_motocicleta +
                    tp_veiculo_automovel * custos_automovel +
                    tp_veiculo_caminhao * custos_caminhao +
                    tp_veiculo_onibus * custos_onibus +
                    tp_veiculo_outros * custos_outros
        ) |> 
        left_join(df_ipca |> select(date, valor_ipca), by = "date") |> 
        mutate(
            ipca_last = df_ipca$valor_ipca[12],
            custos_veiculos = ipca_last / valor_ipca * custos_veiculos_raw
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_veiculos = sum(custos_veiculos))
}

calc_custos_inst = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro), df_ipca
) {
    df_sinistros |> 
        mutate(
            date = ym(paste0(year(data_sinistro), "-", month(data_sinistro)))
        ) |> 
        left_join(
            df_custos,
            by = c("tipo_registro" = "tipo_sinistro", "date")
        ) |> 
        left_join(df_ipca |> select(date, valor_ipca), by = "date") |> 
        mutate(
            ipca_last = df_ipca$valor_ipca[12],
            custos_inst = ipca_last / valor_ipca * custos_atual
        ) |> 
        group_by({{ group }}) |> 
        summarise(custos_inst = sum(custos_inst))
}

calc_custos_urbanos = function(
    df_sinistros, df_custos, group = c(cod_ibge, tipo_registro), df_ipca
) {
    df_sinistros |> 
        mutate(
            date = ym(paste0(year(data_sinistro), "-", month(data_sinistro)))
        ) |> 
        left_join(
            df_custos,
            by = c("tipo_registro" = "tipo_sinistro", "date")
        ) |>
        left_join(df_ipca |> select(date, valor_ipca), by = "date") |> 
        mutate(
            ipca_last = df_ipca$valor_ipca[12],
            custos_urbanos = ipca_last / valor_ipca * custos_atual
        ) |>
        group_by({{ group }}) |> 
        summarise(custos_urbanos = sum(custos_urbanos))
}

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

extract_sinistros_tipo_via_na <- function(df_sinistros, date) {
    df_sinistros |> 
        filter(
            data_sinistro > as.Date(date),
            is.na(tipo_via),
            tipo_registro != "Notificação"
        )
}

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



