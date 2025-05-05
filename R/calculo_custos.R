extract_sinistros <- function(
    df_sinistros,
    tp_via = c("Rodovias", "Vias municipais"),
    date
) {
    df_sinistros |> 
        filter(
            data_sinistro > as.Date(date),
            tipo_via == tp_via,
            tipo_registro != "Notificação"
        )
}

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
    df_municipios, custos_rodovias, custos_urbanos
) {
    df_municipios |> 
        left_join(custos_rodovias |> select(-municipio), by = "cod_ibge") |> 
        left_join(custos_urbanos, by = "cod_ibge") |> 
        mutate(
            across(starts_with("custos"), ~if_else(is.na(.x), 0, .x)),
            custos_totais = custos_rodovias + custos_urbanos
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
