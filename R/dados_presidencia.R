library(targets)
library(tidyverse)

tar_load(df_custos_municipio)
tar_load(df_sinistros)

df_sinistro_por_tipo <- df_sinistros |>
    filter(tipo_registro != "Notificação", year(data_sinistro) == 2024) |>
    count(tipo_registro)


tar_load(df_custos_tipo_registro_urbano)
tar_load(df_custos_tipo_registro_rodovias)
tar_load(df_sinistros_na)

df_custos_tipo_registro_na <- df_sinistros_na |>
    mutate(
        custo_na = case_match(
            tipo_registro,
            "Sinistro fatal" ~ 718554.03,
            "Sinistro não fatal" ~ 86234.69
        )
    ) |>
    group_by(tipo_registro) |>
    summarise(custo_na = sum(custo_na))

df_custos_geral <- df_custos_tipo_registro_urbano |>
    left_join(df_custos_tipo_registro_rodovias, by = "tipo_registro") |>
    left_join(df_custos_tipo_registro_na, by = "tipo_registro") |>
    replace_na(list(custo_na = 0)) |>
    mutate(
        custos_totais_geral = custos_urbanos + custos_totais_rodovias + custo_na
    ) |>
    select(tipo_registro, custos_totais_geral) |>
    left_join(df_sinistro_por_tipo, by = "tipo_registro") |>
    mutate(media_custo = custos_totais_geral / n)

df_custos_geral |> View()
