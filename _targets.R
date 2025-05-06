library(targets)

tar_option_set(
    packages = c("tidyverse", "infosiga", "janitor")
)

tar_source(
    files = c(
        "R/catalogo_custos.R",
        "R/load_municipios.R",
        "R/calculo_custos.R"
    )
)

list(
    tar_target(date_start, "2024-03-31"),
    tar_target(
        tp_sinistros,
        c("Sem vítimas", "Sinistro não fatal", "Sinistro fatal")
    ),
    tar_target(fator_ipca_201412_202503, 7245.38 / 4028.44),
    tar_target(fator_ipca_200304_202503, 7245.38 / 2144.49),
    tar_target(
        df_custos_pessoas,
        create_custos_pessoas(tp_sinistros, fator_ipca_201412_202503)
    ),
    tar_target(
        df_custos_veiculos,
        create_custos_veiculos(tp_sinistros, fator_ipca_201412_202503)
    ),
    tar_target(
        df_custos_inst,
        create_custos_inst(tp_sinistros, fator_ipca_201412_202503)
    ),
    tar_target(
        df_custos_urbanos,
        create_custos_urbanos(tp_sinistros, fator_ipca_200304_202503)
    ),
    tar_target(
        municipios_path,
        "data/divisoes_regionais_esp.csv",
        format = "file"
    ),
    tar_target(df_municipios, load_municipios(municipios_path)),
    tar_target(df_sinistros, load_infosiga("sinistros")),
    tar_target(
        df_sinistros_rodovias, 
        extract_sinistros(df_sinistros, "Rodovias", date_start)
    ),
    tar_target(
        df_sinistros_municipios,
        extract_sinistros(df_sinistros, "Vias municipais", date_start)
    ),
    tar_target(
        df_custos_rodovias_pessoas, 
        calc_custos_pessoas(
            df_sinistros_rodovias,
            df_custos_pessoas,
            group = cod_ibge
        )
    ),
    tar_target(
        df_custos_rodovias_veiculos,
        calc_custos_veiculos(
            df_sinistros_rodovias, 
            df_custos_veiculos, 
            cod_ibge
        )
    ),
    tar_target(
        df_custos_rodovias_inst,
        calc_custos_inst(df_sinistros_rodovias, df_custos_inst, cod_ibge)
    ),
    tar_target(
        df_custos_rodovias,
        join_custos_rodovias(
            df_municipios,
            df_custos_rodovias_pessoas,
            df_custos_rodovias_veiculos,
            df_custos_rodovias_inst
        )
    ),
    tar_target(
        df_custos_vias_municipais,
        calc_custos_urbanos(
            df_sinistros_municipios,
            df_custos_urbanos, 
            cod_ibge
        )
    ),
    tar_target(
        df_custos_tipo_registro_pessoas,
        calc_custos_pessoas(
            df_sinistros_rodovias, 
            df_custos_pessoas, 
            tipo_registro
        )
    ),
    tar_target(
        df_custos_tipo_registro_veiculos,
        calc_custos_veiculos(
            df_sinistros_rodovias, 
            df_custos_veiculos, 
            tipo_registro
        )
    ),
    tar_target(
        df_custos_tipo_registro_inst,
        calc_custos_inst(
            df_sinistros_rodovias,
            df_custos_inst,
            tipo_registro
        )
    ),
    tar_target(
        df_custos_tipo_registro_urbano,
        calc_custos_urbanos(
            df_sinistros_municipios,
            df_custos_urbanos,
            tipo_registro
        )
    ),
    tar_target(
        df_custos_tipo_registro_rodovias,
        join_tipo_registro_rodovias_custos(
            df_custos_tipo_registro_pessoas,
            df_custos_tipo_registro_veiculos,
            df_custos_tipo_registro_inst
        )
    ),
    tar_target(
        df_custos_na_rodovias, 
        calc_custos_na(
            df_sinistros_rodovias, 
            df_custos_tipo_registro_rodovias, 
            "Rodovias"
        )
    ),
    tar_target(
        df_custos_na_urbano,
        calc_custos_na(
            df_sinistros_municipios,
            df_custos_tipo_registro_urbano,
            "Vias urbanas"
        )
    ),
    tar_target(
        df_sinistros_na,
        extract_sinistros_tipo_via_na(df_sinistros, date_start)
    ),
    tar_target(
        df_custos_vias_na,
        calc_custos_sinistros_na(
            df_sinistros_na,
            df_custos_na_urbano,
            df_custos_na_rodovias
        )
    ),
    tar_target(
        df_custos_municipio,
        join_all_custos(
            df_municipios,
            df_custos_rodovias,
            df_custos_vias_municipais,
            df_custos_vias_na
        )
    )
)
