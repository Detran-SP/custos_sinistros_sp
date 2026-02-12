library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "tidyverse",
        "ost.utils",
        "janitor",
        "gt",
        "quarto",
        "plotly",
        "geobr",
        "leaflet",
        "leaflet.extras"
    )
)

tar_source(
    files = c(
        "R/catalogo_custos.R",
        "R/load_municipios.R",
        "R/calculo_custos.R",
        "R/report_utils.R"
    )
)

list(
    tar_target(date_start, "2019-01-01"),
    tar_target(date_end, "2025-12-31"),
    tar_target(
        tp_sinistros,
        c("Sem vítimas", "Sinistro não fatal", "Sinistro fatal")
    ),
    tar_target(fator_ipca_201412_atual, 7403.29 / 4028.44),
    tar_target(fator_ipca_200304_atual, 7403.29 / 2144.49),
    tar_target(
        df_custos_pessoas,
        create_custos_pessoas(
            tp_sinistros,
            fator_ipca_201412_atual,
            df_prop_vitimas
        )
    ),
    tar_target(
        df_custos_veiculos,
        create_custos_veiculos(
            tp_sinistros,
            fator_ipca_201412_atual,
            df_prop_veiculos_fatais,
            df_prop_veiculos_naofatais
        )
    ),
    tar_target(
        df_custos_inst,
        create_custos_inst(tp_sinistros, fator_ipca_201412_atual)
    ),
    tar_target(
        df_custos_urbanos,
        create_custos_urbanos(tp_sinistros, fator_ipca_200304_atual)
    ),
    tar_target(
        municipios_path,
        "data/divisoes_regionais_esp.csv",
        format = "file"
    ),
    tar_target(df_municipios, load_municipios(municipios_path)),
    tar_target(df_sinistros, load_sinistros_full()),
    tar_target(
        df_sinistros_rodovias,
        extract_sinistros(
            df_sinistros,
            "Estradas e rodovias",
            date_start,
            date_end
        )
    ),
    tar_target(
        df_sinistros_municipios,
        extract_sinistros(df_sinistros, "Vias urbanas", date_start, date_end)
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
            "Estradas e rodovias"
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
        extract_sinistros_tipo_via_na(df_sinistros, date_start, date_end)
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
    ),
    tar_target(
        tbl_custos_veiculos,
        formatar_tabela_custos(
            df_custos_veiculos,
            "tipo_veiculos",
            "Tipo de veículo"
        )
    ),
    tar_target(
        tbl_custos_pessoas,
        formatar_tabela_custos(
            df_custos_pessoas,
            "tipo_vitimas",
            "Gravidade da vítima"
        )
    ),
    tar_target(
        tbl_custos_inst,
        formatar_tabela_custos(
            df_custos_inst,
            "tipo_sinistro",
            "Tipo de sinistro",
            expandir = FALSE
        )
    ),
    tar_target(tbl_custos_urbanos, formatar_custos_urbanos(df_custos_urbanos)),
    tar_target(
        tbl_custos_na,
        formatar_custos_na(df_custos_na_rodovias, df_custos_na_urbano)
    ),
    tar_target(custo_total, calc_custo_total(df_custos_municipio)),
    tar_target(
        n_sinistros,
        calc_quantidade_sinistros(df_sinistros, date_start, date_end)
    ),
    tar_target(
        tbl_sinistros,
        formatar_tabela_sinistros(df_sinistros, date_start, date_end, "resumo")
    ),
    tar_target(
        tbl_vitimas,
        formatar_tabela_sinistros(
            df_sinistros,
            date_start,
            date_end,
            "gravidade"
        )
    ),
    # tar_target(
    #     list_fig_sinistros_veiculo,
    #     map(
    #         c("Estradas e rodovias", "Vias urbanas"),
    #         plot_veiculos_sinistro,
    #         df = df_sinistros,
    #         date_start = date_start,
    #         date_end = date_end
    #     )
    # ),
    #tar_target(fig_custos, plot_custos_componentes(df_custos_municipio)),
    tar_target(
        tbl_resultados_pessoas,
        formatar_custos_pessoas_rodovias(
            df_sinistros_rodovias,
            df_custos_pessoas
        )
    ),
    tar_target(
        tbl_resultados_veiculos,
        formatar_custos_veiculos_rodovias(
            df_sinistros_rodovias,
            df_custos_veiculos
        )
    ),
    tar_target(
        tbl_resultados_inst,
        formatar_custos_inst_rodovias(
            df_sinistros_rodovias,
            df_custos_inst
        )
    ),
    tar_target(
        tbl_resultados_vias_municipais,
        formatar_custos_vias_municipais(
            df_sinistros_municipios,
            df_custos_urbanos
        )
    ),
    tar_target(
        df_custos_na_report,
        calc_custos_na_report(
            df_custos_na_rodovias,
            df_custos_na_urbano,
            df_sinistros_na
        )
    ),
    tar_target(
        tbl_custos_municipio,
        formatar_tabela_custos_municipios(df_custos_municipio)
    ),
    tar_target(
        df_prop_vitimas,
        calc_prop_vitimas(df_sinistros)
    ),
    tar_target(
        df_prop_veiculos_fatais,
        calc_prop_veiculos(
            df_sinistros,
            "Sinistro fatal"
        )
    ),
    tar_target(
        df_prop_veiculos_naofatais,
        calc_prop_veiculos(
            df_sinistros,
            "Sinistro não fatal"
        )
    ),
    tar_target(
        plt_count_sinistros_via,
        plot_count_sinistros_via(df_sinistros, date_start, date_end)
    ),
    tar_target(
        plt_vitimas_gravidade_urbano,
        plot_vitimas_gravidade(
            df_sinistros,
            date_start,
            date_end,
            "Vias urbanas"
        )
    ),
    tar_target(
        plt_vitimas_gravidade_rodovias,
        plot_vitimas_gravidade(
            df_sinistros,
            date_start,
            date_end,
            "Estradas e rodovias"
        )
    ),
    tar_target(
        plt_veic_envolvido_fatal_rodovias,
        plot_veic_envolvido(
            df_sinistros,
            date_start,
            date_end,
            "Sinistro fatal",
            "Estradas e rodovias"
        )
    ),
    tar_target(
        plt_veic_envolvido_fatal_urbano,
        plot_veic_envolvido(
            df_sinistros,
            date_start,
            date_end,
            "Sinistro fatal",
            "Vias urbanas"
        )
    ),
    tar_target(
        plt_veic_envolvido_nao_fatal_rodovia,
        plot_veic_envolvido(
            df_sinistros,
            date_start,
            date_end,
            "Sinistro não fatal",
            "Estradas e rodovias"
        )
    ),
    tar_target(
        plt_veic_envolvido_nao_fatal_urbano,
        plot_veic_envolvido(
            df_sinistros,
            date_start,
            date_end,
            "Sinistro não fatal",
            "Vias urbanas"
        )
    ),
    tar_target(
        plt_custos_pessoas_rodovias,
        plot_custos_pessoas_rodovias(df_sinistros_rodovias, df_custos_pessoas)
    ),
    tar_target(
        plt_custos_veic_rodovias,
        plot_custos_veic_rodovias(df_sinistros_rodovias, df_custos_veiculos)
    ),
    tar_target(
        plt_custos_inst_rodovias,
        plot_custos_inst_rodovias(df_sinistros_rodovias, df_custos_inst)
    ),
    tar_target(
        plt_custos_urbano,
        plot_custos_urbano(df_sinistros_municipios, df_custos_urbanos)
    ),
    tar_target(
        plt_custos_na,
        plot_custos_na(
            df_sinistros_na,
            df_custos_na_rodovias,
            df_custos_na_urbano
        )
    ),
    tar_target(
        map_custos_total,
        plot_custos_map(sf_municipios, df_custos_municipio, "custos_totais")
    ),
    tar_target(
        map_custos_urbanos,
        plot_custos_map(sf_municipios, df_custos_municipio, "custos_urbanos")
    ),
    tar_target(
        map_custos_rodovias,
        plot_custos_map(sf_municipios, df_custos_municipio, "custos_rodovias")
    ),
    tar_target(
        map_custos_na,
        plot_custos_map(sf_municipios, df_custos_municipio, "custos_na")
    ),
    tar_target(sf_municipios, geobr::read_municipality(code_muni = "SP")),
    tar_target(
        plt_custos_componentes,
        plot_custos_componentes(df_custos_municipio)
    ),
    tar_quarto(report, ".")
)
