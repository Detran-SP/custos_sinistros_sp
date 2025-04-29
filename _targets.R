library(targets)

tar_option_set(
    packages = c("tidyverse", "infosiga")
)

tar_source(
    "R/catalogo_custos.R"
)

list(
    tar_target(
        tp_sinistros,
        c("Sem vítimas", "Sinistro não fatal", "Sinistro fatal")
    ),
    tar_target(fator_ipca_201412_202503, 1.7986),
    tar_target(fator_ipca_200304_202503, 2.3786),
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
    )
)
