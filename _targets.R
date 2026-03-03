library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "tidyverse", "ost.utils", "janitor", "gt", 
    "quarto", "plotly", "geobr", "leaflet", "leaflet.extras"
  ),
  format = "rds"
)

# Início da lista de alvos
list(
  # 1. Carregamento dos Dados (Lendo direto do ZIP como você tem na pasta data)
  tar_target(
    df_sinistros,
    {
      caminho_zip <- "data/dados_infosiga.zip"
      arquivos_no_zip <- unzip(caminho_zip, list = TRUE)$Name
      alvos <- arquivos_no_zip[grepl("sinistros_.*\\.csv", arquivos_no_zip)]
      
      purrr::map_dfr(alvos, ~{
        readr::read_csv2(unzip(caminho_zip, .x, exdir = tempdir()))
      }) %>%
        janitor::clean_names()
    }
  ),

  # 2. Parâmetros do Relatório (2019-2025)
  tar_target(date_start, "2019-01-01"),
  tar_target(date_end, "2025-12-31"),
  
  # 3. Constantes e Fatores IPCA
  tar_target(fator_ipca_201412_atual, 7403.29 / 4028.44),
  tar_target(fator_ipca_200304_atual, 7403.29 / 2144.49),
  
  # 4. Dados Geográficos
  tar_target(sf_municipios, geobr::read_municipality(code_muni = "SP")),
  
  # 5. O Alvo do Relatório Quarto (O que gera os HTMLs finais)
  tar_quarto(report, ".")
)