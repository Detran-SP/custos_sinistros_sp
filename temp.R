library(infosiga)
library(tidyverse)

# sinistros = load_infosiga("sinistros")
# vitimas = load_infosiga("vitimas")
veiculos = load_infosiga("veiculos")

# glimpse(sinistros)

# sinistros |> 
#     #filter(tipo_registro == "Sinistro fatal") |> 
#     select(starts_with("gravidade")) |> 
#     colSums()

# sinistros |> 
#     filter(tipo_registro == "Sinistro fatal") |> 
#     select(starts_with("gravidade")) |> 
#     pull(gravidade_nao_disponivel) |> 
#     unique()

# sinistros_fatais = sinistros |> 
#     filter(tipo_registro == "Sinistro fatal")

# vitimas |> 
#     left_join(sinistros_fatais, by = "id_sinistro") |> 
#     filter(!is.na(hora_sinistro)) |> 
#     View()


## Custos

### Rodovias

tipo_vitimas = c("Ileso", "Leve", "Grave", "Fatal")
tipo_sinistro = c("Sem vítimas", "Sinistro não fatal", "Sinistro fatal")

df_custos_pessoas = expand_grid(tipo_vitimas, tipo_sinistro)
df_custos_pessoas$custos = c(
    1086.14, 4110.60, 1839.94,      # Ileso
    6456.33, 8469.44, 8635.77,      # Leve
    22421.06, 125133.91, 141155.96, # Grave
    199.28, 335172.20, 433286.69    # Fatal
)

tipo_veiculos = c(
    "Automóvel", "Motocicleta", "Bicicleta", "Caminhão", "Ônibus", "Outros"
)

df_custos_veiculos = expand_grid(tipo_veiculos, tipo_sinistro)
df_custos_veiculos$custos = c(
    7159.12, 12126.82, 19323.91,   # Automóveis
    2473.21, 2741.02, 4269.83,     # Motocicletas
    0.00, 168.74, 124.10,          # Bicicletas
    22313.92, 65656.00, 47825.45,  # Caminhões
    16069.30, 10536.86, 20686.60,  # Ônibus
    10307.36, 80108.63, 81209.29   # Outros
)

df_custos_institucionais = tibble(
    tipo_sinistro = tipo_sinistro,
    custos = c(453.35, 338.33, 653.06)
)

fator_ipca_201512_202503 = 1.628 # Calculadora IBGE

df_custos_pessoas$custos_atuais = 
    df_custos_pessoas$custos * fator_ipca_201512_202503

df_custos_veiculos$custos_atuais = 
    df_custos_veiculos$custos * fator_ipca_201512_202503

df_custos_institucionais$custos_atuais = 
    df_custos_institucionais$custos * fator_ipca_201512_202503

### Urbano

fator_ipca_200304_202503 = 2.3786

df_custos_urbanos = tibble(
    tipo_sinistro = tipo_sinistro,
    custos = c(3261.54, 17459.69, 144477.50),
    custos_atuais = custos * fator_ipca_200304_202503
)

## Sinistros

### Rodovias

### Urbano

## Cálculo

### Rodovias

### Urbano

### Total