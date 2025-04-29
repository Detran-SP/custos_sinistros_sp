create_custos_pessoas = function(tp_sinistros, fator_ipca) {
    tipo_vitimas = c("Ileso", "Leve", "Grave", "Fatal")
    expand_grid(tipo_vitimas, tp_sinistros) |> 
        mutate(
            custos = c(
                # Extraídos de IPEA (2020)
                1086.14, 4110.60, 1839.94,      # Ileso
                6456.33, 8469.44, 8635.77,      # Leve
                22421.06, 125133.91, 141155.96, # Grave
                199.28, 335172.20, 433286.69    # Fatal
            ),
            custos_atual = custos * fator_ipca
        )
}

create_custos_veiculos <- function(tp_sinistros, fator_ipca) {
    tipo_veiculos = c(
        "Automóvel", "Motocicleta", "Bicicleta", "Caminhão", "Ônibus", "Outros"
    )
    expand_grid(tipo_veiculos, tp_sinistros) |> 
        mutate(
            custos = c(
                # Extraídos de IPEA (2020)
                7159.12, 12126.82, 19323.91,   # Automóveis
                2473.21, 2741.02, 4269.83,     # Motocicletas
                0.00, 168.74, 124.10,          # Bicicletas
                22313.92, 65656.00, 47825.45,  # Caminhões
                16069.30, 10536.86, 20686.60,  # Ônibus
                10307.36, 80108.63, 81209.29   # Outros
            ),
            custos_atual = custos * fator_ipca
        )
}

create_custos_inst <- function(tp_sinistros, fator_ipca) {
    tibble(
        tipo_sinistro = tp_sinistros,
        # Extraídos de IPEA (2020)
        custos = c(453.35, 338.33, 653.06),
        custos_atual = custos * fator_ipca
    )
}

create_custos_urbanos <- function(tp_sinistros, fator_ipca) {
    tibble(
        tipo_sinistro = tp_sinistros,
        custos = c(3261.54, 17459.69, 144477.50),
        custos_atual = custos * fator_ipca
    )
}