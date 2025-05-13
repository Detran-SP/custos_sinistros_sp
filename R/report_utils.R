#' Format cost table by category
#'
#' Formats a cost table with optional expansion by crash type.
#'
#' @param df A data frame with cost data.
#' @param coluna_categoria The name of the category column.
#' @param label_categoria A label to display for the category column.
#' @param expandir Logical. If TRUE, expand by crash type. Default is TRUE.
#'
#' @return A formatted gt table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_tabela_custos(
#'     df_custos,
#'     "tipo_vitimas",
#'     "Type of Victim"
#' )
#' }
formatar_tabela_custos <- function(
    df, coluna_categoria, label_categoria, expandir = TRUE
) {
    df_formatado <- df |>
        select(-custos)

    if (expandir) {
        df_formatado <- df_formatado |>
            pivot_wider(names_from = tp_sinistros, values_from = custos_atual)
    }

    tabela <- df_formatado |>
        gt() |>
        cols_label(!!sym(coluna_categoria) := label_categoria) |>
        fmt_number(
            columns = -all_of(coluna_categoria),
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        )

    if (expandir) {
        tabela <- tabela |>
            tab_spanner(
                columns = -all_of(coluna_categoria),
                label = "Tipo de sinistro"
            )
    }

    return(tabela)
}

#' Format urban cost table
#'
#' Formats a table showing costs for urban crashes.
#'
#' @param df A data frame with urban cost data.
#'
#' @return A formatted gt table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_urbanos(df_urbanos)
#' }
formatar_custos_urbanos <- function(df) {
    df_formatado <- df |>
        select(-custos)

    tabela <- df_formatado |>
        gt() |>
        cols_label(
            tipo_sinistro = "Tipo de sinistro",
            custos_atual = "Custos"
        ) |>
        fmt_number(
            columns = -tipo_sinistro,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        )

    return(tabela)
}

#' Format average cost table for undefined road type
#'
#' Calculates and formats the average cost per crash for undefined road types.
#'
#' @param df_rodovias A data frame with highway costs.
#' @param df_urbanos A data frame with urban costs.
#'
#' @return A formatted gt table with average costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_na(df_rodovias, df_urbanos)
#' }
formatar_custos_na <- function(df_rodovias, df_urbanos) {
    tabela <- df_rodovias |>
        bind_rows(df_urbanos) |>
        mutate(custo_medio = custos / sinistros) |>
        group_by(tipo_registro) |>
        summarise(custo_medio = mean(custo_medio)) |>
        gt() |>
        cols_label(
            tipo_registro = "Tipo de sinistro",
            custo_medio = "Custo médio"
        ) |>
        fmt_number(
            columns = -tipo_registro,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        )

    return(tabela)
}

#' Format average cost table for undefined road type
#'
#' Calculates and formats the average cost per crash for undefined road types.
#'
#' @param df_rodovias A data frame with highway costs.
#' @param df_urbanos A data frame with urban costs.
#'
#' @return A formatted gt table with average costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_na(df_rodovias, df_urbanos)
#' }
calc_custo_total <- function(df_custos) {
    custo_total <- df_custos$custos_totais |> sum()

    custo_total_str <- scales::dollar(
        custo_total,
        accuracy = 0.01,
        prefix = "R$ ",
        decimal.mark = ",",
        big.mark = "."
    )

    return(custo_total_str)
}

#' Calculate number of crash records in a date range
#'
#' Counts the number of crash records in the specified date range.
#'
#' @param df_sinistros A data frame with crash records.
#' @param date_start Start date in "yyyy-mm-dd" format.
#' @param date_end End date in "yyyy-mm-dd" format.
#'
#' @return A formatted string with the number of crashes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' n <- calc_quantidade_sinistros(
#'     df_sinistros,
#'     "2024-01-01",
#'     "2024-12-31"
#' )
#' }
calc_quantidade_sinistros <- function(df_sinistros, date_start, date_end) {
    n_sinistros <- df_sinistros |>
        filter(
            data_sinistro >= as.Date(date_start),
            data_sinistro <= as.Date(date_end),
            tipo_registro != "Notificação"
        ) |>
        nrow()

    n_sinistros = scales::number(
        n_sinistros, 
        big.mark = ".",
        decimal.mark = ","
    )
    
    return(n_sinistros)
}

#' Format crash summary or severity table
#'
#' Creates a summary or severity table based on filtered crash records.
#'
#' @param df A data frame with crash records.
#' @param date_start Start date in "yyyy-mm-dd" format.
#' @param date_end End date in "yyyy-mm-dd" format.
#' @param tipo Either "resumo" or "gravidade". Default is "resumo".
#'
#' @return A formatted gt table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela_resumo <- formatar_tabela_sinistros(
#'     df_sinistros,
#'     "2024-01-01",
#'     "2024-12-31",
#'     tipo = "resumo"
#' )
#'
#' tabela_gravidade <- formatar_tabela_sinistros(
#'     df_sinistros,
#'     "2024-01-01",
#'     "2024-12-31",
#'     tipo = "gravidade"
#' )
#' }
formatar_tabela_sinistros <- function(
    df, date_start, date_end, tipo = c("resumo", "gravidade")
) {
    tipo <- match.arg(tipo)

    df_filtrado <- df |>
        filter(
            data_sinistro >= as.Date(date_start),
            data_sinistro <= as.Date(date_end),
            tipo_registro != "Notificação"
        ) |>
        mutate(
            tipo_via = if_else(
                is.na(tipo_via), 
                "Local não identificado", 
                tipo_via
            )
        )

    if (tipo == "resumo") {
        tabela <- df_filtrado |>
            count(tipo_via, tipo_registro) |>
            pivot_wider(
                names_from = tipo_registro, 
                values_from = n, 
                values_fill = 0
            ) |>
            gt() |>
            cols_label(tipo_via = "Tipo de via") |>
            fmt_number(
                columns = -tipo_via,
                dec_mark = ",",
                sep_mark = ".",
                decimals = 0
            )

        return(tabela)
    }

    if (tipo == "gravidade") {
        tabela <- df_filtrado |>
            group_by(tipo_via, tipo_registro) |>
            summarise(
                sem_info = sum(gravidade_nao_disponivel),
                ileso = sum(gravidade_ileso),
                leve = sum(gravidade_leve),
                grave = sum(gravidade_grave),
                fatal = sum(gravidade_fatal)
            ) |>
            pivot_longer(
                cols = sem_info:fatal,
                names_to = "gravidade_vitima",
                values_to = "n"
            ) |>
            pivot_wider(
                names_from = tipo_registro, 
                values_from = n, 
                values_fill = 0
            ) |>
            mutate(
                tipo_via = factor(
                    tipo_via,
                    levels = c(
                        "Rodovias", 
                        "Vias municipais", 
                        "Local não identificado"
                    )
                ),
                gravidade_vitima = dplyr::case_match(
                    gravidade_vitima,
                    "sem_info" ~ "Gravidade não disponível",
                    "ileso" ~ "Ileso",
                    "leve" ~ "Leve",
                    "grave" ~ "Grave",
                    "fatal" ~ "Fatal"
                )
            ) |>
            filter(tipo_via != "Local não identificado") |>
            arrange(tipo_via) |>
            gt(rowname_col = "gravidade_vitima", row_group_as_column = TRUE) |>
            fmt_number(
                columns = `Sinistro fatal`:`Sinistro não fatal`,
                sep_mark = ".",
                dec_mark = ",",
                decimals = 0
            )

        return(tabela)
    }
}

#' Plot vehicle involvement in crashes
#'
#' Creates a bar plot showing vehicle types involved in crashes by road type
#' and crash severity.
#'
#' @param df A data frame with crash records.
#' @param date_start Start date in "yyyy-mm-dd" format.
#' @param date_end End date in "yyyy-mm-dd" format.
#'
#' @return A ggplot2 object with the bar plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_veiculos_sinistro(
#'     df_sinistros,
#'     "2024-01-01",
#'     "2024-12-31"
#' )
#' }
plot_veiculos_sinistro <- function(df, date_start, date_end) {
    df_filtrado <- df |>
        filter(
            data_sinistro >= as.Date(date_start),
            data_sinistro <= as.Date(date_end),
            tipo_registro != "Notificação"
        ) |>
        mutate(
            tipo_via = if_else(
                is.na(tipo_via),
                "Local não identificado",
                tipo_via
            )
        ) |>
        group_by(tipo_via, tipo_registro) |>
        summarise(
            bicicleta = sum(tp_veiculo_bicicleta),
            motocicleta = sum(tp_veiculo_motocicleta),
            automovel = sum(tp_veiculo_automovel),
            onibus = sum(tp_veiculo_onibus),
            caminhao = sum(tp_veiculo_caminhao),
            outros = sum(tp_veiculo_outros),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = bicicleta:outros,
            names_to = "tipo_veiculo",
            values_to = "n"
        ) |>
        mutate(
            tipo_veiculo = case_match(
                tipo_veiculo,
                "bicicleta" ~ "Bicicleta",
                "motocicleta" ~ "Motocicleta",
                "automovel" ~ "Automóvel",
                "onibus" ~ "Ônibus",
                "caminhao" ~ "Caminhão",
                "outros" ~ "Outros"
            ),
            tipo_veiculo = factor(
                tipo_veiculo,
                levels = c(
                    "Outros", "Caminhão", "Ônibus",
                    "Automóvel", "Motocicleta", "Bicicleta"
                )
            )
        ) |>
        filter(tipo_via != "Local não identificado")

    plot <- ggplot(df_filtrado, aes(x = tipo_veiculo, y = n)) +
        geom_col(fill = "#004077") +
        coord_flip() +
        facet_grid(
            rows = vars(tipo_via),
            cols = vars(tipo_registro),
            scales = "free"
        ) +
        labs(x = NULL, y = NULL)

    return(plot)
}

#' Plot cost components
#'
#' Creates a bar plot showing the distribution of cost components in
#' billions of Reais.
#'
#' @param df_custos A data frame with cost components.
#'
#' @return A ggplot2 object with the bar plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_custos_componentes(df_custos)
#' }
plot_custos_componentes <- function(df_custos) {
    custos <- df_custos |>
        select(-cod_ibge, -municipio, -custos_rodovias, -custos_totais) |>
        colSums()

    df_componentes <- tibble(
        componente = names(custos),
        valor = custos
    ) |>
        mutate(
            componente = factor(
                componente,
                levels = c(
                    "custos_urbanos", "custos_pessoas", "custos_veiculos",
                    "custos_inst", "custos_na"
                )
            ),
            componente = case_match(
                componente,
                "custos_pessoas" ~ "Rodovias - pessoas",
                "custos_veiculos" ~ "Rodovias - veículos",
                "custos_inst" ~ "Rodovias - Inst. e danos patrimoniais",
                "custos_urbanos" ~ "Vias municipais",
                "custos_na" ~ "Locais não identificados"
            )
        )

    grafico <- ggplot(df_componentes, aes(x = componente, y = valor)) +
        geom_col(fill = "#004077") +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(
            label = scales::label_number(
                scale = 1e-9,
                prefix = "R$ ",
                suffix = " bi",
                big.mark = ".",
                decimal.mark = ","
            )
        ) +
        theme(plot.margin = margin(10, 40, 10, 10)) # top, right, bottom, left

    return(grafico)
}

#' Format person-related costs for highways
#'
#' Formats a table summarizing person-related costs for highway crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with cost definitions.
#'
#' @return A formatted gt table with person-related costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_pessoas_rodovias(
#'     df_sinistros,
#'     df_custos
#' )
#' }
formatar_custos_pessoas_rodovias <- function(df_sinistros, df_custos) {
    tbl_custos <- df_custos |>
        filter(
            tipo_vitimas != "Ileso",
            tp_sinistros != "Sem vítimas"
        ) |>
        select(-custos)

    tabela <- df_sinistros |>
        group_by(tipo_registro) |>
        summarise(
            Leve = sum(gravidade_leve),
            Grave = sum(gravidade_grave),
            Fatal = sum(gravidade_fatal),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = Leve:Fatal,
            names_to = "tipo_vitimas",
            values_to = "n"
        ) |>
        left_join(
            tbl_custos,
            by = c("tipo_registro" = "tp_sinistros", "tipo_vitimas")
        ) |>
        filter(n != 0) |>
        mutate(custo_total = n * custos_atual) |>
        select(-tipo_registro) |>
        gt(rowname_col = "tipo_vitimas") |>
        cols_label(
            n = "Qnt. de vítimas",
            custos_atual = "Custo p/ vítima",
            custo_total = "Custo total"
        ) |>
        fmt_number(
            columns = custos_atual:custo_total,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        ) |>
        fmt_number(columns = n, sep_mark = ".", decimals = 0) |>
        grand_summary_rows(
            columns = custo_total,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(
                .,
                pattern = "R$ {x}",
                sep_mark = ".",
                dec_mark = ","
            )
        ) |>
        grand_summary_rows(
            columns = n,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(., decimals = 0, sep_mark = ".")
        )

    return(tabela)
}

#' Format vehicle-related costs for highways
#'
#' Formats a table summarizing vehicle-related costs for highway crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with cost definitions.
#'
#' @return A formatted gt table with vehicle-related costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_veiculos_rodovias(
#'     df_sinistros,
#'     df_custos
#' )
#' }
formatar_custos_veiculos_rodovias <- function(df_sinistros, df_custos) {
    tbl_custos <- df_custos |>
        filter(tp_sinistros != "Sem vítimas") |>
        select(-custos)

    tabela <- df_sinistros |>
        group_by(tipo_registro) |>
        summarise(
            Bicicleta = sum(tp_veiculo_bicicleta),
            Motocicleta = sum(tp_veiculo_motocicleta),
            Automóvel = sum(tp_veiculo_automovel),
            Ônibus = sum(tp_veiculo_onibus),
            Caminhão = sum(tp_veiculo_caminhao),
            Outros = sum(tp_veiculo_outros),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = Bicicleta:Outros,
            names_to = "tipo_veiculos",
            values_to = "n"
        ) |>
        left_join(
            tbl_custos,
            by = c("tipo_registro" = "tp_sinistros", "tipo_veiculos")
        ) |>
        mutate(custo_total = n * custos_atual) |>
        gt(groupname_col = "tipo_registro", row_group_as_column = TRUE) |>
        cols_label(
            tipo_veiculos = "Tipo de veículo",
            n = "Qnt. de veículos",
            custos_atual = "Custo p/ veículo",
            custo_total = "Custo total"
        ) |>
        fmt_number(
            columns = custos_atual:custo_total,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        ) |>
        fmt_number(columns = n, sep_mark = ".", decimals = 0) |>
        grand_summary_rows(
            columns = custo_total,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(
                .,
                pattern = "R$ {x}",
                sep_mark = ".",
                dec_mark = ","
            )
        ) |>
        grand_summary_rows(
            columns = n,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(., decimals = 0, sep_mark = ".")
        )

    return(tabela)
}

#' Format institutional costs for highways
#'
#' Formats a table summarizing institutional costs for highway crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with institutional cost definitions.
#'
#' @return A formatted gt table with institutional costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_inst_rodovias(
#'     df_sinistros,
#'     df_custos
#' )
#' }
formatar_custos_inst_rodovias <- function(df_sinistros, df_custos) {
    tbl_custos <- df_custos |>
        filter(tipo_sinistro != "Sem vítimas") |>
        select(-custos)

    tabela <- df_sinistros |>
        count(tipo_registro) |>
        left_join(
            tbl_custos,
            by = c("tipo_registro" = "tipo_sinistro")
        ) |>
        mutate(custo_total = n * custos_atual) |>
        gt(rowname_col = "tipo_registro") |>
        cols_label(
            n = "Qnt. de sinistros",
            custos_atual = "Custo p/ sinistro",
            custo_total = "Custo total"
        ) |>
        fmt_number(
            columns = custos_atual:custo_total,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        ) |>
        fmt_number(columns = n, sep_mark = ".", decimals = 0) |>
        grand_summary_rows(
            columns = custo_total,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(
                .,
                pattern = "R$ {x}",
                sep_mark = ".",
                dec_mark = ","
            )
        ) |>
        grand_summary_rows(
            columns = n,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(., decimals = 0, sep_mark = ".")
        )

    return(tabela)
}

#' Format urban road costs
#'
#' Formats a table summarizing costs for urban road crashes.
#'
#' @param df_sinistros A data frame with crash records.
#' @param df_custos A data frame with cost definitions.
#'
#' @return A formatted gt table with urban road costs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_custos_vias_municipais(
#'     df_sinistros,
#'     df_custos
#' )
#' }
formatar_custos_vias_municipais <- function(df_sinistros, df_custos) {
    tbl_custos <- df_custos |>
        filter(tipo_sinistro != "Sem vítimas") |>
        select(-custos)

    tabela <- df_sinistros |>
        count(tipo_registro) |>
        left_join(
            tbl_custos,
            by = c("tipo_registro" = "tipo_sinistro")
        ) |>
        mutate(custo_total = n * custos_atual) |>
        gt(rowname_col = "tipo_registro") |>
        cols_label(
            n = "Qnt. de sinistros",
            custos_atual = "Custo p/ sinistro",
            custo_total = "Custo total"
        ) |>
        fmt_number(
            columns = custos_atual:custo_total,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        ) |>
        fmt_number(columns = n, sep_mark = ".", decimals = 0) |>
        grand_summary_rows(
            columns = custo_total,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(
                .,
                pattern = "R$ {x}",
                sep_mark = ".",
                dec_mark = ","
            )
        ) |>
        grand_summary_rows(
            columns = n,
            fns = list(Total = ~ sum(.)),
            fmt = ~ fmt_number(., decimals = 0, sep_mark = ".")
        )

    return(tabela)
}

#' Calculate costs for undefined road type (report version)
#'
#' Calculates costs for crashes with undefined road type using average costs
#' from known road types.
#'
#' @param df_custos_rodo A data frame with highway costs.
#' @param df_custos_urbano A data frame with urban costs.
#' @param df_sinistros_na A data frame with undefined road type crashes.
#'
#' @return A data frame with calculated costs for undefined road type.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' custos_na <- calc_custos_na_report(
#'     df_custos_rodo,
#'     df_custos_urbano,
#'     df_sinistros_na
#' )
#' }
calc_custos_na_report <- function(
    df_custos_rodo, df_custos_urbano, df_sinistros_na
) {
    tbl_custos <- df_custos_rodo |>
        bind_rows(df_custos_urbano) |>
        mutate(custo_medio = custos / sinistros) |>
        group_by(tipo_registro) |>
        summarise(
            custos_atual = mean(custo_medio),
            .groups = "drop"
        )
    
    resultado <- df_sinistros_na |>
        count(tipo_registro) |>
        left_join(tbl_custos, by = "tipo_registro") |>
        mutate(custo_total = n * custos_atual)

    return(resultado)
}

#' Format cost table by municipality
#'
#' Formats a cost table with municipality-level summary.
#'
#' @param df_custos A data frame with cost components by municipality.
#'
#' @return A formatted gt table with costs by municipality.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tabela <- formatar_tabela_custos_municipios(df_custos)
#' }
formatar_tabela_custos_municipios <- function(df_custos) {
    tabela <- df_custos |>
        select(
            municipio,
            custos_rodovias,
            custos_urbanos,
            custos_na,
            custos_totais
        ) |>
        gt() |>
        fmt_number(
            columns = -municipio,
            pattern = "R$ {x}",
            sep_mark = ".",
            dec_mark = ","
        ) |>
        cols_label(
            municipio = "Município",
            custos_rodovias = "Rodovias",
            custos_urbanos = "Vias municipais",
            custos_na = "Locais não identificados",
            custos_totais = "Custo total"
        ) |>
        tab_spanner(
            columns = custos_rodovias:custos_na,
            label = "Custo por tipo de via"
        ) |>
        opt_interactive(
            use_pagination = TRUE,
            use_sorting = TRUE,
            page_size_default = 20,
            use_compact_mode = TRUE,
            use_highlight = TRUE,
            use_filters = TRUE
        ) |>
        tab_options(table.font.size = "11pt")

    return(tabela)
}