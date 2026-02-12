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
    df,
    coluna_categoria,
    label_categoria,
    expandir = TRUE
) {
    df_formatado <- df |>
        select(-custos)

    if (coluna_categoria != 'tipo_sinistro') {
        df_formatado <- df_formatado |>
            filter(tp_sinistros != "Sem vítimas")
    }

    if (coluna_categoria == "tipo_sinistro") {
        df_formatado <- df_formatado |>
            filter(tipo_sinistro != "Sem vítimas")
    }

    if (coluna_categoria == "tipo_vitimas") {
        df_formatado <- df_formatado |>
            filter(tipo_vitimas != "Ileso") |>
            mutate(
                custos_atual = if_else(
                    tipo_vitimas == "Fatal" &
                        tp_sinistros == "Sinistro não fatal",
                    NA,
                    custos_atual
                )
            )
    }

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
        ) |>
        sub_missing(missing_text = "-")

    if (expandir) {
        tabela <- tabela |>
            tab_spanner(
                columns = -all_of(coluna_categoria),
                label = "Tipo de sinistro"
            )
    }

    if (coluna_categoria == "tipo_sinistro") {
        tabela <- tabela |>
            cols_label(custos_atual = "Custos")
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
        select(-custos) |>
        filter(tipo_sinistro != "Sem vítimas")

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

#' Calculate total cost from cost data
#'
#' Calculates the total cost from a cost data frame and returns it
#' as a formatted currency string.
#'
#' @param df_custos A data frame containing cost data with a
#' `custos_totais` column.
#'
#' @return A formatted string with the total cost in Brazilian Real.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' total_str <- calc_custo_total(df_custos)
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
    df,
    date_start,
    date_end,
    tipo = c("resumo", "gravidade")
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
                sem_info = sum(qtd_gravidade_nao_disponivel),
                ileso = sum(qtd_gravidade_ileso),
                leve = sum(qtd_gravidade_leve),
                grave = sum(qtd_gravidade_grave),
                fatal = sum(qtd_gravidade_fatal)
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
                        "Estradas e rodovias",
                        "Vias urbanas",
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
            #filter(tipo_via != "Local não identificado") |>
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
#' and crash severity for a specific road type.
#'
#' @param df A data frame with crash records.
#' @param date_start Start date in "yyyy-mm-dd" format.
#' @param date_end End date in "yyyy-mm-dd" format.
#' @param via A character string specifying the road type to filter
#' (e.g., "Estradas e rodovias", "Vias urbanas").
#'
#' @return A plotly object with the bar plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_veiculos_sinistro(
#'     df_sinistros,
#'     "2024-01-01",
#'     "2024-12-31",
#'     "Estradas e rodovias"
#' )
#' }
plot_veiculos_sinistro <- function(df, date_start, date_end, via) {
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
            bicicleta = sum(qtd_bicicleta),
            motocicleta = sum(qtd_motocicleta),
            automovel = sum(qtd_automovel),
            onibus = sum(qtd_onibus),
            caminhao = sum(qtd_caminhao),
            outros = sum(qtd_veic_outros),
            nao_disponivel = sum(qtd_veic_nao_disponivel),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = bicicleta:nao_disponivel,
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
                "outros" ~ "Outros",
                "nao_disponivel" ~ "Não disponível"
            ),
            tipo_veiculo = factor(
                tipo_veiculo,
                levels = c(
                    "Não disponível",
                    "Outros",
                    "Caminhão",
                    "Ônibus",
                    "Automóvel",
                    "Motocicleta",
                    "Bicicleta"
                )
            )
        ) |>
        filter(tipo_via == via)

    plot <- plot_ly(
        data = df_filtrado,
        y = ~tipo_veiculo,
        x = ~n,
        type = "bar",
        color = ~tipo_registro,
        orientation = "h",
        colors = c(
            ost.utils::palette_detran()$darkblue,
            ost.utils::palette_detran()$lightblue
        )
    ) |>
        layout(
            barmode = "group",
            xaxis = list(
                title = "Quantidade de veículos",
                showline = TRUE,
                showgrid = FALSE,
                tickformat = ".0f"
            ),
            yaxis = list(title = "Tipo de veículo")
        )

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
                    "custos_urbanos",
                    "custos_pessoas",
                    "custos_veiculos",
                    "custos_inst",
                    "custos_na"
                )
            ),
            componente = case_match(
                componente,
                "custos_pessoas" ~ "Estradas e rodovias - pessoas",
                "custos_veiculos" ~ "Estradas e rodovias - veículos",
                "custos_inst" ~ "Estradas e rodovias - danos patrimoniais",
                "custos_urbanos" ~ "Vias urbanas",
                "custos_na" ~ "Locais não identificados"
            ),
            valor_formatado = valor / 1e9
        )

    grafico <- plotly::plot_ly(
        data = df_componentes,
        y = ~componente,
        x = ~valor_formatado,
        type = "bar",
        orientation = "h",
        marker = list(color = ost.utils::palette_detran()$blue)
    ) |>
        layout(
            xaxis = list(
                title = "Custos",
                showline = TRUE,
                showgrid = FALSE,
                tickprefix = "R$ ",
                ticksuffix = " bi"
            ),
            yaxis = list(title = "Componente de custo")
        )

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
            Leve = sum(qtd_gravidade_leve),
            Grave = sum(qtd_gravidade_grave),
            Fatal = sum(qtd_gravidade_fatal),
            `Não disponível` = sum(qtd_gravidade_nao_disponivel),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = Leve:`Não disponível`,
            names_to = "tipo_vitimas",
            values_to = "n"
        ) |>
        left_join(
            tbl_custos,
            by = c("tipo_registro" = "tp_sinistros", "tipo_vitimas")
        ) |>
        filter(n != 0) |>
        group_by(tipo_vitimas) |>
        summarise(n = sum(n), custos_atual, .groups = "drop") |>
        slice_head(n = 4) |>
        mutate(custo_total = n * custos_atual) |>
        # select(-tipo_registro) |>
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
            Bicicleta = sum(qtd_bicicleta),
            Motocicleta = sum(qtd_motocicleta),
            Automóvel = sum(qtd_automovel),
            Ônibus = sum(qtd_onibus),
            Caminhão = sum(qtd_caminhao),
            Outros = sum(qtd_veic_outros),
            `Não disponível` = sum(qtd_veic_nao_disponivel),
            .groups = "drop"
        ) |>
        pivot_longer(
            cols = Bicicleta:`Não disponível`,
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
    df_custos_rodo,
    df_custos_urbano,
    df_sinistros_na
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


plot_count_sinistros_via <- function(sinistros, date_start, date_end) {
    df <- sinistros |>
        filter(
            data_sinistro >= date_start,
            data_sinistro <= date_end,
            tipo_registro != "Notificação"
        ) |>
        count(tipo_via, tipo_registro, ano_sinistro) |>
        replace_na(list(tipo_via = "Local não identificado"))

    plot <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = n, fill = tipo_via)) +
        theme_minimal() +
        facet_wrap(vars(tipo_registro), nrow = 2, scales = "free_y") +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::label_number(big.mark = ".")
        ) +
        scale_x_continuous(minor_breaks = NULL) +
        labs(fill = NULL, y = NULL, x = NULL) +
        theme(panel.grid.major.x = element_blank(), legend.position = "top") +
        scale_fill_manual(
            values = c(
                ost.utils::palette_detran()$purple,
                ost.utils::palette_detran()$grey,
                ost.utils::palette_detran()$darkblue
            )
        )

    return(
        ggplotly(plot) |>
            layout(
                legend = list(
                    orientation = "h",
                    y = 1.15,
                    x = 0.5,
                    xanchor = "center"
                )
            )
    )
}


plot_vitimas_gravidade <- function(sinistros, date_start, date_end, input_via) {
    df <- sinistros |>
        filter(
            data_sinistro >= date_start,
            data_sinistro <= date_end,
            tipo_registro != "Notificação",
            tipo_via == input_via
        ) |>
        group_by(ano_sinistro) |>
        summarise(
            `Fatal` = sum(qtd_gravidade_fatal),
            `Grave` = sum(qtd_gravidade_grave),
            `Leve` = sum(qtd_gravidade_leve),
            #ileso = sum(qtd_gravidade_ileso),
            `Não identificada` = sum(qtd_gravidade_nao_disponivel)
        ) |>
        pivot_longer(
            cols = `Fatal`:`Não identificada`,
            names_to = "gravidade",
            values_to = "n"
        )

    plot <- df |>
        ggplot() +
        geom_col(aes(x = ano_sinistro, y = n, fill = gravidade)) +
        theme_minimal() +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::label_number(big.mark = ".")
        ) +
        scale_x_continuous(minor_breaks = NULL) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(panel.grid.major.x = element_blank()) +
        scale_fill_manual(
            values = c(
                palette_detran()$darkblue,
                palette_detran()$blue,
                palette_detran()$lightblue,
                palette_detran()$grey
            )
        )

    ggplotly(plot) |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}

plot_veic_envolvido <- function(
    sinistros,
    date_start,
    date_end,
    input_registro = c("Sinistro fatal", "Sinistro não fatal"),
    input_via = c("Estradas e rodovias", "Vias urbanas")
) {
    df <- sinistros |>
        filter(
            data_sinistro >= date_start,
            data_sinistro <= date_end,
            tipo_registro != "Notificação",
            tipo_registro == input_registro,
            tipo_via == input_via
        ) |>
        group_by(ano_sinistro) |>
        summarise(
            `Bicicleta` = sum(qtd_bicicleta),
            `Motocicleta` = sum(qtd_motocicleta),
            `Automóvel` = sum(qtd_automovel),
            `Ônibus` = sum(qtd_onibus),
            `Caminhão` = sum(qtd_caminhao),
            `Outros` = sum(qtd_veic_outros),
            `Não identificado` = sum(qtd_veic_nao_disponivel),
            .groups = "drop"
        ) |>
        pivot_longer(
            `Bicicleta`:`Não identificado`,
            names_to = "tipo_veiculo",
            values_to = "n"
        ) |>
        mutate(
            tipo_veiculo = factor(
                tipo_veiculo,
                levels = c(
                    "Bicicleta",
                    "Motocicleta",
                    "Automóvel",
                    "Ônibus",
                    "Caminhão",
                    "Outros",
                    "Não identificado"
                )
            )
        )

    plot <- df |>
        ggplot() +
        geom_col(aes(x = ano_sinistro, y = n, fill = tipo_veiculo)) +
        theme_minimal() +
        scale_x_continuous(minor_breaks = NULL) +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::label_number(big.mark = ".")
        ) +
        theme(panel.grid.major.x = element_blank()) +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_fill_manual(
            values = c(
                palette_detran()$blue,
                palette_detran()$lightblue,
                palette_detran()$darkpurple,
                palette_detran()$purple,
                palette_detran()$lightpurple,
                "lightgrey",
                palette_detran()$grey
            )
        )

    ggplotly(plot) |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}


plot_custos_pessoas_rodovias <- function(
    sinistros_rodovias,
    catalog_custos_pessoas
) {
    df_custos_wide <- catalog_custos_pessoas |>
        select(-custos) |>
        filter(tipo_vitimas != "Ileso") |>
        pivot_wider(
            names_from = tipo_vitimas,
            values_from = custos_atual,
            names_prefix = "custos_"
        ) |>
        clean_names()

    df <- sinistros_rodovias |>
        left_join(
            df_custos_wide,
            by = c("tipo_registro" = "tp_sinistros")
        ) |>
        mutate(
            custos_pessoas_leve = qtd_gravidade_leve * custos_leve,
            custos_pessoas_grave = qtd_gravidade_grave * custos_grave,
            custos_pessoas_fatal = qtd_gravidade_fatal * custos_fatal,
            custos_pessoas_nao_disponivel = qtd_gravidade_nao_disponivel *
                custos_nao_disponivel
        ) |>
        group_by(ano_sinistro) |>
        summarise(
            `Leve` = sum(custos_pessoas_leve),
            `Grave` = sum(custos_pessoas_grave),
            `Fatal` = sum(custos_pessoas_fatal),
            `Não identificado` = sum(custos_pessoas_nao_disponivel)
        ) |>
        pivot_longer(
            cols = `Leve`:`Não identificado`,
            names_to = "gravidade",
            values_to = "custos"
        )

    plot <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = custos, fill = gravidade)) +
        theme_minimal() +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::label_dollar(
                prefix = "R$ ",
                big.mark = ".",
                decimal.mark = ","
            )
        ) +
        scale_x_continuous(minor_breaks = NULL) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(panel.grid.major.x = element_blank()) +
        scale_fill_manual(
            values = c(
                palette_detran()$darkblue,
                palette_detran()$blue,
                palette_detran()$lightblue,
                palette_detran()$grey
            )
        )

    ggplotly(plot) |>
        config(locale = "pt-BR") |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}

plot_custos_veic_rodovias <- function(
    sinistros_rodovias,
    catalog_custos_veiculos
) {
    df_wide = catalog_custos_veiculos |>
        select(-custos) |>
        pivot_wider(
            names_from = tipo_veiculos,
            values_from = custos_atual,
            names_prefix = "custos_"
        ) |>
        clean_names()

    df <- sinistros_rodovias |>
        left_join(
            df_wide,
            by = c("tipo_registro" = "tp_sinistros")
        ) |>
        mutate(
            `Bicicleta` = qtd_bicicleta * custos_bicicleta,
            `Motocicleta` = qtd_motocicleta * custos_motocicleta,
            `Automóvel` = qtd_automovel * custos_automovel,
            `Caminhão` = qtd_caminhao * custos_caminhao,
            `Ônibus` = qtd_onibus * custos_onibus,
            `Outros` = qtd_veic_outros * custos_outros,
            `Não identificado` = qtd_veic_nao_disponivel * custos_nao_disponivel
        ) |>
        group_by(ano_sinistro, tipo_registro) |>
        summarise(
            `Bicicleta` = sum(`Bicicleta`),
            `Motocicleta` = sum(`Motocicleta`),
            `Automóvel` = sum(`Automóvel`),
            `Caminhão` = sum(`Caminhão`),
            `Ônibus` = sum(`Ônibus`),
            `Outros` = sum(`Outros`),
            `Não identificado` = sum(`Não identificado`)
        ) |>
        pivot_longer(
            cols = `Bicicleta`:`Não identificado`,
            names_to = "tipo_veiculo",
            values_to = "custos"
        ) |>
        mutate(
            tipo_veiculo = factor(
                tipo_veiculo,
                levels = c(
                    "Bicicleta",
                    "Motocicleta",
                    "Automóvel",
                    "Ônibus",
                    "Caminhão",
                    "Outros",
                    "Não identificado"
                )
            )
        )

    plot <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = custos, fill = tipo_veiculo)) +
        theme_minimal() +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")
        ) +
        scale_x_continuous(minor_breaks = NULL) +
        scale_fill_manual(
            values = c(
                palette_detran()$blue,
                palette_detran()$lightblue,
                palette_detran()$darkpurple,
                palette_detran()$purple,
                palette_detran()$lightpurple,
                "lightgrey",
                palette_detran()$grey
            )
        ) +
        theme(panel.grid.major.x = element_blank()) +
        labs(x = NULL, y = NULL, fill = NULL) +
        facet_wrap(vars(tipo_registro), nrow = 2)

    ggplotly(plot) |>
        config(locale = "pt-BR") |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}

plot_custos_inst_rodovias <- function(
    sinistros_rodovias,
    catalog_custos_inst
) {
    df <- sinistros_rodovias |>
        left_join(
            catalog_custos_inst |> select(-custos),
            by = c("tipo_registro" = "tipo_sinistro")
        ) |>
        group_by(ano_sinistro, tipo_registro) |>
        summarise(custos = sum(custos_atual))

    plot <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = custos, fill = tipo_registro)) +
        theme_minimal() +
        scale_x_continuous(minor_breaks = NULL) +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")
        ) +
        scale_fill_manual(
            values = c(palette_detran()$darkblue, palette_detran()$lightblue)
        ) +
        theme(panel.grid.major.x = element_blank()) +
        labs(x = NULL, y = NULL, fill = NULL)

    ggplotly(plot) |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}


plot_custos_urbano <- function(sinistros_urbano, catalog_custos_urbano) {
    df <- sinistros_urbano |>
        left_join(
            catalog_custos_urbano |> select(-custos),
            by = c("tipo_registro" = "tipo_sinistro")
        ) |>
        group_by(ano_sinistro, tipo_registro) |>
        summarise(custos = sum(custos_atual))

    plt <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = custos, fill = tipo_registro)) +
        theme_minimal() +
        scale_x_continuous(minor_breaks = NULL) +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")
        ) +
        scale_fill_manual(
            values = c(palette_detran()$darkblue, palette_detran()$lightblue)
        ) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(panel.grid.major.x = element_blank())

    ggplotly(plt) |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}

plot_custos_na <- function(
    df_sinistros_na,
    df_custos_na_rodovias,
    df_custos_na_urbano
) {
    tbl_custos <- df_custos_na_rodovias |>
        bind_rows(df_custos_na_urbano) |>
        mutate(custo_medio = custos / sinistros) |>
        group_by(tipo_registro) |>
        summarise(custo_medio = mean(custo_medio), .groups = "drop")

    df <- df_sinistros_na |>
        left_join(tbl_custos, by = "tipo_registro") |>
        group_by(ano_sinistro, tipo_registro) |>
        summarise(custos = sum(custo_medio), .groups = "drop")

    plt <- ggplot(df) +
        geom_col(aes(x = ano_sinistro, y = custos, fill = tipo_registro)) +
        theme_minimal() +
        scale_x_continuous(minor_breaks = NULL) +
        scale_y_continuous(
            minor_breaks = NULL,
            labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")
        ) +
        scale_fill_manual(
            values = c(palette_detran()$darkblue, palette_detran()$lightblue)
        ) +
        theme(panel.grid.major.x = element_blank()) +
        labs(x = NULL, y = NULL, fill = NULL)

    ggplotly(plt) |>
        config(locale = "pt-BR") |>
        layout(
            legend = list(
                orientation = "h",
                y = 1.1,
                x = 0.5,
                xanchor = "center"
            )
        )
}

plot_custos_map <- function(shapes, df_custos, var) {
    sf <- shapes |>
        select(code_muni) |>
        mutate(code_muni = as.character(code_muni)) |>
        left_join(df_custos, by = c("code_muni" = "cod_ibge")) |>
        rename(custos = {{ var }})

    bins <- unique(round(
        quantile(
            sf$custos,
            probs = seq(0, 1, length.out = 9),
            na.rm = TRUE
        ),
        -6
    ))
    bins[1] <- 0
    bins[length(bins)] <- ceiling(max(sf$custos, na.rm = TRUE) / 1e6) * 1e6
    pal <- colorBin("Blues", domain = sf$custos, bins = bins)

    labels = sprintf(
        "<strong>%s</strong><br/>Custos: R$ %s<br/>",
        sf$municipio,
        format(
            sf$custos,
            big.mark = ".",
            decimal.mark = ",",
            nsmall = 2,
            scientific = FALSE
        )
    ) |>
        lapply(htmltools::HTML)

    leaflet(sf, options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
            fillColor = ~ pal(custos),
            stroke = TRUE,
            color = "white",
            fillOpacity = 1,
            weight = 1,
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            ),
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            layerId = ~municipio
        ) |>
        addLegend(
            pal = pal,
            values = sf$custos,
            position = "bottomleft",
            opacity = 1,
            title = "Custos:",
            labFormat = labelFormat(
                prefix = "R$ ",
                big.mark = "."
            )
        ) |>
        leaflet.extras::addFullscreenControl()
}
