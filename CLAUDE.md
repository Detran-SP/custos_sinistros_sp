# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based reproducible research project by Detran-SP that estimates the costs of traffic crashes (sinistros de transito) across Sao Paulo state municipalities. Uses official Infosiga open data, IPEA cost methodology, and IPCA inflation adjustment. Outputs an HTML book report via Quarto.

## Commands

```r
# Setup: install all locked package versions
install.packages("renv")
renv::restore()

# Run the full pipeline (data processing + report generation)
targets::tar_make()

# Inspect a specific target result
targets::tar_read(target_name)
targets::tar_load(target_name)  # loads into environment

# Visualize pipeline dependency graph
targets::tar_visnetwork()

# Check which targets are outdated
targets::tar_outdated()
```

There are no unit tests. Validation relies on the `{targets}` pipeline's dependency tracking and incremental rebuilds.

## Architecture

**Pipeline-driven**: `_targets.R` defines ~50 targets that form a DAG. The pipeline is the entry point for all computation — functions in `R/` are never called directly outside of targets.

**Data flow**:
```
data/dados_infosiga.zip → load_sinistros_full() [via ost.utils]
    ↓
Split by road type ("Estradas e rodovias" / "Vias urbanas")
    ↓
Calculate costs per component (pessoas, veiculos, institucional, urbano)
    ↓
join_custos_rodovias() / join_all_custos() → aggregate by municipality
    ↓
Formatting functions (formatar_*) → gt tables + plotly charts
    ↓
tar_quarto() renders *.qmd chapters → docs/index.html
```

**Key source files in `R/`**:
- `catalogo_custos.R` — `create_custos_*()` functions that build reference cost tables per crash type from IPEA base values, adjusted by IPCA inflation factors
- `calculo_custos.R` — `calc_custos_*()` and `extract_sinistros()` functions that join crash records with cost catalogs, plus `calc_prop_*()` for victim/vehicle proportions
- `report_utils.R` — `formatar_*()` table formatters (using `{gt}`) and `plot_*()` visualization functions (using `{plotly}`)
- `load_municipios.R` — loads municipality reference data (Latin-1 encoded CSV)

**Configurable parameters** (set as targets in `_targets.R`):
- `date_start` / `date_end`: analysis period
- `fator_ipca_201412_atual` / `fator_ipca_200304_atual`: inflation adjustment ratios (must be manually updated when IPCA changes)

## Conventions

- **Language**: All code, variable names, function names, and documentation are in Portuguese
- **Naming**: `calc_*` (calculations), `create_*` (catalog builders), `extract_*` (filters), `join_*` (data joins), `formatar_*` (report formatters), `plot_*` (visualizations)
- **Style**: Tidyverse pipes (`|>`), dplyr verbs, non-standard evaluation with `{{ }}` for column references passed as arguments
- **Dependencies**: Managed via `{renv}` — run `renv::restore()` after pulling. The `{ost.utils}` package is a custom internal Detran-SP package for Infosiga data handling
- **Report**: Quarto book format configured in `_quarto.yml` with `_brand.yml` for typography/colors. Chapters are numbered `.qmd` files
