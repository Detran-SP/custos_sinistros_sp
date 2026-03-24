# Estimativa do custo dos sinistros de trânsito no estado de São Paulo - Metodologia de cálculo

## Visão geral

Este projeto oferece um fluxo reprodutível para estimar os custos
associados aos sinistros de trânsito nos municípios do estado de São Paulo,
no período de 2019 a 2025. Integra dados abertos de sinistros do
[Infosiga SP](https://www.infosiga.sp.gov.br/), a metodologia de custos do
IPEA e ajustes por inflação (IPCA) para gerar um relatório técnico em
formato de livro HTML com tabelas, gráficos interativos e mapas.

## Funcionalidades

- Funções modulares em R para cálculo de custos por tipo de vítima, tipo de
  veículo, resposta institucional e infraestrutura urbana.
- Integração com o pacote `{targets}` para pipelines reprodutíveis.
- Geração automática de relatórios em Quarto (formato livro com múltiplos capítulos).
- Suporte a ajustes por inflação utilizando o índice IPCA.
- Tabelas formatadas com `{gt}` e gráficos interativos com `{plotly}`.
- Mapas municipais interativos com `{leaflet}` e geometrias do `{geobr}`.

## Instalação

Para instalar os pacotes necessários e configurar o ambiente:

```r
install.packages("renv")
renv::restore()
```

## Uso

1. **Baixe os dados do Infosiga**: Acesse os [dados abertos do Infosiga (dados divulgados em Janeiro/2026)](https://drive.google.com/file/d/177B3Mds3R5O8X5RXZa0kZ-wTgzEbJWC9/view) e insira o arquivo `dados_infosiga.zip` na pasta `data/`.

2. **Configure os parâmetros**: Edite o arquivo `_targets.R` caso necessário (datas de início/fim e fatores IPCA).

3. **Execute o pipeline**:

```r
targets::tar_make()
```

O relatório final estará disponível em `docs/index.html`.


## Estrutura do projeto

```
├── _targets.R              # Definição do pipeline
├── _quarto.yml             # Configuração do Quarto
├── _brand.yml              # Configuração de branding
├── index.qmd               # Página inicial do relatório
├── 01-introducao.qmd       # Capítulo: Introdução
├── 02-metodologia.qmd      # Capítulo: Metodologia
├── 03-resultados.qmd       # Capítulo: Resultados
├── 04-conclusao.qmd        # Capítulo: Conclusão
├── 05-referencias.qmd      # Capítulo: Referências
├── refs.bib                # Referências bibliográficas
├── data/                   # Bases de dados de entrada
│   ├── dados_infosiga.zip
│   └── divisoes_regionais_esp.csv
├── R/                      # Funções em R
│   ├── calculo_custos.R    # Cálculo e junção de custos
│   ├── catalogo_custos.R   # Catálogos de custo de referência
│   ├── dados_presidencia.R # Dados auxiliares
│   ├── load_municipios.R   # Carregamento de municípios
│   └── report_utils.R      # Tabelas (gt) e gráficos (plotly/leaflet)
├── img/                    # Imagens e figuras
├── docs/                   # Relatório HTML gerado
├── renv/                   # Ambiente R (gerenciado pelo renv)
├── renv.lock               # Versões dos pacotes
├── README.md               # Documentação do projeto
└── LICENSE                 # Licença GPL-3.0
```

## Licença

Este projeto está licenciado sob a [Licença GPL-3.0](LICENSE).

## Contato

Divisão de Estudos para Segurança no Trânsito - DETRAN-SP
[estudos.transito@detran.sp.gov.br](mailto:estudos.transito@detran.sp.gov.br)
