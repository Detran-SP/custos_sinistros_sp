# Estimativa do custo dos sinistros de trânsito no estado de São Paulo - Metodologia de cálculo

## Visão geral

Este projeto oferece um fluxo reprodutível para estimar os custos
associados aos sinistros de trânsito nos municípios do estado de São Paulo.
Integra dados de sinistros, modelos de custos e ajustes por inflação para gerar
relatórios e visualizações detalhadas.

## Funcionalidades

- Funções modulares em R para cálculo de custos por tipo de vítima, tipo de
  veículo e resposta institucional.
- Integração com o pacote `{targets}` para pipelines reprodutíveis.
- Geração automática de relatórios em Quarto (formato livro com múltiplos capítulos).
- Suporte a ajustes por inflação utilizando o índice IPCA.
- Tabelas e gráficos formatados para relatórios técnicos.

## Instalação

Para instalar os pacotes necessários e configurar o ambiente:

```r
# Instale as dependências
install.packages("renv")
renv::restore()
```

## Uso

1. **Carregue os [dados abertos do Infosiga](https://infosiga.detran.sp.gov.br/rest/painel/download/file/dados_infosiga.zip)**: Baixe os dados abertos do Infosiga e insira o arquivo `dados_infosiga.zip` na pasta `data/`.

2. **Configure os parâmetros**: Edite o arquivo `_targets.R` caso necessário (datas e índices IPCA).

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
│   ├── calculo_custos.R
│   ├── catalogo_custos.R
│   ├── dados_presidencia.R
│   ├── load_municipios.R
│   └── report_utils.R
├── img/                    # Imagens e figuras
├── docs/                   # Relatório HTML gerado
├── renv/                   # Ambiente R
├── renv.lock               # Versões dos pacotes
├── README.md               # Documentação do projeto
└── LICENSE                 # Licença do projeto
```

## Licença

Este projeto está licenciado sob a [Licença GPL-3.0](LICENSE).

## Contato

Divisão de Estudos para Segurança no Trânsito - DETRAN-SP  
[estudos.transito@detran.sp.gov.br](mailto:estudos.transito@detran.sp.gov.br)
