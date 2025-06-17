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
- Geração automática de relatórios em Quarto.
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

1. **Configure os parâmetros**: Edite o arquivo `_targets.R` caso necessário.

2. **Execute o pipeline**:

   ```r
   targets::tar_make()
   ```

O relatório final estará disponível como `index.html`.

## Estrutura do projeto

```
├── _targets.R       # Definição do pipeline
├── index.qmd        # Relatório em Quarto Markdown
├── data/            # Bases de dados de entrada
├── R/               # Funções em R
├── renv/            # Ambiente R
├── renv.lock        # Versões dos pacotes
├── README.md        # Documentação do projeto
└── LICENSE          # Licença do projeto
```

## Licença

Este projeto está licenciado sob a [Licença GPL-3.0](LICENSE).

## Contato

Divisão de Estudos para Segurança no Trânsito - DETRAN-SP  
📧 [estudos.transito@detran.sp.gov.br](mailto:estudos.transito@detran.sp.gov.br)
