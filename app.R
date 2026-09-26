# SHINY APP - CONSULTA DE SÉRIES HISTÓRICAS

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# ARQUIVO DE DADOS

arquivo <- file.path(
  getwd(),
  "dados_gerais.xlsx"
)

# VERIFICAR ARQUIVO

if (!file.exists(arquivo)) {
  
  stop(
    paste0(
      "O arquivo 'dados_gerais.xlsx' não foi encontrado.\n",
      "Coloque o arquivo na mesma pasta do app.R.\n\n",
      "Caminho procurado:\n",
      arquivo
    )
  )
}

# CARREGAR DADOS

algodao <- read_excel(
  arquivo,
  sheet = "Algodão"
)

amendoim <- read_excel(
  arquivo,
  sheet = "Amendoim"
)

arroz <- read_excel(
  arquivo,
  sheet = "Arroz"
)

aveia <- read_excel(
  arquivo,
  sheet = "Aveia"
)

canola <- read_excel(
  arquivo,
  sheet = "Canola"
)

centeio <- read_excel(
  arquivo,
  sheet = "Centeio"
)

cevada <- read_excel(
  arquivo,
  sheet = "Cevada"
)

feijao <- read_excel(
  arquivo,
  sheet = "Feijão"
)

gergelim <- read_excel(
  arquivo,
  sheet = "Gergelim"
)

girassol <- read_excel(
  arquivo,
  sheet = "Girassol"
)

mamona <- read_excel(
  arquivo,
  sheet = "Mamona"
)

milho <- read_excel(
  arquivo,
  sheet = "Milho"
)

soja <- read_excel(
  arquivo,
  sheet = "Soja"
)

sorgo <- read_excel(
  arquivo,
  sheet = "Sorgo"
)

trigo <- read_excel(
  arquivo,
  sheet = "Trigo"
)

triticale <- read_excel(
  arquivo,
  sheet = "Triticale"
)

# PREPARAR DADOS

preparar_dados <- function(df) {
  
  # Verificar colunas obrigatórias
  if (!all(c("Região", "Ano") %in% names(df))) {
    
    stop(
      "A planilha precisa conter as colunas 'Região' e 'Ano'."
    )
  }
  
  # Converter Região e Ano para texto
  df <- df %>%
    mutate(
      Região = as.character(Região),
      Ano = as.character(Ano)
    )

  # Identificar colunas quantitativas
  colunas_valores <- setdiff(
    names(df),
    c("Região", "Ano")
  )

  # Converter valores para numérico
  if (length(colunas_valores) > 0) {
    
    df <- df %>%
      mutate(
        across(
          all_of(colunas_valores),
          ~ suppressWarnings(
            as.numeric(
              ifelse(
                .x == "-",
                NA,
                .x
              )
            )
          )
        )
      )
  }
  
  # Remover linhas sem Região ou Ano
  df <- df %>%
    filter(
      !is.na(Região),
      Região != "",
      !is.na(Ano),
      Ano != ""
    )
 
  return(df)
}

# PREPARAR TODAS AS CULTURAS

algodao <- preparar_dados(algodao)
amendoim <- preparar_dados(amendoim)
arroz <- preparar_dados(arroz)
aveia <- preparar_dados(aveia)
canola <- preparar_dados(canola)
centeio <- preparar_dados(centeio)
cevada <- preparar_dados(cevada)
feijao <- preparar_dados(feijao)
gergelim <- preparar_dados(gergelim)
girassol <- preparar_dados(girassol)
mamona <- preparar_dados(mamona)
milho <- preparar_dados(milho)
soja <- preparar_dados(soja)
sorgo <- preparar_dados(sorgo)
trigo <- preparar_dados(trigo)
triticale <- preparar_dados(triticale)

# FUNÇÃO PARA ORDENAR SAFRAS

ordenar_anos <- function(x) {
  
  x <- unique(as.character(x))
  
  ano_numerico <- suppressWarnings(
    as.numeric(
      sub(
        "/.*",
        "",
        x
      )
    )
  )
  
  if (sum(!is.na(ano_numerico)) > 0) {
    
    return(
      x[
        order(
          ano_numerico,
          na.last = TRUE
        )
      ]
    )
    
  } else {
    
    return(
      sort(x)
    )
  }
}

# INTERFACE

ui <- page_fillable(
  
  uiOutput("pagina")
  
)

# SERVIDOR

server <- function(
    input,
    output,
    session
) {
  
  # CONTROLE DA PÁGINA
   
  consulta_iniciada <- reactiveVal(FALSE)
  
  # INICIAR CONSULTA
  
  observeEvent(
    input$iniciar_consulta,
    {
      
      consulta_iniciada(TRUE)
      
    }
  )
  
  # PÁGINA INICIAL
  
  pagina_inicial <- function() {
    
    div(
      
      style = "
        width: 100%;
        min-height: 100vh;
        display: flex;
        justify-content: center;
        align-items: center;
        background-color: #f8f9fa;
      ",
      
      div(
        
        style = "
          width: 90%;
          max-width: 950px;
          background: white;
          border-radius: 15px;
          padding: 50px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08);
          margin: 30px;
        ",
   
        # TÍTULO
        
        div(
          
          style = "
            text-align: center;
            margin-bottom: 35px;
          ",
          
          h1(
            "Consulta de Séries Históricas",
            style = "
              font-weight: 700;
              margin-bottom: 12px;
            "
          ),
          
          h4(
            "Dados agrícolas por região e safra",
            style = "
              font-weight: 400;
              color: #6c757d;
            "
          )
          
        ),
        
        # TEXTO
        
        div(
          
          style = "
            font-size: 17px;
            line-height: 1.8;
            text-align: justify;
            color: #343a40;
          ",
          
          p(
            "Este aplicativo permite consultar e visualizar
            séries históricas de diferentes culturas agrícolas,
            possibilitando a análise e comparação dos dados
            entre regiões ao longo das safras."
          ),
          
          p(
            "O usuário poderá selecionar a cultura de interesse,
            uma ou mais regiões, o período de análise e as
            variáveis disponíveis na base de dados."
          ),
          
          p(
            "Os resultados são apresentados por meio de
            gráficos interativos, permitindo explorar a
            evolução dos dados ao longo das safras e comparar
            diferentes regiões."
          ),
          
        ),
       
        # CULTURAS
        
        div(
          
          style = "
            background-color: #f1f3f5;
            border-radius: 10px;
            padding: 25px;
            margin-top: 30px;
            margin-bottom: 30px;
            text-align: center;
          ",
          
          h4(
            "Culturas disponíveis",
            style = "
              font-weight: 600;
              margin-bottom: 15px;
            "
          ),
          
          p(
            "Algodão • Amendoim • Arroz • Aveia • Canola •
            Centeio • Cevada • Feijão • Gergelim • Girassol •
            Mamona • Milho • Soja • Sorgo • Trigo • Triticale",
            
            style = "
              font-size: 18px;
              margin: 0;
            "
          )
          
        ),
        
        # COMO UTILIZAR
        
        div(
          
          style = "
            border-left: 4px solid #0d6efd;
            padding-left: 20px;
            margin: 30px 0;
          ",
          
          h4(
            "Como utilizar",
            style = "
              font-weight: 600;
            "
          ),
          
          tags$ol(
            
            tags$li(
              "Escolha a cultura."
            ),
            
            tags$li(
              "Selecione a Federação."
            ),
            
            tags$li(
              "Opcionalmente, selecione uma ou mais regiões e/ou estados."
            ),
            
            tags$li(
              "Defina o período de interesse."
            ),
            
            tags$li(
              "Escolha uma ou mais variáveis que deseja analisar."
            ),
            
            tags$li(
              "Explore os gráficos interativos."
            )
            
          )
          
        ),
        
        # BOTÃO
        
        div(
          
          style = "
            text-align: center;
            margin-top: 35px;
          ",
          
          actionButton(
            
            inputId = "iniciar_consulta",
            
            label = "Iniciar consulta",
            
            icon = icon(
              "magnifying-glass"
            ),
            
            class = "btn-primary btn-lg",
            
            style = "
              padding: 13px 40px;
              font-size: 18px;
              border-radius: 8px;
            "
            
          )
          
          ,
          
          # FONTE DOS DADOS
          
          div(
            
            style = "
              margin-top: 20px;
              font-size: 12px;
              line-height: 1.5;
              text-align: center;
              color: #6c757d;
            ",
            
            p(
              "Os dados utilizados neste aplicativo foram obtidos
              da Companhia Nacional de Abastecimento (CONAB),
              por meio das Séries Históricas de Safras,
              disponíveis no portal oficial da instituição."
            ),
            
            p(
              style = "margin-top: 5px;",
              
              strong("Fonte dos dados: "),
              
              a(
                "CONAB — Séries Históricas de Safras",
                href = "https://www.gov.br/conab/pt-br/atuacao/informacoes-agropecuarias/safras/series-historicas",
                target = "_blank"
              )
            )
            
          )
          
        )
        
      )
      
    )
    
  }
  
  
  # PÁGINA DE CONSULTA
  
  pagina_consulta <- function() {
    
    page_sidebar(
      
      
      # SIDEBAR
      
      sidebar = sidebar(
        
        width = 320,
        
        h4("Filtros"),
        
        # CULTURA
        
        selectInput(
          
          inputId = "cultura",
          
          label = "Cultura:",
          
          choices = c(
            "Selecione uma cultura..." = "",
            "Algodão" = "Algodão",
            "Amendoim" = "Amendoim",
            "Arroz" = "Arroz",
            "Aveia" = "Aveia",
            "Canola" = "Canola",
            "Centeio" = "Centeio",
            "Cevada" = "Cevada",
            "Feijão" = "Feijão",
            "Gergelim" = "Gergelim",
            "Girassol" = "Girassol",
            "Mamona" = "Mamona",
            "Milho" = "Milho",
            "Soja" = "Soja",
            "Sorgo" = "Sorgo",
            "Trigo" = "Trigo",
            "Triticale" = "Triticale"
          ),
          
          selected = ""
          
        ),
        
        # FEDERAÇÃO
        
        selectizeInput(
          
          inputId = "federacao",
          
          label = "Federação:",
          
          choices = c(
            "BRASIL" = "BRASIL"
          ),
          
          multiple = FALSE,
          
          selected = "BRASIL",
          
          options = list(
            placeholder = "Selecione a Federação"
          )
          
        ),
        
        # REGIÃO
        
        selectizeInput(
          
          inputId = "regiao",
          
          label = "Região (opcional):",
          
          choices = NULL,
          
          multiple = TRUE,
          
          options = list(
            placeholder = "Selecione uma ou mais regiões",
            plugins = list("remove_button")
          )
          
        ),
        
        # ESTADO
        
        selectizeInput(
          
          inputId = "estado",
          
          label = "Estado (opcional):",
          
          choices = NULL,
          
          multiple = TRUE,
          
          options = list(
            placeholder = "Selecione um ou mais estados",
            plugins = list("remove_button")
          )
          
        ),
        
        # ANOS
        
        uiOutput(
          "selecao_anos"
        ),
        
        # VARIÁVEIS
        
        uiOutput(
          "selecao_variaveis"
        ),
        
        hr(),
        
        # LIMPAR FILTROS
        
        actionButton(
          
          inputId = "resetar",
          
          label = "Limpar filtros",
          
          icon = icon(
            "rotate-left"
          ),
          
          class = "btn-secondary"
          
        ),
        
        br(),
        br(),
        
        
        # VOLTAR
        
        actionButton(
          
          inputId = "voltar_inicio",
          
          label = "Voltar ao início",
          
          icon = icon(
            "house"
          ),
          
          class = "btn-outline-secondary"
          
        )
        
      ),
      
      
      # CORPO
      
      div(
        
        style = "
          padding: 10px;
        ",
        
        h2(
          textOutput("titulo"),
          class = "mb-3"
        ),
        
        uiOutput(
          "informacao_filtro"
        ),
        
        uiOutput(
          "mensagem_consulta"
        ),
        
        uiOutput(
          "graficos"
        )
      )
      
    )
    
  }
  
  
  # RENDERIZAR PÁGINA
  
  output$pagina <- renderUI({
    
    if (consulta_iniciada()) {
      
      pagina_consulta()
      
    } else {
      
      pagina_inicial()
      
    }
    
  })
  
  
  # VOLTAR AO INÍCIO
  
  observeEvent(
    
    input$voltar_inicio,
    
    {
      
      consulta_iniciada(FALSE)
      
    }
    
  )
  
  
  # SELECIONAR BASE DA CULTURA
  
  dados_cultura <- reactive({
    
    req(
      input$cultura,
      input$cultura != ""
    )
    
    
    switch(
      
      input$cultura,
      
      "Algodão" = algodao,
      
      "Amendoim" = amendoim,
      
      "Arroz" = arroz,
      
      "Aveia" = aveia,
      
      "Canola" = canola,
      
      "Centeio" = centeio,
      
      "Cevada" = cevada,
      
      "Feijão" = feijao,
      
      "Gergelim" = gergelim,
      
      "Girassol" = girassol,
      
      "Mamona" = mamona,
      
      "Milho" = milho,
      
      "Soja" = soja,
      
      "Sorgo" = sorgo,
      
      "Trigo" = trigo,
      
      "Triticale" = triticale,
      
      NULL
      
    )
    
  })
  
  # ATUALIZAR REGIÃO E ESTADO
  
  observeEvent(
    
    input$cultura,
    
    {
      
      req(
        input$cultura,
        input$cultura != ""
      )
      
      dados <- dados_cultura()
      
      
      valores <- unique(
        dados$Região
      )
      
      
      valores <- valores[
        !is.na(valores) &
          valores != "" &
          valores != "BRASIL"
      ]
      
      # REGIÕES
      
      regioes <- c(
        "CENTRO-OESTE",
        "CENTRO-SUL",
        "NORDESTE",
        "NORTE",
        "NORTE/NORDESTE",
        "SUDESTE",
        "SUL"
      )
      
      
      regioes_disponiveis <- intersect(
        regioes,
        valores
      )
      
      # ESTADOS
      
      estados <- c(
        
        "Acre" = "AC",
        "Alagoas" = "AL",
        "Amapá" = "AP",
        "Amazonas" = "AM",
        "Bahia" = "BA",
        "Ceará" = "CE",
        "Distrito Federal" = "DF",
        "Espírito Santo" = "ES",
        "Goiás" = "GO",
        "Maranhão" = "MA",
        "Mato Grosso" = "MT",
        "Mato Grosso do Sul" = "MS",
        "Minas Gerais" = "MG",
        "Pará" = "PA",
        "Paraíba" = "PB",
        "Paraná" = "PR",
        "Pernambuco" = "PE",
        "Piauí" = "PI",
        "Rio de Janeiro" = "RJ",
        "Rio Grande do Norte" = "RN",
        "Rio Grande do Sul" = "RS",
        "Rondônia" = "RO",
        "Roraima" = "RR",
        "Santa Catarina" = "SC",
        "São Paulo" = "SP",
        "Sergipe" = "SE",
        "Tocantins" = "TO"
        
      )
      
      # SOMENTE ESTADOS PRESENTES NA BASE
      
      estados_disponiveis <- estados[
        unname(estados) %in% valores
      ]
      
      # ATUALIZAR REGIÕES
      
      updateSelectizeInput(
        
        session,
        
        "regiao",
        
        choices = regioes_disponiveis,
        
        selected = character(0),
        
        server = TRUE
        
      )
      
      # ATUALIZAR ESTADOS
      
      updateSelectizeInput(
        
        session,
        
        "estado",
        
        choices = estados_disponiveis,
        
        selected = character(0),
        
        server = TRUE
        
      )
      
    }
    
  )
  
  # ANOS DISPONÍVEIS
  
  anos_disponiveis <- reactive({
    
    req(
      input$cultura,
      input$cultura != ""
    )
    
    dados <- dados_cultura()
    
    ordenar_anos(
      dados$Ano
    )
    
  })
  
  # INTERFACE DOS ANOS
  
  output$selecao_anos <- renderUI({
    
    req(
      input$cultura,
      input$cultura != ""
    )
    
    anos <- anos_disponiveis()
    
    tagList(
      
      selectInput(
        
        inputId = "ano_inicio",
        
        label = "Ano inicial:",
        
        choices = c(
          "Selecione..." = "",
          anos
        ),
        
        selected = ""
        
      ),
      
      selectInput(
        
        inputId = "ano_fim",
        
        label = "Ano final:",
        
        choices = c(
          "Selecione..." = "",
          anos
        ),
        
        selected = ""
        
      )
      
    )
    
  })
  
  # VARIÁVEIS DISPONÍVEIS
  
  variaveis_disponiveis <- reactive({
    
    req(
      input$cultura,
      input$cultura != ""
    )
    
    dados <- dados_cultura()
    
    setdiff(
      names(dados),
      c(
        "Região",
        "Ano"
      )
    )
    
  })
  
  # SELEÇÃO DAS VARIÁVEIS
  
  output$selecao_variaveis <- renderUI({
    
    req(
      input$cultura,
      input$cultura != ""
    )
    
    vars <- variaveis_disponiveis()
    
    selectizeInput(
      
      inputId = "variaveis",
      
      label = "Variáveis:",
      
      choices = vars,
      
      selected = character(0),
      
      multiple = TRUE,
      
      options = list(
        
        placeholder = "Selecione uma ou mais variáveis",
        
        plugins = list(
          "remove_button"
        ),
        
        maxOptions = 10
        
      )
      
    )
    
  })
  
  # LIMPAR FILTROS
  
  observeEvent(
    
    input$resetar,
    
    {
      
      updateSelectInput(
        session,
        "cultura",
        selected = ""
      )
      
      updateSelectizeInput(
        session,
        "federacao",
        choices = c(
          "BRASIL" = "BRASIL"
        ),
        selected = "BRASIL"
      )
      
      updateSelectizeInput(
        session,
        "regiao",
        choices = NULL,
        selected = character(0)
      )
      
      updateSelectizeInput(
        session,
        "estado",
        choices = NULL,
        selected = character(0)
      )
      
    }
    
  )
  
  # DADOS FILTRADOS
  
  dados_filtrados <- reactive({
    
    req(
      
      input$cultura,
      input$cultura != "",
      
      input$federacao,
      input$federacao != "",
      
      input$ano_inicio,
      input$ano_inicio != "",
      
      input$ano_fim,
      input$ano_fim != ""
      
    )
    
    dados <- dados_cultura()
    
    
    # ANOS
    
    anos <- ordenar_anos(
      dados$Ano
    )
    
    
    inicio <- match(
      input$ano_inicio,
      anos
    )
    
    
    fim <- match(
      input$ano_fim,
      anos
    )
    
    
    req(
      !is.na(inicio),
      !is.na(fim)
    )
    
    # Inverter se necessário
    if (inicio > fim) {
      
      temp <- inicio
      
      inicio <- fim
      
      fim <- temp
      
    }
    
    anos_selecionados <- anos[
      inicio:fim
    ]
    
    
    # FILTRO GEOGRÁFICO
    
    if (
      !is.null(input$regiao) &&
      length(input$regiao) > 0
    ) {
      
      locais_selecionados <- input$regiao
      
    } else if (
      !is.null(input$estado) &&
      length(input$estado) > 0
    ) {
      
      locais_selecionados <- input$estado
      
    } else {
      
      locais_selecionados <- input$federacao
      
    }
    
    # APLICAR FILTROS
    
    dados %>%
      filter(
        Região %in% locais_selecionados,
        Ano %in% anos_selecionados
      )
    
  })
  
  
  # TÍTULO
  
  output$titulo <- renderText({
    
    if (
      is.null(input$cultura) ||
      input$cultura == ""
    ) {
      
      return(
        "Consulta de Séries Históricas"
      )
      
    }
    
    paste0(
      input$cultura,
      " — Série Histórica"
    )
    
  })
  
  # MENSAGEM DA CONSULTA
  
  output$mensagem_consulta <- renderUI({
    
    # SEM CULTURA
    
    if (
      is.null(input$cultura) ||
      input$cultura == ""
    ) {
      
      return(
        
        div(
          
          style = "
            padding: 35px;
            text-align: center;
            color: #6c757d;
          ",
          
          icon(
            "filter",
            class = "fa-3x"
          ),
          
          br(),
          br(),
          
          h4(
            "Selecione os filtros para iniciar a consulta"
          ),
          
          p(
            "Escolha uma cultura, a Federação,
            o período e as variáveis de interesse.
            Região e Estado são opcionais."
          )
          
        )
        
      )
      
    }
    
    # FILTROS INCOMPLETOS
    
    if (
      is.null(input$federacao) ||
      input$federacao == "" ||
      is.null(input$ano_inicio) ||
      input$ano_inicio == "" ||
      is.null(input$ano_fim) ||
      input$ano_fim == ""
    ) {
      
      return(
        
        div(
          
          class = "alert alert-warning",
          
          strong(
            "Complete os filtros."
          ),
          
          br(),
          
          "Selecione a Federação, ano inicial e ano final
          para visualizar os resultados."
          
        )
        
      )
      
    }
    
    # VARIÁVEL NÃO SELECIONADA
     
    if (
      is.null(input$variaveis) ||
      length(input$variaveis) == 0
    ) {
      
      return(
        
        div(
          
          class = "alert alert-info",
          
          strong(
            "Selecione pelo menos uma variável."
          ),
          
          br(),
          
          "Escolha uma ou mais variáveis na barra lateral
          para gerar os gráficos."
          
        )
        
      )
      
    }
    
    NULL
    
  })
  
  # INFORMAÇÃO DOS FILTROS
  
  output$informacao_filtro <- renderUI({
    
    req(
      
      input$cultura,
      input$cultura != "",
      
      input$federacao,
      input$federacao != "",
      
      input$ano_inicio,
      input$ano_inicio != "",
      
      input$ano_fim,
      input$ano_fim != ""
      
    )
    
    div(
      
      class = "alert alert-info",
      
      strong(
        "Consulta atual:"
      ),
      
      br(),
      
      paste0(
        "Cultura: ",
        input$cultura
      ),
      
      br(),
      
      paste0(
        "Federação: ",
        input$federacao
      ),
      
      if (
        !is.null(input$regiao) &&
        length(input$regiao) > 0
      ) {
        
        tagList(
          
          br(),
          
          paste0(
            "Região: ",
            paste(
              input$regiao,
              collapse = ", "
            )
          )
          
        )
        
      },
      
      if (
        !is.null(input$estado) &&
        length(input$estado) > 0
      ) {
        
        tagList(
          
          br(),
          
          paste0(
            "Estado: ",
            paste(
              input$estado,
              collapse = ", "
            )
          )
          
        )
        
      },
      
      br(),
      
      paste0(
        "Período: ",
        input$ano_inicio,
        " a ",
        input$ano_fim
      )
      
    )
    
  })
  
  # ÁREA DOS GRÁFICOS
  
  output$graficos <- renderUI({
    
    req(
      
      input$variaveis,
      length(input$variaveis) > 0,
      
      input$cultura,
      input$cultura != "",
      
      input$federacao,
      input$federacao != "",
      
      input$ano_inicio,
      input$ano_inicio != "",
      
      input$ano_fim,
      input$ano_fim != ""
      
    )
    
    tagList(
      
      lapply(
        
        seq_along(
          input$variaveis
        ),
        
        function(i) {
          
          plotlyOutput(
            
            outputId = paste0(
              "grafico_",
              i
            ),
            
            height = "500px"
            
          )
          
        }
        
      )
      
    )
    
  })
  
  # GRÁFICOS INTERATIVOS
  
  observe({
    
    req(
      input$variaveis,
      length(input$variaveis) > 0
    )
    
    lapply(
      
      seq_along(
        input$variaveis
      ),
      
      function(i) {
        
        local({
          
          ii <- i
          
          variavel <- input$variaveis[ii]
          
          
          output[[paste0(
            "grafico_",
            ii
          )]] <- renderPlotly({
            
           # DADOS
            
            dados <- dados_filtrados()
            
            
            req(
              nrow(dados) > 0,
              variavel %in% names(dados)
            )
            
            # PREPARAR DADOS
            
            dados_plot <- dados %>%
              
              select(
                Região,
                Ano,
                Valor = all_of(variavel)
              ) %>%
              
              filter(
                !is.na(Valor)
              )
            
            req(
              nrow(dados_plot) > 0
            )
            
            # ORDENAR ANOS
            
            niveis_ano <- ordenar_anos(
              dados_plot$Ano
            )
            
            
            dados_plot$Ano <- factor(
              dados_plot$Ano,
              levels = niveis_ano
            )
            
            # TOOLTIP
            
            dados_plot <- dados_plot %>%
              
              mutate(
                
                tooltip = paste0(
                  
                  "<b>Região:</b> ",
                  Região,
                  
                  "<br><b>Ano:</b> ",
                  as.character(Ano),
                  
                  "<br><b>",
                  variavel,
                  ":</b> ",
                  
                  format(
                    Valor,
                    big.mark = ".",
                    decimal.mark = ",",
                    trim = TRUE
                  )
                  
                )
                
              )
            
            # GRÁFICO
            
            p <- ggplot(
              
              dados_plot,
              
              aes(
                x = Ano,
                y = Valor,
                group = Região,
                color = Região,
                text = tooltip
              )
              
            ) +
              
              geom_line(
                linewidth = 0.9
              ) +
              
              geom_point(
                size = 1.5
              ) +
              
              labs(
                
                title = variavel,
                
                subtitle = paste(
                  input$cultura,
                  "— comparação entre regiões"
                ),
                
                x = "Ano",
                
                y = variavel,
                
                color = "Região"
                
              ) +
              
              theme_classic(
                base_size = 13
              ) +
              
              theme(
                
                plot.title = element_text(
                  face = "bold",
                  size = 16
                ),
                
                plot.subtitle = element_text(
                  size = 12
                ),
                
                axis.text.x = element_text(
                  angle = 45,
                  hjust = 1
                ),
                
                legend.position = "bottom",
                
                legend.title = element_text(
                  face = "bold"
                )
                
              )
            
            # CONVERTER PARA PLOTLY
            
            ggplotly(
              
              p,
              
              tooltip = "text"
              
            ) %>%
              
              layout(
                
                hoverlabel = list(
                  align = "left"
                ),
                
                legend = list(
                  orientation = "h",
                  x = 0,
                  y = -0.18
                )
                
              )
            
          })
          
        })
        
      }
      
    )
    
  })
  
}


# EXECUTAR APLICAÇÃO

shinyApp(
  ui = ui,
  server = server)