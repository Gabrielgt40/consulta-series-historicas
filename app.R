# SHINY APP - CONSULTA DE SÉRIES HISTÓRICAS

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)


# CARREGAR DADOS

arquivo <- "dados_gerais.xlsx"

algodao <- read_excel(arquivo, sheet = "Algodão")
amendoim <- read_excel(arquivo, sheet = "Amendoim")
arroz <- read_excel(arquivo, sheet = "Arroz")
aveia <- read_excel(arquivo, sheet = "Aveia")
canola <- read_excel(arquivo, sheet = "Canola")

# PREPARAR DADOS

preparar_dados <- function(df) {
  
  df <- df %>%
    mutate(Região = as.character(Região),
           Ano = as.character(Ano))
  
  # Identificar variáveis quantitativas
  
  colunas_valores <- setdiff(
    names(df),
    c("Região",
      "Ano"))
  
  # Converter "-" e valores não numéricos para NA
  
  df <- df %>%
    mutate(
      across(
        all_of(colunas_valores),
        ~ suppressWarnings(
          as.numeric(
            ifelse(
              .x == "-",
              NA,
              .x)))))

  # Remover registros sem região ou ano
  
  df <- df %>%
    filter(
      !is.na(Região),
      Região != "",
      !is.na(Ano),
      Ano != "")

  df
  
  }

algodao <- preparar_dados(algodao)
amendoim <- preparar_dados(amendoim)
arroz <- preparar_dados(arroz)
aveia <- preparar_dados(aveia)
canola <- preparar_dados(canola)

# ORDENAR AS SAFRAS

ordenar_anos <- function(x) {
  
  x <- unique(x)
  
  ano_numerico <- suppressWarnings(
    as.numeric(
      sub(
        "/.*",
        "",
        x)) )

  if (sum(!is.na(ano_numerico)) > 0) {
    
    x[
      order(
        ano_numerico,
        na.last = TRUE)
    ]
    
  } else {
    
    sort(x)
    
  }
  
}

# INTERFACE

ui <- page_fillable(
  
  
   # PÁGINA INICIAL
  
  uiOutput(
    "pagina"))

# SERVIDOR

server <- function(
    input,
    output,
    session) {
  
  
  # CONTROLE DA PÁGINA
  # FALSE = página inicial
  # TRUE  = página de consulta
  
  consulta_iniciada <- reactiveVal(FALSE)
  
   # BOTÃO INICIAR CONSULTA
  
  observeEvent(input$iniciar_consulta,
    
    {
      
      consulta_iniciada(TRUE)
      
    }
    
  )
  
  # FUNÇÃO DA PÁGINA INICIAL
  
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
          
          h1("Consulta de Séries Históricas",
            
            style = "
              font-weight: 700;
              margin-bottom: 12px;
            "),
          
          h4("Dados agrícolas por região e safra",
            
            style = "
              font-weight: 400;
              color: #6c757d;
            ")),
        
        # TEXTO
        
        div(style = "
            font-size: 17px;
            line-height: 1.8;
            text-align: justify;
            color: #343a40;
          ",
          
          p("Este aplicativo permite consultar e visualizar
            séries históricas de diferentes culturas agrícolas,
            possibilitando a análise e comparação dos dados
            entre regiões ao longo das safras." ),
          
          p("O usuário poderá selecionar a cultura de interesse,
            uma ou mais regiões, o período de análise e as
            variáveis disponíveis na base de dados."),
          
          p( "Os resultados são apresentados por meio de
            gráficos interativos e tabelas, permitindo explorar
            a evolução dos dados ao longo das safras.")),
        
        
        # CULTURAS
        
        div( style = "
            background-color: #f1f3f5;
            border-radius: 10px;
            padding: 25px;
            margin-top: 30px;
            margin-bottom: 30px;
            text-align: center;
          ",
          
          h4("Culturas disponíveis",
            style = "
              font-weight: 600;
              margin-bottom: 15px;
            "),
          
          p("Algodão  •  Amendoim  •  Arroz  •  Aveia  •  Canola",
            style = "
              font-size: 18px;
              margin: 0;
            ")),
        
        
        # COMO UTILIZAR
         
        div(style = "
            border-left: 4px solid #0d6efd;
            padding-left: 20px;
            margin: 30px 0;
          ",
          
          h4("Como utilizar",
            style = "
              font-weight: 600;
            "),
          
          tags$ol(
            
            tags$li(
              "Escolha a cultura."),
            
            tags$li(
              "Selecione uma ou mais regiões."),
            
            tags$li(
              "Defina o período de interesse."),
            
            tags$li(
              "Escolha as variáveis que deseja analisar."),
            
            tags$li(
              "Explore os gráficos e a tabela."))),
        
        
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
              "magnifying-glass"),
            
            class = "btn-primary btn-lg",
            
            style = "
              padding: 13px 40px;
              font-size: 18px;
              border-radius: 8px;
            "))))
    
  }
  
  
  # FUNÇÃO DA PÁGINA DE CONSULTA
  
  pagina_consulta <- function() {
    
    page_sidebar(
      
      
     # SIDEBAR
 
      sidebar = sidebar(width = 320,
        
        h4("Filtros"),
        
        # CULTURA
         
        selectInput(inputId = "cultura",
          label = "Cultura:",
          choices = c(
            "Selecione uma cultura..." = "",
            "Algodão",
            "Amendoim",
            "Arroz",
            "Aveia",
            "Canola"),
          selected = ""),
        
        
        # REGIÃO
        
        selectizeInput( inputId = "regiao",
          label = "Região:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Selecione uma ou mais regiões",
            plugins = list(
              "remove_button"))),
      
        # ANOS
        
        uiOutput("selecao_anos"),
        
        # VARIÁVEIS
         
        uiOutput(
          "selecao_variaveis"),
        
        hr(),
        
        # LIMPAR FILTROS
        
        actionButton(inputId = "resetar",
          label = "Limpar filtros",
          icon = icon(
            "rotate-left"),
          class = "btn-secondary"),
        
        br(),
        br(),
     
        # VOLTAR
         
        actionButton( inputId = "voltar_inicio",
          label = "Voltar ao início",
          icon = icon(
            "house"),
          class = "btn-outline-secondary")),
      
       # CORPO
      
      div(style = "
          padding: 10px;
        ",
        
        h2( textOutput(
            "titulo" ),
          class = "mb-3"),
        
        
        uiOutput(
          "informacao_filtro" ),
        
        
       # MENSAGEM INICIAL DA CONSULTA
         
        uiOutput(
          "mensagem_consulta"),
        
        
        # GRÁFICOS
        
        uiOutput(
          "graficos"),
        
        
        # TABELA
         
        uiOutput(
          "area_tabela")))
    
  }
  
  
  # RENDERIZAR PÁGINA

   output$pagina <- renderUI({
    
    if (
      consulta_iniciada()) {
      pagina_consulta()
      
    } else {
      
      pagina_inicial()
      
    }
    
  })
  
  # VOLTAR PARA O INÍCIO
   observeEvent(input$voltar_inicio,
    
    {
      
      consulta_iniciada(FALSE)
      
    }
    
  )
  
  # SELECIONAR A BASE
   dados_cultura <- reactive({
    
    req(input$cultura, input$cultura != "")
    
    
    if (input$cultura == "Algodão") 
      
      {
      
      algodao
      
    } else if (
      input$cultura == "Amendoim") 
      
      {
      
      amendoim
      
    } else if ( input$cultura == "Arroz")
      
      {
      
      arroz
      
    } else if (input$cultura == "Aveia")
      
      {
      
      aveia
      
    } else if (input$cultura == "Canola") 
      
      {
      
      canola
      
    }
    
  })
  
  
  # ATUALIZAR REGIÕES
   observeEvent(input$cultura,
    
    {
      
      req(
        input$cultura,
        input$cultura != "")

      dados <- dados_cultura()
      
      regioes_cultura <- sort(
        
        unique(
          dados$Região))
      
      regioes_cultura <- regioes_cultura[
        !is.na(regioes_cultura)
      ]
      
      
      updateSelectizeInput(session, "regiao", choices = regioes_cultura,
        selected = character(0))
      
    },
    
    ignoreInit = TRUE)
  
  # ANOS DISPONÍVEIS
  anos_disponiveis <- reactive({
    
    req(
      input$cultura,
      input$cultura != "")
    
    dados <- dados_cultura()
    
    ordenar_anos(
      dados$Ano)
    
  })
  
 # INTERFACE DOS ANOS
 
  output$selecao_anos <- renderUI({
    
    req(
      input$cultura,
      input$cultura != "")
    
    anos <- anos_disponiveis()
    
    tagList(
      
      selectInput(
        
        inputId = "ano_inicio",
        
        label = "Ano inicial:",
        
        choices = c(
          
          "Selecione..." = "",
          
          anos),
        
        selected = ""),
      
      selectInput(
        
        inputId = "ano_fim",
        
        label = "Ano final:",
        
        choices = c(
          
          "Selecione..." = "",
          
          anos),
        
        selected = ""))
    
  })
  
  
  # VARIÁVEIS DISPONÍVEIS
  
  variaveis_disponiveis <- reactive({
  
    req(
      input$cultura,
      input$cultura != "")
  
    dados <- dados_cultura()
    
    setdiff(
      
      names(dados),
      
      c(
        "Região",
        "Ano"))
    
  })
  
  # SELEÇÃO DAS VARIÁVEIS
  
  output$selecao_variaveis <- renderUI({
    req(
      input$cultura,
      input$cultura != "")
    
    vars <- variaveis_disponiveis()
    
    checkboxGroupInput(
      
      inputId = "variaveis",
      
      label = "Variáveis:",
      
      choices = vars,
      
      selected = character(0))
    
  })
  
  # LIMPAR FILTROS
  
   observeEvent(
    input$resetar,
    
    {
      
      # Cultura
      
      updateSelectInput(
        
        session,
        
        "cultura",
        
        selected = "")
      
      # Região
      
      updateSelectizeInput(
        
        session,
        
        "regiao",
        
        choices = NULL,
        
        selected = character(0))
      
    }
    
  )
  
  # DADOS FILTRADOS
  
   dados_filtrados <- reactive({
    
    req(
      
      input$cultura,
      input$cultura != "",
      
      input$regiao,
      length(input$regiao) > 0,
      
      input$ano_inicio,
      input$ano_inicio != "",
      
      input$ano_fim,
      input$ano_fim != "")
    
    dados <- dados_cultura()
    
    anos <- ordenar_anos(
      dados$Ano)
    
    inicio <- match(
      input$ano_inicio,
      anos)
    
    fim <- match(
      input$ano_fim,
      anos)
    
    req(
      !is.na(inicio),
      !is.na(fim))
    
    # Caso o usuário inverta os anos
    
    if (
      inicio > fim ) {
      
      temp <- inicio
      
      inicio <- fim
      
      fim <- temp
      
    }
    anos_selecionados <- anos[
      inicio:fim
    ]
    
    dados %>%
      
      filter(
        
        Região %in% input$regiao,
        
        Ano %in% anos_selecionados)
    
  })
  
  # TÍTULO
   
  output$titulo <- renderText({
    
    if (
      is.null(input$cultura) ||
      input$cultura == "") {
      "Consulta de Séries Históricas"
      
    } else {
      
      paste0(
        input$cultura,
        " — Série Histórica")
    }
    
  })
  
  # MENSAGEM DA CONSULTA
 
  output$mensagem_consulta <- renderUI({
    
    # Se ainda não escolheu cultura
    
    if (
      is.null(input$cultura) ||
      input$cultura == "") {
      
      return(
        div(
          
          style = "
            padding: 35px;
            text-align: center;
            color: #6c757d;
          ",
          icon(
            "filter",
            class = "fa-3x"),
          br(),
          br(),
          h4(
            "Selecione os filtros para iniciar a consulta"),
          
          p("Escolha uma cultura, uma ou mais regiões,
            o período e as variáveis de interesse.")))
      
    }
    
    # Se escolheu cultura, mas ainda não completou filtros
    
    if (
      is.null(input$regiao) ||
      length(input$regiao) == 0 ||
      is.null(input$ano_inicio) ||
      input$ano_inicio == "" ||
      is.null(input$ano_fim) ||
      input$ano_fim == "") 
      
      {
      
      return(
        div( class = "alert alert-warning",
          strong(
            "Complete os filtros."),
          br(),
          "Selecione região, ano inicial e ano final
          para visualizar os resultados.") )
      
    }
    
    NULL
    
  })
  
  # INFORMAÇÕES DOS FILTROS
 
  output$informacao_filtro <- renderUI({
    
    req(
      input$cultura,
      input$cultura != "",
      input$regiao,
      length(input$regiao) > 0,
      input$ano_inicio,
      input$ano_inicio != "",
      input$ano_fim,
      input$ano_fim != "")
    
    div(
      class = "alert alert-info",
      strong(
        "Consulta atual: "),
      br(),
      paste0(
        "Cultura: ",
        input$cultura),
      br(),
      paste0(
        "Regiões: ",
        paste(
          input$regiao,
          collapse = ", ") ),
      br(),
      paste0(
        "Período: ",
        input$ano_inicio,
        " a ",
        input$ano_fim))
    
  })
  
  # ÁREA DOS GRÁFICOS
  
  output$graficos <- renderUI({
    req(
      input$variaveis,
      length(input$variaveis) > 0,
      input$cultura,
      input$cultura != "",
      input$regiao,
      length(input$regiao) > 0,
      input$ano_inicio,
      input$ano_inicio != "",
      input$ano_fim,
      input$ano_fim != "")
    
    tagList(
      lapply(
        seq_along(
          input$variaveis),
        
        function(i) {
          
          plotlyOutput(
            outputId = paste0(
              "grafico_",
              i ),
            height = "500px")
          
          }
        
      ))
    
  })
  
   # GRÁFICOS INTERATIVOS
  
  observe({ 
    req(
      input$variaveis,
      length(input$variaveis) > 0)
    
    lapply( 
      seq_along(
        input$variaveis),
    
        function(i) { 
        local({
          ii <- i
          variavel <- input$variaveis[ii]
          output[[paste0(
            "grafico_",
            ii
          )]] <- renderPlotly({
             
            dados <- dados_filtrados()
            req(
              nrow(dados) > 0,
              variavel %in% names(dados) )
            
            # Preparar dados
            
            dados_plot <- dados %>%
              select(
                Região,
                Ano,
                Valor = all_of(
                  variavel )) %>%
              filter(
                !is.na(Valor) )
            
            req(
              nrow(dados_plot) > 0)
            
           # Ordenar anos
             
            niveis_ano <- ordenar_anos(
              dados_plot$Ano )
            
            dados_plot$Ano <- factor( 
              dados_plot$Ano,
              levels = niveis_ano )
            
            # Tooltip
            
            dados_plot <- dados_plot %>%
              mutate(
                tooltip = paste0(
                  "<b>Região:</b> ",
                  Região,
                  "<br><b>Ano:</b> ",
                  Ano,
                  "<br><b>",
                  variavel,
                  ":</b> ",
                  format(
                    Valor,
                    big.mark = ".",
                    decimal.mark = ",",
                    trim = TRUE)))
            
            
            # Gráfico
            
            p <- ggplot( 
              dados_plot,
              aes(
                x = Ano,
                y = Valor,
                group = Região,
                color = Região,
                text = tooltip)) +
              geom_line(
                linewidth = 0.9) +
              geom_point(
                size = 2.5) +
              labs(
                 title = variavel,
                subtitle = paste(
                  input$cultura,
                  "— comparação entre regiões"),  
                x = "Ano",
                y = variavel,
                color = "Região" ) +
              theme_classic(
                base_size = 13) +
              theme(
                plot.title = element_text(  
                  face = "bold",
                  size = 16),
                plot.subtitle = element_text( 
                  size = 12),
                axis.text.x = element_text(
                  angle = 45,
                  hjust = 1),
                legend.position = "bottom",
                legend.title = element_text(
                  face = "bold"))
            
            # Plotly
            
            ggplotly(
               p,
               tooltip = "text" ) %>%
              layout(
                hoverlabel = list(  
                  align = "left"  ),
                legend = list(
                  orientation = "h", 
                  x = 0,
                  y = -0.18))
            
          })
   
        })
        
      }
      
    )
    
  })
  
  #  ÁREA DA TABELA
  output$area_tabela <- renderUI({
    req(
      input$variaveis,
      length(input$variaveis) > 0,
      input$cultura,
      input$cultura != "",
      input$regiao,
      length(input$regiao) > 0,
      input$ano_inicio,
      input$ano_inicio != "",
      input$ano_fim,
      input$ano_fim != ""   )
    
    tagList(
      hr(),
      h4(
        "Dados filtrados"),
      DTOutput(
        "tabela"),
      br())
  })
  
  # TABELA INTERATIVA
 
  output$tabela <- renderDT({  
    req(
      input$variaveis,
      length(input$variaveis) > 0 )
    dados <- dados_filtrados()
    req(nrow(dados) > 0)

    dados %>%
      select(
        Região,
        Ano,
        all_of(
          input$variaveis))
    
  },
  
  filter = "top",
  options = list(
    pageLength = 15,
    scrollX = TRUE),
  rownames = FALSE)

}

# EXECUTAR APLICAÇÃO

shinyApp(
  ui = ui,
  server = server)