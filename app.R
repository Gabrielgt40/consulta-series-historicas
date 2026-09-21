# ============================================================
# SHINY APP - CONSULTA DE SÉRIES HISTÓRICAS
# Algodão e Amendoim
# Versão 1.0

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

arquivo <- "dados_gerais.xlsx"

algodao <- read_excel(arquivo, sheet = "Algodão")
amendoim <- read_excel(arquivo, sheet = "Amendoim")
arroz <- read_excel(arquivo, sheet = "Arroz")
aveia <- read_excel(arquivo, sheet = "Aveia")
canola <- read_excel(arquivo, sheet = "Canola")

preparar_dados <- function(df) {
  
  df <- df %>%
    mutate(
      Região = as.character(Região),
      Ano = as.character(Ano))
  
  # Identificar variáveis quantitativas
  colunas_valores <- setdiff(
    names(df),
    c("Região", "Ano"))
  
  # Converter "-" e valores não numéricos para NA
  df <- df %>%
    mutate(
      across(
        all_of(colunas_valores),
        ~ suppressWarnings(
          as.numeric(
            ifelse(.x == "-", NA, .x)))))
  
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
  
  # Extrair o primeiro ano da safra
  ano_numerico <- suppressWarnings(
    as.numeric(
      sub("/.*", "", x)))
  
  # Se conseguir converter, usa a ordem cronológica
  if (sum(!is.na(ano_numerico)) > 0) {
    
    x[
      order(
        ano_numerico,
        na.last = TRUE)]
    
  } else {
    
    sort(x)
    
  }
  
}


# INTERFACE

ui <- page_sidebar(
  
  title = "Consulta de Séries Históricas",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"),
  
  sidebar = sidebar(
    
    width = 320,
    
    h4("Filtros"),
    
    # CULTURA
     
    selectInput(
      inputId = "cultura",
      label = "Cultura:",
      choices = c(
        "Algodão",
        "Amendoim",
        "Arroz",
        "Aveia",
        "Canola"),
      selected = "Algodão"),
    
    # REGIÕES
 
    selectizeInput(
      inputId = "regiao",
      label = "Região:",
      choices = NULL,
      multiple = TRUE,
      
      options = list(
        placeholder = "Selecione uma ou mais regiões",
        plugins = list("remove_button"))),
    
    # ANO INICIAL

    uiOutput(
      "selecao_anos"),
    
    # VARIÁVEIS
   
    uiOutput(
      "selecao_variaveis"
    ),
    
    hr(),
    
    # BOTÃO DE RESET
     
    actionButton(
      inputId = "resetar",
      label = "Restaurar filtros",
      icon = icon("rotate-left"),
      class = "btn-secondary")),
  
  
  # CORPO
  
  div(
    
    h2(
      textOutput("titulo"),
      class = "mb-3"),
    
    uiOutput(
      "informacao_filtro"),
    
    hr(),
    
    # GRÁFICOS
      
    uiOutput(
      "graficos"),
    
    hr(),
    
    # TABELA
   
    h4("Dados filtrados"),
    
    DTOutput(
      "tabela"),
    
    br(),
    
    downloadButton(
      outputId = "download_dados",
      label = "Baixar dados filtrados",
      class = "btn-primary")))


# 6. SERVIDOR

server <- function(input, output, session) {
  
  
  # 6.1. SELECIONAR A BASE
 
  dados_cultura <- reactive({
    
    req(input$cultura)
    
    if (input$cultura == "Algodão") {
      
      algodao
      
    } else if (input$cultura == "Amendoim") {
      
      amendoim
      
    } else if (input$cultura == "Arroz") {
      
      arroz
      
    } else if (input$cultura == "Aveia") {
      
      aveia
      
    } else if (input$cultura == "Canola") {
      
      canola
      
    }
    
  })
  
  
  # 6.2. ATUALIZAR REGIÕES
  
  observeEvent(
    
    input$cultura,
    
    {
      
      dados <- dados_cultura()
      
      regioes_cultura <- sort(
        unique(
          dados$Região))
      
      regioes_cultura <- regioes_cultura[
        !is.na(regioes_cultura)
      ]
      
      # Seleciona BRASIL automaticamente
      selecao_inicial <- if (
        "BRASIL" %in% regioes_cultura) {
        
        "BRASIL"
        
      } else {
        
        regioes_cultura[1]
        
      }
      
      updateSelectizeInput(
        
        session,
        
        "regiao",
        
        choices = regioes_cultura,
        
        selected = selecao_inicial
        
      )
      
    },
    
    ignoreInit = FALSE
    
  )
  
  
  # 6.3. ANOS DISPONÍVEIS
  
  anos_disponiveis <- reactive({
    
    dados <- dados_cultura()
    
    ordenar_anos(
      dados$Ano
    )
    
  })
  
  
 # 6.4. INTERFACE DE ANOS
  
  output$selecao_anos <- renderUI({
    
    anos <- anos_disponiveis()
    
    tagList(
      
      selectInput(
        
        inputId = "ano_inicio",
        
        label = "Ano inicial:",
        
        choices = anos,
        
        selected = anos[1]
        
      ),
      
      selectInput(
        
        inputId = "ano_fim",
        
        label = "Ano final:",
        
        choices = anos,
        
        selected = tail(
          anos,
          1
        )
        
      )
      
    )
    
  })
  
  
  # 6.5. VARIÁVEIS DISPONÍVEIS
 
  variaveis_disponiveis <- reactive({
    
    dados <- dados_cultura()
    
    setdiff(
      names(dados),
      c(
        "Região",
        "Ano"
      )
    )
    
  })
  
  
  # 6.6. SELEÇÃO DAS VARIÁVEIS
   
  output$selecao_variaveis <- renderUI({
    
    vars <- variaveis_disponiveis()
    
    checkboxGroupInput(
      
      inputId = "variaveis",
      
      label = "Variáveis:",
      
      choices = vars,
      
      selected = vars
      
    )
    
  })
  
  
  # 6.7. RESET
  
  observeEvent(
    
    input$resetar,
    
    {
      
      dados <- dados_cultura()
      
      regioes <- sort(
        unique(
          dados$Região
        )
      )
      
      regioes <- regioes[
        !is.na(regioes)
      ]
      
      selecao <- if (
        "BRASIL" %in% regioes
      ) {
        
        "BRASIL"
        
      } else {
        
        regioes[1]
        
      }
      
      updateSelectizeInput(
        
        session,
        
        "regiao",
        
        selected = selecao
        
      )
      
      anos <- ordenar_anos(
        dados$Ano
      )
      
      updateSelectInput(
        
        session,
        
        "ano_inicio",
        
        selected = anos[1]
        
      )
      
      updateSelectInput(
        
        session,
        
        "ano_fim",
        
        selected = tail(
          anos,
          1
        )
        
      )
      
    }
    
  )
  
  
  # 7. DADOS FILTRADOS
   
  dados_filtrados <- reactive({
    
    req(
      input$regiao,
      input$ano_inicio,
      input$ano_fim
    )
    
    dados <- dados_cultura()
    
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
    
    # Corrigir caso o usuário inverta os anos
    if (inicio > fim) {
      
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
        
        Ano %in% anos_selecionados
        
      )
    
  })
  
  
  # 8. TÍTULO
  
  output$titulo <- renderText({
    
    paste0(
      input$cultura,
      " — Série Histórica"
    )
    
  })
  
  
   # 9. INFORMAÇÕES DOS FILTROS
  
  output$informacao_filtro <- renderUI({
    
    req(
      input$regiao
    )
    
    div(
      
      class = "alert alert-info",
      
      strong(
        "Consulta atual: "
      ),
      
      br(),
      
      paste0(
        "Cultura: ",
        input$cultura
      ),
      
      br(),
      
      paste0(
        "Regiões: ",
        paste(
          input$regiao,
          collapse = ", "
        )
      ),
      
      br(),
      
      paste0(
        "Período: ",
        input$ano_inicio,
        " a ",
        input$ano_fim
      )
      
    )
    
  })
  
  
  # 10. ÁREA DOS GRÁFICOS

  output$graficos <- renderUI({
    
    req(
      input$variaveis
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
  
  
  # 11. GRÁFICOS INTERATIVOS
  
  observe({
    
    req(input$variaveis)
    
    lapply(
      seq_along(input$variaveis),
      
      function(i) {
        
        local({
          
          ii <- i
          
          variavel <- input$variaveis[ii]
          
          output[[paste0("grafico_", ii)]] <- renderPlotly({
            
            dados <- dados_filtrados()
            
            req(
              nrow(dados) > 0,
              variavel %in% names(dados)
            )
            
            # ------------------------------------------------
            # Preparar dados
            # ------------------------------------------------
            
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
            
            # ------------------------------------------------
            # Ordenar anos
            # ------------------------------------------------
            
            niveis_ano <- ordenar_anos(
              dados_plot$Ano
            )
            
            dados_plot$Ano <- factor(
              dados_plot$Ano,
              levels = niveis_ano
            )
            
            # ------------------------------------------------
            # Texto apresentado ao passar o mouse
            # ------------------------------------------------
            
            dados_plot <- dados_plot %>%
              mutate(
                
                tooltip = paste0(
                  "<b>Região:</b> ", Região,
                  "<br><b>Ano:</b> ", Ano,
                  "<br><b>", variavel, ":</b> ",
                  format(
                    Valor,
                    big.mark = ".",
                    decimal.mark = ",",
                    trim = TRUE
                  )
                )
                
              )
            
            # ------------------------------------------------
            # Gráfico ggplot
            # ------------------------------------------------
            
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
                size = 2.5
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
            
            # ------------------------------------------------
            # Transformar em Plotly
            # ------------------------------------------------
            
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


# 12. TABELA INTERATIVA

output$tabela <- renderDT({
  
  req(
    input$variaveis
  )
  
  dados_filtrados() %>%
    
    select(
      
      Região,
      
      Ano,
      
      all_of(
        input$variaveis
      )
      
    )
  
},

filter = "top",

extensions = "Buttons",

options = list(
  
  pageLength = 15,
  
  scrollX = TRUE,
  
  dom = "Bfrtip",
  
  buttons = c(
    "copy",
    "csv",
    "excel"
  )
  
),

rownames = FALSE

)


# 13. DOWNLOAD

output$download_dados <- downloadHandler(
  
  filename = function() {
    
    paste0(
      
      tolower(
        input$cultura
      ),
      
      "_serie_historica.csv"
      
    )
    
  },
  
  content = function(file) {
    
    write.csv(
      
      dados_filtrados(),
      
      file,
      
      row.names = FALSE,
      
      fileEncoding = "UTF-8"
      
    )
    
  }
  
)

  }


# 14. EXECUTAR

shinyApp(
  ui = ui,
  server = server
)

install.packages("rsconnect")
library(rsconnect)

rsconnect::writeManifest()
