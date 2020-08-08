library(shiny)
library(shinydashboard)
#library(shinyBS)
#library(shinycssloaders)
#library(shinyjs)
#library(shinyWidgets)


library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
#library(circlepackeR)
library(ggplot2)
#library(shiny.i18n)


produtos_data <- read_excel("PROJETOS_PRODUTOS.xlsm", sheet=1)
produtos_data$ENTREGA=as.Date(produtos_data$ENTREGA , origin = "1899-12-30")

institucional_data <- read_excel("PROJETOS_INSTITUCIONAL.xlsm", sheet=1)
institucional_data$ENTREGA=as.Date(institucional_data$ENTREGA , origin = "1899-12-30")




ui = dashboardPage(skin = "green",
                   dashboardHeader(title = span(img(src='https://www.sicoobcoopvale.com.br/image/pinlogo.png', width = 40), "Marketing")),
                   dashboardSidebar(
                     sidebarMenu(
                       selectInput(inputId = "mesref", label = "Inicio do periodo", selected = "Janeiro", c("Janeiro" = "2020-01-01",
                                                                                                            "Fevereiro" = "2020-02-01",
                                                                                                            "Março" = "2020-03-01",
                                                                                                            "Abril" = "2020-04-01",
                                                                                                            "Maio" = "2020-05-01",
                                                                                                            "Junho" = "2020-06-01",
                                                                                                            "Julho" = "2020-07-01")),
                       menuItem("ASCOM", tabName = 'ascom_tab', icon = icon("comments")),
                       menuItem("Institucional", tabName = 'institucional_tab', icon = icon("building")),
                       menuItem("Produtos", tabName = 'produtos_tab', icon = icon("bank")),
                       menuItem("Suporte de Qualidade", menuSubItem("Analise de dados", tabName = 'dados_tab', icon = icon("sitemap")),
                                menuSubItem("Audiovisual", tabName = 'audiovisual_tab', icon = icon("camera")),
                                menuSubItem("Arquitetura", tabName = 'arquitetura_tab', icon = icon("archway")),
                                menuSubItem("Midias digitais", tabName = 'mdigitais_tab', icon = icon("instagram")),
                                menuSubItem("Projetos", tabName = 'projetos_tab', icon = icon("map")),
                                menuSubItem("Redator", tabName = 'redacao_tab', icon = icon("pencil")))
                       
                     )
                   ),
                   dashboardBody(fluidPage(
                     tabItems(
                       tabItem(tabName = 'produtos_tab', h2("Resultados mensais do time de Podutos, Marketing - Central"),
                               fluidRow( infoBoxOutput("producao_total_box"), infoBoxOutput("pecas_mes_box"), infoBoxOutput("pecas_aprovacao"), infoBoxOutput("alteracoes_box") ),
                               fluidRow( wellPanel("Quantidade de alterações realizadas mensalmente (Produtos e Institucional)", plotlyOutput('temporal_alteracoes'))),
                               box(title = "Proporção de produção por colaborador", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("proporcao_colaborador_plot")),
                               box(title = "Tipos de peças estáticas produzidas", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("pecas_mais_produzidas")),
                               box(title = "Produção diária por colaborador", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("producao_mensal")),
                               box(title = "Características dos serviços prestados", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("servicos_prestados")),
                               box(title = "Cooperativas que mais acionaram o núcleo de Produtos", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("pedidos_coop"))
                       ),
                       tabItem(tabName = 'institucional_tab', h2("Resultados mensais do time de Institucional, Marketing - Central"),
                               fluidRow(infoBoxOutput("producao_total_institucional_box"), infoBoxOutput("pecas_mes_institucional_box"),
                                        infoBoxOutput("pecas_institucional_aprovacao"), infoBoxOutput("alteracoes_institucional_box")),
                               fluidRow(wellPanel("Quantidade de alterações realizadas mensalmente (Produtos e Institucional)", plotlyOutput('temporal_alteracoes_institucional'))),
                               box(title = "Proporção de produção por colaborador", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("proporcao_colaborador_institucional_plot")),
                               box(title = "Tipos de peças estáticas produzidas", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("pecas_mais_produzidas_institucional")),
                               box(title = "Produção diária por colaborador", solidHeader = TRUE, collapsible = TRUE, plotlyOutput("producao_mensal_institucional")),
                               box(),
                               box(),
                               box()),
                       tabItem(tabName = 'ascom_tab'
                       ),
                       tabItem(tabName = 'dados_tab'
                       ),
                       tabItem(tabName = 'audiovisual_tab'
                       ),
                       tabItem(tabName = 'arquitetura_tab'
                       ),
                       tabItem(tabName = 'mdigitais_tab'
                       ),
                       tabItem(tabName = 'projetos_tab'
                       ),
                       tabItem(tabName = 'redacao_tab'
                       )
                       
                     )
                     
                   )
                   )
)