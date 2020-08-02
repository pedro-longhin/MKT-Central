library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
#library(circlepackeR)
library(ggplot2)
#library(shiny.i18n)
library(rsconnect)


ui = dashboardPage(
  dashboardHeader(title = 'Marketing'),
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = "mesref", label = "Inicio do periodo", selected = "Janeiro", c("Janeiro" = "2020-01-01",
                                                                                           "Fevereiro" = "2020-02-01",
                                                                                           "Marco" = "2020-03-01",
                                                                                           "Abril" = "2020-04-01",
                                                                                           "Maio" = "2020-05-01",
                                                                                           "Junho" = "2020-06-01",
                                                                                           "Julho" = "2020-07-01")),
      menuItem("ASCOM", tabName = 'ascom_tab', icon = icon("comments")),
      menuItem("Institucional", tabName = 'institucional_tab', icon = icon("building")),
      menuItem("Produtos", tabName = 'produtos_tab', icon = icon("bank")),
      menuItem("Analise de dados", icon = icon("globe")),
      menuItem("Audiovisual", icon = icon("camera")),
      menuItem("Arquitetura", icon = icon("building")),
      menuItem("Midias digitais", icon = icon("instagram")),
      menuItem("Projetos", icon = icon("map")),
      menuItem("Redator", icon = icon("pencil"))
    )
  ),
  dashboardBody(fluidPage(
    tabItems(
      tabItem(tabName = 'produtos_tab', h2("Resultados mensais do time de Podutos, Marketing - Central"),
              fluidRow( infoBoxOutput("producao_total_box"), infoBoxOutput("pecas_mes_box"), infoBoxOutput("pecas_aprovacao"), infoBoxOutput("alteracoes_box") ),
              box(plotlyOutput("proporcao_colaborador_plot")),
              box(plotlyOutput("pecas_mais_produzidas")),
              box(plotlyOutput("producao_mensal")),
              box(plotlyOutput("servicos_prestados"))
      )
      
      
    )
    
  )
  )
)