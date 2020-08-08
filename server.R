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

Sys.setlocale("LC_ALL", "portuguese") #identificação do idioma

server = function(input, output){
  
  output$producao_total_box = renderInfoBox({#VALUE BOX - PRODUÇÃO ANUAL
    
    pecaprodutosanual= produtos_data %>% filter(ENTREGA>=as.Date("2020-01-01")) %>% filter(STATUS != "NOVO") %>% filter(CATEGORIA!="Dúvidas/Envio") %>% nrow()
    
    infoBox(value = pecaprodutosanual, title = "Produção anual", subtitle = "peças estáticas", fill = TRUE, color = "green", icon = icon("check-circle"))
  })
  
  output$pecas_mes_box = renderInfoBox({#VALUE BOX - PRODUCAO MENSAL
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    pecaprodutosmensal = ref %>% filter(STATUS != "NOVO") %>% nrow()
    
    infoBox(value = pecaprodutosmensal, title = "Peças", subtitle = "produção no mês", fill = TRUE, color = "olive", icon = icon("thumbs-up"))
  })
  
  output$pecas_aprovacao = renderInfoBox({#VALUE BOX - AGUARDANDO APROVAÃÃO
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    aguardando=ref %>% filter(STATUS %in% c("AGUARDANDO APROVAÇÃO", "STAND-BY") ) %>% nrow()
    
    infoBox(value = aguardando, title = "Peças", subtitle = "Aguardando aprovação", fill = TRUE, color = "orange", icon = icon("comments"))
  })
  
  output$alteracoes_box = renderInfoBox({#VALUE BOX - ALTERAÇÕES
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    alteracoes = ref %>% filter(STATUS != "NOVO"); alteracoes=sum(na.omit(alteracoes$ALTERACOES))
    
    infoBox(value = alteracoes, title = "Refação", subtitle = "Quantidade de alterações", fill = TRUE, color = "red", icon = icon("user-times"))
  })
  
  output$temporal_alteracoes = renderPlotly({
    ref=produtos_data %>% filter(ENTREGA>=as.Date("2020-01-01"))
    ref2=institucional_data %>% filter(ENTREGA>=as.Date("2020-01-01"))
    alteracoes_mes=data.frame(ref$ENTREGA , ref$ALTERACOES) %>% drop_na()
    alteracoes_mes2=data.frame(ref2$ENTREGA , ref2$ALTERACOES) %>% drop_na()
    names(alteracoes_mes) = c('group', 'value')
    names(alteracoes_mes2) = c('group', 'value')
    alteracoes_mes=aggregate(alteracoes_mes$value ~month(alteracoes_mes$group) , data=alteracoes_mes, sum)
    alteracoes_mes2=aggregate(alteracoes_mes2$value ~month(alteracoes_mes2$group) , data=alteracoes_mes2, sum)
    data <- seq(as.Date(cut(Sys.Date(), "year")), (as.Date(cut(Sys.Date(), "month")) %m-% months(1)), "month")
    dadostemporal=data.frame(alteracoes_mes$`alteracoes_mes$value`, data)
    dadostemporal2=data.frame(alteracoes_mes2$`alteracoes_mes2$value`, data)
    names(dadostemporal)=c('Peca', 'Mes')
    names(dadostemporal2)=c('Peca2', 'Mes')
    dadostemporal=merge(dadostemporal, dadostemporal2, all=T)
    
    plot_ly(dadostemporal, x = ~Mes, y = ~Peca, name = 'Alterações Produtos', type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~Peca2, name = 'Alterações Institucional', mode = 'lines+markers')
    
  })
  
  
  
  output$proporcao_colaborador_plot = renderPlotly({
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    proporcao_colaborador=data.frame(table(ref$TECNICO))
    names(proporcao_colaborador)=c("Funcionario", "Quantidade")
    # Create Data
    data <- data.frame(
      group=proporcao_colaborador$Funcionario,
      value=proporcao_colaborador$Quantidade
    )
    
    
    plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$pecas_mais_produzidas = renderPlotly({
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    pecas_acumuladas=data.frame(table(ref$PECA))
    
    plot_ly(pecas_acumuladas, x = ~reorder(Var1, -Freq), y = ~Freq, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Tipo de peça", range = c(-0.5,9.5)), yaxis = list(title="Quantidade produzida"))
  })
  
  ################################
  ################################
  
  output$producao_mensal = renderPlotly({ })
  
  output$servicos_prestados = renderPlotly({
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    servicosprestados=data.frame(table(ref$CATEGORIA))
    names(servicosprestados)=c("Funcionario", "Quantidade")
    # Create Data
    data <- data.frame(
      group=servicosprestados$Funcionario,
      value=servicosprestados$Quantidade
    )
    
    
    plot_ly(data, labels = ~group, values = ~value) %>%
      add_pie(hole = 0.6) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$pedidos_coop = renderPlotly({
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    pecas_acumuladas=data.frame(table(ref$COOPERATIVA))
    
    plot_ly(pecas_acumuladas, x = ~reorder(Var1, -Freq), y = ~Freq, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Cooperativa", range = c(-0.5,9.5)), yaxis = list(title="Serviços prestados"))
  })
  
  ##################################  INSTITUCIONAL
  ##################################  INSTITUCIONAL
  
  
  output$temporal_alteracoes_institucional = renderPlotly({
    ref=produtos_data %>% filter(ENTREGA>=as.Date("2020-01-01"))
    ref2=institucional_data %>% filter(ENTREGA>=as.Date("2020-01-01"))
    alteracoes_mes=data.frame(ref$ENTREGA , ref$ALTERACOES) %>% drop_na()
    alteracoes_mes2=data.frame(ref2$ENTREGA , ref2$ALTERACOES) %>% drop_na()
    names(alteracoes_mes) = c('group', 'value')
    names(alteracoes_mes2) = c('group', 'value')
    alteracoes_mes=aggregate(alteracoes_mes$value ~month(alteracoes_mes$group) , data=alteracoes_mes, sum)
    alteracoes_mes2=aggregate(alteracoes_mes2$value ~month(alteracoes_mes2$group) , data=alteracoes_mes2, sum)
    data <- seq(as.Date(cut(Sys.Date(), "year")), (as.Date(cut(Sys.Date(), "month")) %m-% months(1)), "month")
    dadostemporal=data.frame(alteracoes_mes$`alteracoes_mes$value`, data)
    dadostemporal2=data.frame(alteracoes_mes2$`alteracoes_mes2$value`, data)
    names(dadostemporal)=c('Peca', 'Mes')
    names(dadostemporal2)=c('Peca2', 'Mes')
    dadostemporal=merge(dadostemporal, dadostemporal2, all=T)
    
    plot_ly(dadostemporal, x = ~Mes, y = ~Peca, name = 'Alterações Produtos', type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~Peca2, name = 'Alterações Institucional', mode = 'lines+markers')
  })
  
  
  output$producao_total_institucional_box = renderInfoBox({#VALUE BOX - PRODUÇÃO ANUAL
    
    pecainstitucionalanual= institucional_data %>% filter(ENTREGA>=as.Date("2020-01-01")) %>% filter(STATUS != "NOVO") %>% filter(CATEGORIA!="Dúvidas/Envio") %>% nrow()
    
    infoBox(value = pecainstitucionalanual, title = "Produção anual", subtitle = "peças estáticas", fill = TRUE, color = "green", icon = icon("check-circle"))
  })
  
  output$pecas_mes_institucional_box = renderInfoBox({#VALUE BOX - PRODUCAO MENSAL
    
    ref=institucional_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    pecainstitucionalmensal = ref %>% filter(STATUS != "NOVO") %>% nrow()
    
    infoBox(value = pecainstitucionalmensal, title = "Peças", subtitle = "produção no mês", fill = TRUE, color = "olive", icon = icon("thumbs-up"))
  })
  
  output$pecas_institucional_aprovacao = renderInfoBox({#VALUE BOX - AGUARDANDO APROVAÇÃO
    
    ref=institucional_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    aguardando=ref %>% filter(STATUS %in% c("AGUARDANDO APROVAÇÃO", "STAND-BY") ) %>% nrow()
    
    infoBox(value = aguardando, title = "Peças", subtitle = "Aguardando aprovação", fill = TRUE, color = "orange", icon = icon("comments"))
  })
  
  output$alteracoes_institucional_box = renderInfoBox({#VALUE BOX - ALTERAÇÕES
    
    ref=institucional_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    alteracoes = ref %>% filter(STATUS != "NOVO"); alteracoes=sum(na.omit(alteracoes$ALTERACOES))
    
    infoBox(value = alteracoes, title = "Refação", subtitle = "Quantidade de alterações", fill = TRUE, color = "red", icon = icon("user-times"))
  })
  
  ############# PLOTS
  ############# PLOTS
  
  output$proporcao_colaborador_institucional_plot = renderPlotly({
    ref=institucional_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    proporcao_colaborador=data.frame(table(ref$TECNICO))
    names(proporcao_colaborador)=c("Funcionario", "Quantidade")
    # Create Data
    data <- data.frame(
      group=proporcao_colaborador$Funcionario,
      value=proporcao_colaborador$Quantidade
    )
    
    
    plot_ly(data, labels = ~group, values = ~value, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$pecas_mais_produzidas_institucional = renderPlotly({
    
    ref=institucional_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    pecas_acumuladas=data.frame(table(ref$PECA))
    
    plot_ly(pecas_acumuladas, x = ~reorder(Var1, -Freq), y = ~Freq, type = 'bar', name = 'SF Zoo') %>%
      layout(barmode = 'group', xaxis = list(title = "Tipo de peça", range = c(-0.5,9.5)), yaxis = list(title="Quantidade produzida"))
  })
  
  output$producao_mensal_institucional = renderPlotly({ })
  
  
}