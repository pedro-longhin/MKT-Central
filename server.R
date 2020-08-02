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

produtos_data <- read_excel("PROJETOS_PRODUTOS.xlsm", sheet=1)
produtos_data$ENTREGA=as.Date(produtos_data$ENTREGA , origin = "1899-12-30")
#enc2utf8(produtos_data$COOPERATIVA)


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
  
  output$pecas_aprovacao = renderInfoBox({#VALUE BOX - AGUARDANDO APROVAÇÃO
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    aguardando=ref %>% filter(STATUS %in% c("AGUARDANDO APROVAÇÃO", "STAND-BY") ) %>% nrow()
    
    infoBox(value = aguardando, title = "Peças", subtitle = "Aguardando aprovação", fill = TRUE, color = "orange", icon = icon("comments"))
  })
  
  output$alteracoes_box = renderInfoBox({#VALUE BOX - ALTERACÕES
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    alteracoes = ref %>% filter(STATUS != "NOVO"); alteracoes=sum(na.omit(alteracoes$ALTERACOES))
    
    infoBox(value = alteracoes, title = "Refação", subtitle = "Quantidade de alterações", fill = TRUE, color = "red", icon = icon("user-times"))
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
      layout(barmode = 'group', xaxis = list(title = "Tipo de peca", range = c(-0.5,9.5)), yaxis = list(title="Quantidade produzida"))
  })
  
  ################################
  ################################
  
  output$producao_mensal = renderPlotly({
    
    ref=produtos_data %>% filter(ENTREGA>=as.Date(input$mesref) & ENTREGA<((as.Date(input$mesref) %m+% months(1))))
    
    demandas_mes=ref
    
    
    teste_pecas=data.frame(table(demandas_mes$TECNICO, demandas_mes$ENTREGA))
    names(teste_pecas)=c("Colaborador", "year", "n")
    teste_pecas$year=as.Date(teste_pecas$year)
    
    # Plot
    p <- teste_pecas %>% 
      ggplot( aes(x=year, y=n, fill=Colaborador, text=n)) +
      geom_area( )+
      #scale_fill_viridis(discrete = TRUE)+
      #theme(legend.position="none") +
      #theme_ipsum()+
      scale_x_date(date_labels = "%e %B")
    #theme(legend.position="none")
    
    # Turn it interactive
    p <- ggplotly(p, tooltip="text") %>% layout(xaxis = list(title="Dia do mes"), yaxis = list(title="Quantidade de pecas"))
    p
  })
  
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
  
  
}