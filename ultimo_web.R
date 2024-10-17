library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)

# Leer el archivo CSV
data <- read.csv("C:/Users/ASUS/Desktop/parcialsw2/sw2_r/supermarket_sales_realistic_cleaned.csv")

# Convertir la columna Fecha a un formato de fecha
data$Fecha <- as.Date(data$Fecha, format="%Y-%m-%d")

# Extraer el mes y año de la columna de fecha
data$Mes <- format(data$Fecha, "%Y-%m")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Ventas Supermercado"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ventas Mensuales", tabName = "ventas_mensuales", icon = icon("chart-line")),
      menuItem("Ventas por Categoría", tabName = "ventas_categoria", icon = icon("chart-pie")),
      menuItem("Ventas por Sucursal", tabName = "ventas_sucursal", icon = icon("store")),
      menuItem("Ventas por Tipo de Pago", tabName = "ventas_tipo_pago", icon = icon("credit-card")),
      menuItem("Ventas por Hora", tabName = "ventas_hora", icon = icon("clock"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Filtros",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        selectInput("sucursal", "Sucursal:", choices = c("Todas", unique(data$Sucursal))),
        selectInput("categoria", "Categoría:", choices = c("Todas", unique(data$Categoria))),
        selectInput("mes", "Mes:", choices = c("Todos", unique(data$Mes)))
      )
    ),
    tabItems(
      tabItem(tabName = "ventas_mensuales",
              fluidRow(
                box(title = "Ventas Mensuales", status = "primary", solidHeader = TRUE, 
                    plotOutput("plot_ventas_mensuales"), width = 12)
              )),
      tabItem(tabName = "ventas_categoria",
              fluidRow(
                box(title = "Ventas por Categoría", status = "primary", solidHeader = TRUE, 
                    plotOutput("plot_ventas_categoria"), width = 12)
              )),
      tabItem(tabName = "ventas_sucursal",
              fluidRow(
                box(title = "Ventas por Sucursal", status = "primary", solidHeader = TRUE, 
                    plotOutput("plot_ventas_sucursal"), width = 12)
              )),
      tabItem(tabName = "ventas_tipo_pago",
              fluidRow(
                box(title = "Ventas por Tipo de Pago", status = "primary", solidHeader = TRUE, 
                    plotOutput("plot_ventas_tipo_pago"), width = 12)
              )),
      tabItem(tabName = "ventas_hora",
              fluidRow(
                box(title = "Ventas por Hora", status = "primary", solidHeader = TRUE, 
                    plotOutput("plot_ventas_hora"), width = 12)
              ))
    )
  )
)

# Server: Lógica de la aplicación
server <- function(input, output) {
  
  # Filtrar datos según las selecciones del usuario
  filtered_data <- reactive({
    data_filtered <- data
    if (input$sucursal != "Todas") {
      data_filtered <- data_filtered %>% filter(Sucursal == input$sucursal)
    }
    if (input$categoria != "Todas") {
      data_filtered <- data_filtered %>% filter(Categoria == input$categoria)
    }
    if (input$mes != "Todos") {
      data_filtered <- data_filtered %>% filter(Mes == input$mes)
    }
    data_filtered
  })
  
  # Ventas Mensuales
  output$plot_ventas_mensuales <- renderPlot({
    ventas_mensuales <- filtered_data() %>%
      group_by(Mes) %>%
      summarise(TotalVentas = sum(Total, na.rm = TRUE))
    
    ggplot(ventas_mensuales, aes(x = Mes, y = TotalVentas)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Ventas Mensuales", x = "Mes", y = "Total Ventas") +
      theme_minimal()
  })
  
  # Ventas por Categoría
  output$plot_ventas_categoria <- renderPlot({
    ventas_por_categoria <- filtered_data() %>%
      group_by(Categoria) %>%
      summarise(TotalVentas = sum(Total, na.rm = TRUE)) %>%
      arrange(desc(TotalVentas))
    
    ggplot(ventas_por_categoria, aes(x = reorder(Categoria, -TotalVentas), y = TotalVentas)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Ventas por Categoría", x = "Categoría", y = "Total Ventas") +
      theme_minimal() +
      coord_flip()
  })
  
  # Ventas por Sucursal
  output$plot_ventas_sucursal <- renderPlot({
    ventas_por_sucursal <- filtered_data() %>%
      group_by(Sucursal) %>%
      summarise(TotalVentas = sum(Total, na.rm = TRUE)) %>%
      arrange(desc(TotalVentas))
    
    ventas_por_sucursal <- ventas_por_sucursal %>%
      mutate(Percentage = TotalVentas / sum(TotalVentas) * 100)
    
    ggplot(ventas_por_sucursal, aes(x = "", y = TotalVentas, fill = Sucursal)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Ventas por Sucursal", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
      geom_text(aes(label = paste0(Sucursal, ": ", round(Percentage, 1), "%\n", "Total: ", TotalVentas)), 
                position = position_stack(vjust = 0.5))
  })
  
  # Ventas por Tipo de Pago
  output$plot_ventas_tipo_pago <- renderPlot({
    ventas_por_tipo_pago <- filtered_data() %>%
      group_by(Pago) %>%
      summarise(TotalVentas = sum(Total, na.rm = TRUE)) %>%
      arrange(desc(TotalVentas))
    
    ggplot(ventas_por_tipo_pago, aes(x = reorder(Pago, -TotalVentas), y = TotalVentas)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      labs(title = "Ventas por Tipo de Pago", x = "Tipo de Pago", y = "Total Ventas") +
      theme_minimal() +
      coord_flip()
  })
  
  # Ventas por Hora
  output$plot_ventas_hora <- renderPlot({
    # Convertir la columna 'Hora' a un formato adecuado
    data$Hora <- as.POSIXct(data$Hora, format = "%H:%M:%S")
    data <- data[!is.na(data$Hora), ]
    
    # Extraer solo la hora para agrupar
    data$HoraSimple <- format(data$Hora, "%H")
    
    ventas_por_hora <- data %>%
      group_by(HoraSimple) %>%
      summarise(TotalVentas = sum(Total, na.rm = TRUE), NumeroClientes = n()) %>%
      arrange(desc(NumeroClientes))
    
    ggplot(ventas_por_hora, aes(x = HoraSimple, y = TotalVentas)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title = "Ventas por Hora", x = "Hora del Día", y = "Total Ventas") +
      theme_minimal()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)