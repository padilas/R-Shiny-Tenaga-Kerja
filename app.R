library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Load data from different sheets
SektorP <- read.csv("D:/Collage/2nd Term/MVD/R SHINY - PROJECT 1/Sektor Pekerjaan.csv")
Status_P <- read_csv("D:/Collage/2nd Term/MVD/R SHINY - PROJECT 1/Status P.csv")

# Check the structure of Sektor to ensure columns are correctly read
str(SektorP)
str(Status_P)

# Reshape Status_p data to long format
Status_p_long <- Status_P %>% 
  pivot_longer(
    cols = c(dibawahSD, SMP, SLTAUmum, SLTAKejuruan, AkademiDiploma, Universitas),
    names_to = "Pendidikan_Terakhir",
    values_to = "Jumlah"
  ) %>% 
  mutate(
    Jumlah = as.numeric(gsub(",", "", Jumlah))
  )

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Tenaga Kerja Indonesia", titleWidth =  300,
                  dropdownMenuOutput("msgOut")),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        "17 Sektor Lapangan Pekerjaan", tabName = "sektor"
      ),
      menuItem(
        "Status Pekerjaan Utama", tabName = "status"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sektor",
              fluidPage(
                valueBoxOutput("selectedSector", width = 8),
                valueBoxOutput("jumlahTotal", width = 4)
              ),
              fluidRow(
                box(title = "filter", status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE,
                    fluidRow(
                      column(4, radioButtons("Tahun_Sektor", "Pilih Tahun", choices = c("Total", "2020", "2021"), selected = "Total")
                      ),
                      column(4, selectInput("Sektor_Pekerjaan", "Pilih Sektor Pekerjaan",
                                            choices = unique(SektorP$Lapangan.Pekerjaan.Utama),
                                            selected = unique(SektorP$Lapangan.Pekerjaan.Utama)[1])
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Bar-plot Setiap Tingkat Pendidikan Pada Sektor Terkait", status = "primary", solidHeader = FALSE, width = 12, plotOutput("pendidikanPlot"))
              ),
              fluidRow(
                valueBoxOutput("jumlahSD", width = 2),
                valueBoxOutput("jumlahSMP", width = 2),
                valueBoxOutput("jumlahUmum", width = 2),
                valueBoxOutput("jumlahKejuruan", width = 2),
                valueBoxOutput("jumlahDiploma", width = 2),
                valueBoxOutput("jumlahUniversitas", width = 2)
              ),
              fluidRow(
                infoBoxOutput("differenceBox", width = 5),
                h4("*note:"),
                h5(">>>>> Pertumbuhan dengan nilai negatif ( - value), berarti terjadi penurunan."),
                h5(">>>>> Pertumbuhan dengan nilai positif (value), berarti terjadi peningkatan.")
              )
      ),
      tabItem(tabName = "status",
              fluidRow(
                valueBoxOutput("status_terpilih", width = 12),
                valueBoxOutput("total_status", width = 12)
              ),
              fluidRow(
                box(title = "filter", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                    selectInput("Status_Pekerjaan", "Select", choices = unique(Status_P$`Status Pekerjaan Utama`),
                                selected = unique(Status_P$`Status Pekerjaan Utama`)[1]),
                    radioButtons("Tahun_status", "Pilih Tahun", choices = c("Total", "2020", "2021"), selected = "Total")
                )
              ),
              fluidRow(
                box(title = "Histogram status pekerjaan", status = "primary", solidHeader = TRUE, width = 12, plotOutput("histogram_status")),
                uiOutput("keterangan_pendidikan")
              )
      )
    )
  )
)

server <- function(input, output) {
  showNotification("Click the message icon on the top right header to get to know about Team 04!", duration = NULL, type = "message")
  
  output$msgOut <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "TEAM 04",
                   message = "Sains Data - A"
                 ),
                 messageItem(
                   from = "Asyifa Nur Fadhilah",
                   message = "L0223004"
                 ),
                 messageItem(
                   from = "Ayatundira Setyoningrum",
                   message = "L0223005"
                 )
    )
  })
  filterData <- reactive({
    data <- SektorP %>%
      filter(Lapangan.Pekerjaan.Utama == input$Sektor_Pekerjaan)
    if(input$Tahun_Sektor != "Total"){
      data <- data %>% filter(Tahun == as.numeric(input$Tahun_Sektor))
    }
    data
  })
  
  output$jumlahTotal <- renderValueBox({
    value <- sum(as.numeric(filterData()$Total), na.rm = TRUE)
    valueBox(value, "Total Semua Tingkat Pendidikan dalam Sektor", icon = icon("graduation-cap"), color = "aqua")
  })
  
  output$jumlahSD <- renderValueBox({
    value <- sum(as.numeric(filterData()$dibawahSD), na.rm = TRUE)
    valueBox(value, "<= SD", icon = icon("book"), color = "green")
  })
  
  output$jumlahSMP <- renderValueBox({
    value <- sum(as.numeric(filterData()$SMP), na.rm = TRUE)
    valueBox(value, "SLTP/SMP", icon = icon("book"), color = "aqua")
  })
  
  output$jumlahUmum <- renderValueBox({
    value <- sum(as.numeric(filterData()$SLTAUmum), na.rm = TRUE)
    valueBox(value, "SLTA/Umum", icon = icon("book"), color = "olive")
  })
  
  output$jumlahKejuruan <- renderValueBox({
    value <- sum(as.numeric(filterData()$SLTAKejuruan), na.rm = TRUE)
    valueBox(value, "SLTA/Kejuruan", icon = icon("book"), color = "blue")
  })
  
  output$jumlahDiploma <- renderValueBox({
    value <- sum(as.numeric(filterData()$AkademiDiploma), na.rm = TRUE)
    valueBox(value, "Akademi/Diploma", icon = icon("book"), color = "light-blue")
  })
  
  output$jumlahUniversitas <- renderValueBox({
    value <- sum(as.numeric(filterData()$Universitas), na.rm = TRUE)
    valueBox(value, "Universitas", icon = icon("book"), color = "teal")
  })
  
  output$selectedSector <- renderValueBox({
    valueBox(input$Sektor_Pekerjaan, "Sektor Terpilih", icon = icon("building"), color = "blue")
  })
  
  output$pendidikanPlot <- renderPlot({
    data <- filterData()
    education_levels <- c("<= SD", "SMP", "SLTA/Umum", "SLTA/Kejuruan", "Akademi/Diploma", "Universitas")
    counts <- c(
      sum(as.numeric(data$dibawahSD), na.rm = TRUE),
      sum(as.numeric(data$SMP), na.rm = TRUE),
      sum(as.numeric(data$SLTAUmum), na.rm = TRUE),
      sum(as.numeric(data$SLTAKejuruan), na.rm = TRUE),
      sum(as.numeric(data$AkademiDiploma), na.rm = TRUE),
      sum(as.numeric(data$Universitas), na.rm = TRUE)
    )
    
    plot_data <- data.frame(education = factor(education_levels, levels = education_levels), count = counts)
    
    ggplot(plot_data, aes(x = education, y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = count), vjust = -0.5, color = "darkgrey", size = 3.5) +
      labs(x = "Tingkat Pendidikan", y = "Jumlah")
  })
  
  output$differenceBox <- renderInfoBox({
    data_2020 <- filter(SektorP, Tahun == 2020 & Lapangan.Pekerjaan.Utama == input$Sektor_Pekerjaan)
    data_2021 <- filter(SektorP, Tahun == 2021 & Lapangan.Pekerjaan.Utama == input$Sektor_Pekerjaan)
    
    diff_total <- as.numeric(data_2021$Total) - as.numeric(data_2020$Total)
    
    infoBox(
      "Pertumbuhan tahun 2020 ke 2021 pada sektor terkait:",
      diff_total,
      icon = icon("chart-line"),
      color = "olive"
    )
  })
  
  output$histogram_status <- renderPlot({
    data <- Status_p_long %>% 
      filter(`Status Pekerjaan Utama` == input$Status_Pekerjaan)
    
    if(input$Tahun_status != "Total") {
      data <- data %>% filter(Tahun == as.numeric(input$Tahun_status))
    }
    
    
    ggplot(data, aes(x = Pendidikan_Terakhir, y = Jumlah, fill = Pendidikan_Terakhir)) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = "",
           x = "Pendidikan Terakhir",
           y = "Jumlah") +
      theme(legend.position = "none")
  })
  
  output$keterangan_pendidikan <- renderUI({
    total_dibawahSD <- sum(Status_P$dibawahSD)
    total_SMP <- sum(Status_P$SMP)
    total_SLTAUmum <- sum(Status_P$SLTAUmum)
    total_SLTAKejuruan <- sum(Status_P$SLTAKejuruan)
    total_Akademi_Diploma <- sum(Status_P$AkademiDiploma)
    total_Universitas <- sum(Status_P$Universitas)
    
    fluidRow(
      box(title = "Total Tenaga Kerja Menurut Pendidikan Terakhir", status = "primary", solidHeader = TRUE, width = 12, 
          column(2, h3(total_dibawahSD, style = "color: white;"), h6("<=SD", style = "color: white;"), style = "background-color: #1F77B4; margin-right: 0px; margin-bottom: 3px"),
          column(2, h3(total_SMP, style = "color: white;"), h6("SMP", style = "color: white;"), style = "background-color: #2CA02C; margin-right: 0px; margin-bottom: 3px"),
          column(2, h3(total_SLTAUmum, style = "color: white;"), h6("SLTA / Umum", style = "color: white;"), style = "background-color: #0073E6; margin-right: 0px; margin-bottom: 3px"),
          column(2, h3(total_SLTAKejuruan, style = "color: white;"), h6("SLTA / Kejuruan", style = "color: white;"), style = "background-color: #28A745; margin-right: 0px;"),
          column(2, h3(total_Akademi_Diploma, style = "color: white;"), h6("Akademi / Diploma", style = "color: white;"), style = "background-color: #66B2FF; margin-right: 0px;"),
          column(2, h3(total_Universitas, style = "color: white;"), h6("Universitas", style = "color: white;"), style = "background-color: #17BECF;")
      )
    )
  })
  
  output$status_terpilih <- renderValueBox({
    valueBox(input$Status_Pekerjaan, "Status Terpilih", icon = icon("worker"), color = "aqua")
  })
  
  output$total_status <- renderValueBox({
    selected_status <- Status_p_long %>%
      filter(`Status Pekerjaan Utama` == input$Status_Pekerjaan)
    
    if(input$Tahun_status != "Total"){
      selected_status <- selected_status %>% filter(Tahun == as.numeric(input$Tahun_status))
    }
    
    total_value <- sum(selected_status$Jumlah, na.rm = TRUE)
    
    valueBox(
      total_value, "Total Tenaga Kerja untuk Status Terpilih", icon = icon("users"),
      color = "blue"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)