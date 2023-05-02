library(shiny)
library(shinydashboard)
library(plotly)
library(DT)




ui <- dashboardPage(skin = "green",
        dashboardHeader(title = div(img(src = "logo.png", height = "40px"), "Dashboard Transplantes 2019"),titleWidth = 440),
        dashboardSidebar(
        sidebarMenu(
                    selectInput("TIPO_TRANSPLANTE", label = h3("Tipo de Transplante"), 
                        choices = list("TMO Alogenico","TMO Autologo","CARDIACO","CONJUNTIVA","CORNEA","HEPATICO","PULMONAR","RENAL","TODOS"), 
                        selected = "TODOS"),
                    menuItem("Geral", tabName = "geral", icon = icon("fa")),
                    menuItem("Dados Transplantes", tabName = "transplantes", icon = icon("fa")),
                    menuItem("Internações", tabName = "internacoes", icon = icon("fa"),
                             menuSubItem("Dados Internacoes",tabName = "dados_int"),
                             menuSubItem("Leitos-dia",
                                         tabName = "internacao_leito_dia")),
                    menuItem("Procedimentos", tabName = "procedimentos", icon = icon("table"),
                             menuSubItem("Pré Transplante",
                                         tabName = "proc_pre_tx"),
                             menuSubItem("Transplante",
                                         tabName = "proc_transp"),
                             menuSubItem("Pós Transplante Precoce",
                                         tabName = "proc_pos_tx_precoce"),
                             menuSubItem("Pós Transplante Tardio",
                                         tabName = "proc_pos_tx_tardio")),
                    menuItem("Exames", tabName = "exames", icon = icon("table"),
                             menuSubItem("Pré Transplante",
                                         tabName = "exames_pre_tx"),
                             menuSubItem("Transplante",
                                         tabName = "exames_transp"),
                             menuSubItem("Pós Transplante Precoce",
                                         tabName = "exames_pos_tx_precoce"),
                             menuSubItem("Pós Transplante Tardio",
                                         tabName = "exames_pos_tx_tardio"),
                             menuSubItem("Protocolos Adesão",
                                         tabName = "adesao")),
                    menuItem("Consultorias", tabName = "consultoria", icon = icon("table"),
                             menuSubItem("Pré Transplante",
                                         tabName = "consultoria_pre_tx"),
                             menuSubItem("Transplante",
                                         tabName = "consultoria_tx"),
                             menuSubItem("Pós Transplante Precoce",
                                         tabName = "consultoria_pos_tx_precoce"),
                             menuSubItem("Pós Transplante Tardio",
                                         tabName = "consultoria_pos_tx_tardio"))
                    
        )
    ),
     dashboardBody(
         tabItems(
           tabItem(tabName = "geral",h2("Itinerário dos Pacientes Transplantados"),
                                    # verbatimTextOutput("output1"),
                                     fluidRow(column(12,img(src = "img_paths.jpg", height = "500px", width = "1000px"))),
                                     tags$hr(),
                                     h2("Linha temporal dos transplantes"),
                                     fluidRow(column(12,plotlyOutput("plot_cc")))),  
           tabItem(tabName = "transplantes",
                     fluidRow(
                              column(12, tableOutput("table1"))
                     ),
                    tags$hr(),
                     fluidRow(
                         column(6, plotlyOutput("plot1")),
                         column(6, plotlyOutput("plot2")),
                         column(6, plotlyOutput("plot3")),
                         column(6, plotlyOutput("plot4"))
                         
                     )
            ),
            tabItem(tabName = "dados_int",
                    fluidRow(column(6, plotlyOutput("plot5")),
                             column(6, plotlyOutput("plot6")),
                             column(6, plotlyOutput("plot7")),
                             column(6, plotlyOutput("plot8")))
            ),
            tabItem(tabName = "proc_pre_tx",
                    fluidRow(column(12, plotOutput("wordcloud_pretx"))),
                    tags$hr(),
                    fluidRow(column(12,tableOutput("table3")))),
            tabItem(tabName = "proc_transp",
                      fluidRow(column(12, plotOutput("wordcloud_tx"))),
                      tags$hr(),
                      fluidRow(column(12, tableOutput("table4")))
                    ),
            tabItem(tabName = "proc_pos_tx_precoce",
                    fluidRow(column(12, plotOutput("wordcloud_postx_prec"))),
                    tags$hr(),
                    fluidRow(column(12, tableOutput("table5")))),
            tabItem(tabName = "proc_pos_tx_tardio",
                    fluidRow(column(12, plotOutput("wordcloud_postx_tardio"))),
                    tags$hr(),
                    fluidRow(column(12, tableOutput("table6")))
                    ),
            tabItem(tabName = "consultoria_pre_tx",
                    fluidRow(column(12, plotOutput("wordcloud_pretx_cons"))),
                    tags$hr(),
                    fluidRow(column(12, DTOutput("count_pretx_cons")))
                    ),
            tabItem(tabName = "consultoria_tx",
                    fluidRow(column(12, plotOutput("wordcloud_tx_cons"))),
                    tags$hr(),
                    fluidRow(column(12, DTOutput("count_tx_cons")))),
            tabItem(tabName = "consultoria_pos_tx_precoce",
                    fluidRow(column(12, plotOutput("wordcloud_postx_prec_cons"))),
                    tags$hr(),
                    fluidRow(column(12, DTOutput("count_postxprec_cons")))),
            tabItem(tabName = "consultoria_pos_tx_tardio",
                    fluidRow(column(12, plotOutput("wordcloud_postx_tardio_cons"))),
                    tags$hr(),
                    fluidRow(column(12, DTOutput("count_postxtard_cons")))
                    ),
           tabItem(tabName = "exames_pre_tx",
                   fluidRow(column(12,DTOutput("count_pretx_exame"))),
                   tags$hr(),
                    fluidRow(column(6, plotlyOutput("plot9")),
                             column(6, plotlyOutput("plot10"))),
                   fluidRow(column(6, plotlyOutput("plot11")),
                            column(6, plotlyOutput("plot12"))),
                                      tags$hr(),
                   fluidRow(column(6,uiOutput("column_select1")),
                            column(6,uiOutput("column_select2"))),
                   fluidRow(column(6,plotlyOutput("diy1_plot")),
                            column(6,plotlyOutput("diy2_plot")))
                   
                  ),
           tabItem(tabName = "exames_transp",
                   fluidRow(column(12,DTOutput("count_tx_exame"))),
                   tags$hr(),
                   fluidRow(column(6, plotlyOutput("plot13")),
                            column(6, plotlyOutput("plot14"))),
                   fluidRow(column(6, plotlyOutput("plot15")),
                            column(6, plotlyOutput("plot16"))),
                   tags$hr(),
                   fluidRow(column(6,uiOutput("column_select3")),
                            column(6,uiOutput("column_select4"))),
                    fluidRow(column(6,plotlyOutput("diy3_plot")),
                            column(6,plotlyOutput("diy4_plot")))
                  ),
           tabItem(tabName = "exames_pos_tx_precoce",
                   fluidRow(column(12,DTOutput("count_postxprec_exame"))),
                   tags$hr(),
                   fluidRow(column(6, plotlyOutput("plot17")),
                            column(6, plotlyOutput("plot18"))),
                   fluidRow(column(6, plotlyOutput("plot19")),
                            column(6, plotlyOutput("plot20"))),
                   tags$hr(),
                   fluidRow(column(6,uiOutput("column_select5")),
                            column(6,uiOutput("column_select6"))),
                   fluidRow(column(6,plotlyOutput("diy5_plot")),
                            column(6,plotlyOutput("diy6_plot")))

           ),
           tabItem(tabName = "exames_pos_tx_tardio",
                   fluidRow(column(12,DTOutput("count_postxtard_exame"))),
                   tags$hr(),
                   fluidRow(column(6, plotlyOutput("plot21")),
                            column(6, plotlyOutput("plot22"))),
                   fluidRow(column(6, plotlyOutput("plot23")),
                            column(6, plotlyOutput("plot24"))),
                   tags$hr(),
                   fluidRow(column(6,uiOutput("column_select7")),
                            column(6,uiOutput("column_select8"))),
                   fluidRow(column(6,plotlyOutput("diy7_plot")),
                            column(6,plotlyOutput("diy8_plot")))
                   ),
          tabItem(tabName = "adesao",
                  fluidRow(column(12,DTOutput("adesao_tacro"))),
                  tags$hr(),
                  fluidRow(column(12,tableOutput("sum_adesao_tacro"))),
                  tags$hr(),
                  fluidRow(column(12,plotlyOutput("plot_adesao")))
                  )
        )
    )
)