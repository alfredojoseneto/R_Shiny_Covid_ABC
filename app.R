# Libraries ---------------------------------------------------------------
if(!require(pacman)) {install.packages("pacman")}
pacman::p_load("pacman", "tidyverse", "rstudioapi", "shiny", "janitor",
               "directlabels", "httr", "shinythemes")

# Setting working directory -----------------------------------------------
dirname(rstudioapi::getActiveDocumentContext()$path) %>% setwd()

# Importar o data set -----------------------------------------------------

# Importando o data set do site do Brasil IO e gravando em disco
httr::GET(url = "https://data.brasil.io/dataset/covid19/caso_full.csv.gz",
          write_disk(path = "./input/caso_full.csv.gz", overwrite = TRUE),
          progress()
)

# Lendo o arquivo importado do Brasil IO para um data frame
df_covid <- read.csv(file = "./input/caso_full.csv.gz", sep = ",", dec = ",",
                     encoding = "UTF-8", stringsAsFactors = FALSE)

df_covid$date <- as.Date(df_covid$date)

# Data wrangling ----------------------------------------------------------

# Converting empty string in city variable to NA
df_covid$city <- ifelse(test = df_covid$city == "", yes = NA, no = df_covid$city)

# Removendo os NAs da cidades
df_covid_clean <- df_covid %>% 
  filter(!is.na(city))


# Criando o filtro com as cidades de interesse
cidades_filtro <- c("São Caetano do Sul",
                    "Santo André",
                    "Mauá",
                    "Diadema",
                    "São Bernardo do Campo",
                    "Rio Grande da Serra",
                    "Ribeirão Pires")

# Criando o data frame com as 7 (sete) cidades do ABC
df_covid_abc <- df_covid_clean %>% 
  filter(city %in% cidades_filtro)

# Criando o data frame com a frequência acumulada de novos casos desde o dia do primeiro caso
df_covid_shiny <- df_covid_abc %>% 
  group_by(city) %>%
  summarise(city,
            date,
            new_confirmed,
            new_deaths) %>% 
  mutate(cum_sum_daily_cases = cumsum(new_confirmed),
         cum_sum_daily_deaths = cumsum(new_deaths))

# Shiny App ---------------------------------------------------------------


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme(theme = "journal"),
  navbarPage(title = "Shiny COVID ABC",
             tabPanel(title = "Gráfico",
                      titlePanel(tags$strong("Evolução do COVID nas 7 Cidades do ABC")),
                      tags$hr(),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput(inputId = "date_range",
                                         label = "Selecione o intervalo de datas",
                                         format = "dd/mm/yyyy",
                                         separator = "-",
                                         min = min(df_covid_shiny$date),
                                         max = max(df_covid_shiny$date),
                                         start = min(df_covid_shiny$date),
                                         end = max(df_covid_shiny$date)
                          ), #end of dateRangeInput
                          
                          checkboxGroupInput(inputId = "cities_abc",
                                             label = "Selecione a(s) Cidade(s)",
                                             choices = c("São Caetano do Sul",
                                                         "Santo André",
                                                         "Mauá",
                                                         "Diadema",
                                                         "São Bernardo do Campo",
                                                         "Rio Grande da Serra",
                                                         "Ribeirão Pires"),
                                             selected = "São Caetano do Sul"
                          ) #end of checkboxGroupInput
                        ), #end of sidebarPanel
                        
                        mainPanel(plotOutput("covid_linear"),
                                  tags$hr(),
                                  plotOutput("covid_linear_death")) #end of mainPanel
                      ), #end of sidebarLayout),
                      navbarMenu("More",
                                 tabPanel("Summary"),
                                 "----",
                                 "Section header",
                                 tabPanel("Table")
                      )
             ), # end of tabPanel gráfico
             tabPanel(title = "Sobre",
                      tags$div(
                        "Este aplicativo foi planejado e construído com uma lúdica pretensão. Quando, em meio à pandemia do CORONA VÍRUS, no mês de junho de 2020, 3 desconhecidos se encontram virtualmente. A Estatística, o Farmacêutico e o Analista de Dados, nessa ordem,Regina Albanese Pose - Docente da Universidade São Caetano do Sul - Conselheira no Conselho Regional de Estatística - CONRE 3, Membro da comunidade R-Ladies SP, interessada em análise de dados com softwares livres e de códigos abertos",
                        tags$a("http://lattes.cnpq.br/1832375183593136"),
                        "; Alfredo Rodrigues Neto - Farmacêutico graduado pela Universidade Federal da Bahia - Pós-Graduado em Farmácia Clínica pela USP, interessado em análise de dados",
                        tags$a(href="http://lattes.cnpq.br/0808877552696289"),
                        "; Olímpio Ribeiro da Fonseca Neto - Analista de Sistemas graduado pela FATEC-SP-Ipiranga, Técnico Sênior de laboratório de baixas temperaturas e altos campos magnéticos IF/USP, interessado em análise de sistemas",
                        tags$a("http://lattes.cnpq.br/3125618480334226"),
                        "."),
                      tags$br(),
                      tags$div(
                        "Três aspirantes à Ciência de Dados, três pessoas impactadas pelo momento vivenciado, três apaixonados pela Ciência e pela vida. A Estatística lança a ideia em um grupo de medicina baseada em evidências, o farmacêutico, aceita o desafio, e o analista de sistemas
resolve interagir com o grupo. E assim, construímos este aplicativo, simples, descritivo, com dados do momento real. O aplicativo mora na casa do farmacêutico,",
                        tags$a("https://alfredojoseneto.shinyapps.io/R_Shiny_Covid_ABC/"),
                        "e pode ser consultado todos os dias, de forma atualizada, pelo site",
                        tags$a("https://brasil.io/home/"),
                        ", que foi o escolhido por nossa equipe, para “alimentar” o sistema, por ser também desenvolvido em software aberto e livre, e promover o fornecimento dos dados com atualizações diárias direto das Secretarias de Saúde Municipais."
                      ),
                      tags$br(),
                      tags$div("As possibilidades são bem simples, e a meta, proposta pela Estatística e Professora da USCS, Regina, foi recortar o cenário das 7 cidades do ABC, quais, sejam Santo André, São Bernardo do Campo, São Caetano do Sul, Mauá, Ribeirão Pires, Rio Grande da Serra, cenário de estudos do Observatório de Políticas Públicas, Empreendedorismo e Conjuntura da USCS, formado por professores, alunos e parceiros convidados, tendo como objetivo elaborar e publicar, periodicamente, notas técnicas no campo das Políticas Públicas, Empreendedorismo e Conjuntura (CONJUSCS). Sendo que este aplicativo, está inserido na 13a. carta de conjuntura do CONJUSCS, e pode ser acessada em ",
                               tags$a(href="https://www.uscs.edu.br/noticias/cartasconjuscs"),
                               "As possíveis telas até agora, setembro de 2020, deste aplicativo são, a tela sem qualquer seleção de cidades, com a seleção de apenas uma delas,
ou, a combinação de duas ou mais cidades para que se faça um comparativo entre as curvas acumuladas de casos e mortes por COVID-19."
                      )
             ) #end of tabPanel sobre
  ), #end of navbarPage
) #end of fluidPage


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  output$covid_linear <- renderPlot({
    
    # Limits of days of the scale_x_date()
    first_day = min(df_covid_shiny$date)
    last_day = max(df_covid_shiny$date)
    
    # Setting colours of the cities
    main_color = "#5d615e"
    
    # Position of the annotation about quarentine
    x_quarentine_pos <- as.Date("2020-03-25")
    y_quarentine_pos <- max(df_covid_shiny$cum_sum_daily_cases)
    
    # Criando o gráfico dos casos em função das datas
    plot_shiny <- df_covid_shiny %>%
      filter(date >= paste(input$date_range[1]),
             date <= paste(input$date_range[2]),
             city %in% input$cities_abc) %>% 
      ggplot(data = .,
             mapping = aes(x = date,
                           y = cum_sum_daily_cases,
                           colour = city)) +
      geom_line(size = 1) +
      geom_vline(xintercept = as.Date("2020-03-25"),
                 linetype = "dashed",
                 colour = "#2769db") +
      scale_color_discrete(guide = "none") +
      scale_x_date(expand = expansion(mult = c(0.01, 0.25)),
                   limits = c(input$date_range[1], input$date_range[2]),
                   breaks = seq(from = first_day, to = last_day, by = 3),
                   date_labels = "%d-%B") +
      directlabels::geom_dl(mapping = aes(label = city),
                            method = list(dl.trans(x = x + 0.2), "last.points")) +
      annotate(geom = "text",
               x = x_quarentine_pos,
               y = y_quarentine_pos,
               colour = "#2769db",
               hjust = -0.01,
               label = "Início da quarentena no Estado de São Paulo (25/03/2020)") +
      labs(
        x = "Data de novos casos",
        y = "Número de casos acumulados por dia (linear)",
        title = "Evolução do número de casos COVID acumulados por dia nas 7 cidades do ABC",
        caption = "Dados públicos e abertos providos pelo Brasil.IO") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 20, colour = main_color),
        plot.subtitle = element_text(colour = main_color),
        plot.caption = element_text(colour = main_color),
        axis.title.x = element_text(size = 14, colour = main_color),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, colour = main_color),
        axis.title.y = element_text(size = 14, colour = main_color),
        axis.text.y = element_text(size = 12, colour = main_color)
      )
    
    print(plot_shiny)
    
  }) #end of renderPlot --> output$linear_covid

  output$covid_linear_death <- renderPlot({
    
    
    # Limits of days of the scale_x_date()
    first_day = min(df_covid_shiny$date)
    last_day = max(df_covid_shiny$date)
    
    # Setting colours of the cities
    main_color = "#5d615e"
    
    # Position of the annotation about quarentine
    x_quarentine_pos <- as.Date("2020-03-25")
    y_quarentine_pos <- max(df_covid_shiny$cum_sum_daily_cases)
    
    
    plot_shiny_deaths <- df_covid_shiny %>%
      filter(date >= paste(input$date_range[1]),
             date <= paste(input$date_range[2]),
             city %in% input$cities_abc) %>% 
      ggplot(data = .,
             mapping = aes(x = date, y = cum_sum_daily_deaths, colour = city)) +
      geom_line(size = 1) +
      scale_x_date(expand = expansion(mult = c(0.01, 0.25)),
                   breaks = seq(from = first_day, to = last_day, by = 3),
                   limits = c(input$date_range[1], input$date_range[2]),
                   date_labels = "%d-%B") +
      scale_color_discrete(guide = "none") +
      directlabels::geom_dl(mapping = aes(label = city),
                            method = list(dl.trans(x = x + 0.2), "last.points")) +
      labs(title = "Evolução do número de óbitos por COVID acumulados por dia nas 7 cidades do ABC",
           x = "Data dos novos óbitos",
           y = "Número de óbitos acumulados por dia (linear)",
           caption = "Dados públicos e abertos providos pelo Brasil.IO") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 20, colour = main_color),
        plot.subtitle = element_text(colour = main_color),
        plot.caption = element_text(colour = main_color),
        axis.title.x = element_text(size = 14, colour = main_color),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, colour = main_color),
        axis.title.y = element_text(size = 14, colour = main_color),
        axis.text.y = element_text(size = 12, colour = main_color)
      )
    print(plot_shiny_deaths)
  }) #end of renderPlot --> output$covid_linear_death
  
} #end of server function


shinyApp(ui = ui, server = server)
