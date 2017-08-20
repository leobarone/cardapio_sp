library(shiny)

source("cardapio.R")

# Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Cardápio das Escolas da Prefeitura São Paulo com Gestão Terceirizada"),
  hr(),  
  helpText("Autor: Leonardo Sangali Barone (leobarone@gmail.com), pai"),
  hr(),  
  helpText("NOTA: Este aplicativo foi desenvolvido para facilitar a consulta das famílias
           ao cardápio semanal das escolas da rede municipal de São Paulo cuja alimentação 
            é fornecida por empresa terceirizada.
           A informação disponibilizada é a mesma já disponibilizada pela Prefeitura de São Paulo no
          portal da Secretaria Municipal de Educação, porém reorganizada. O autor desta ferramenta
          não se responsabiliza pela validade das informações."),
  hr(),
  helpText("AVISO: As empresas 'EMPRESA SHA COMÉRCIO DE ALIMENTOS (LOTE JT)' e 
           'EMPRESA PRM' ainda não foram incluídas na ferramenta Serão incluídas em breve."),
  hr(),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("empresa", "Empresa Terceirizada:", 
                  choices = vetor_empresas[order(vetor_empresas)]),
      hr(),
      selectInput("etapa", "Etapa de Ensino:", 
                  choices = vetor_etapa,
                  selected = "CEI"),
      hr(),
      selectInput("idade", "Idade da Criança:", 
                  choices = vetor_idade,
                  selected = "2 a 6 anos"),
      hr(),
      selectInput("semana_mes", "Semana do Mês:", 
                  choices = unique(dados$semana),
                  selected = "21/8 a 25/8"),
      hr(),
      selectInput("dia_semana", "Dia da Semana:", 
            choices = str_replace(colnames(dados[1:5]), "_", "-"))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      tableOutput("tableOutput"),
      hr(),
      imageOutput("preImage", inline = T),
      helpText("Registro meu protesto contra a mudança nas creches de janta 
               para 'Refeição da Tarde' e com sopa todos os dias.")
    )
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tableOutput <- renderTable({ 
    
    cod_etapa <- vetor_codigo_etapa[vetor_etapa == input$etapa]
    
    dados <- dados[dados$idade == input$idade &
            dados$semana == input$semana_mes &
              dados$cod_etapa == cod_etapa, 
          c("Refeição", str_replace(input$dia_semana, "-", "_"))]
    
    names(dados)[2] <- "Cardápio"
    
    dados
  })
  
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "sopa.png"
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

