library(shiny)
library(tidyverse)
options(digits=10)
theme_set(theme_classic())

writeGPX <- function(lat, lon, name, file) {
  o <- c('<?xml version="1.0"?>
<gpx version="1.1" creator="R" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.topografix.com/GPX/1/1" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">')
  if (missing(name))
    o <- c(o, paste('<wpt lat="',lat,'" lon="',lon,'" />', sep=''))
  else
    o <- c(o, paste('<wpt lat="',lat,'" lon="',lon,'"><name>',name,'</name></wpt>', sep=''))
  o <- c(o, '</gpx>')
  if (is.character(file) || inherits(file, "connection"))
    cat(o, file=file, sep='\n')
}

ui <- fluidPage(
  
  # Application title
  titlePanel("Sonarwiz Wrangler"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("This application receives a raw sonarwiz lines output as a csv file. 
        Place the main line over the target, and as many lines as you want either side. Leave their direction as default."),
      fileInput("csv", 'Sonarwiz file:', multiple = FALSE, accept = NULL, width = NULL),
      checkboxInput("shuffle", "Shuffle lines", value = FALSE),
      checkboxInput("sequence", "Sequential naming", value = FALSE),
      textInput("prefix", "Add prefix to point names (optional)", value = ""),
      p("If the lines are named with a number, this can interfere with the parsing. Check the file and add the problem name below."),
      textInput("stray", "Line name to remove (optional)", value = ""),
      p("Use the graph to check output before downloading. The CSV can be converted to Lowrance USR format using GPS Babel, and the GPX should load directly into other systems."),
      downloadButton("downloadData", "Download adjusted lines as csv"),
      downloadButton("downloadGPX", "Download adjusted lines as GPX"),
      br(),
      h4("Preview:"),
      tableOutput("table")),
    
    mainPanel(
      plotOutput('finished_plot'),
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  sliderHeight<-reactive({
    req(lines())
    nrow(lines())*12+100
  })
  
  lines <- reactive({
    req(input$csv)
    
    if (nchar(input$stray)>0) {
      stray = input$stray
    }
    else {
      stray = " "
    }
    
    SWlines <- read_csv(input$csv$datapath, col_types = cols())
    
    lines <- SWlines %>% 
      filter(!str_detect(Name,'Total Survey Line Length')) %>% 
      pivot_longer(c(`Starting Latitude`, `Ending Latitude`), names_to = "dir", values_to = "Latitude") %>% 
      pivot_longer(c(`Starting Longitude`, `Ending Longitude`), names_to = "dir2", values_to = "Longitude") %>%
      mutate(
        Name=str_remove(Name, stray), #Change this to remove any stray number
        Name=parse_number(Name), #Extract row nuber from name column
        dir = case_when( #Rename starts and ends
          str_detect(dir, 'Start') ~ "ST",
          str_detect(dir, 'End') ~ "EN",),
        dir2 = case_when(
          str_detect(dir2, 'Start') ~ "ST",
          str_detect(dir2, 'End') ~ "EN",)) %>% 
      filter(dir==dir2) %>% #Drop redundant rows
      select(line = Name, dir, LAT =Latitude, LONG = Longitude) #keep relevant columns
    
    n = (nrow(lines)/2-1)/2 #calculate number of rows on each side
    
    if (input$shuffle) {
    lines_fixed <-  lines %>% 
      mutate(
        dir= if_else(line%%2==0, #Flip direction of even lines
                     case_when(
                       str_detect(dir, 'ST') ~ "EN",
                       str_detect(dir, 'EN') ~ "ST",), dir),
        dir= if_else((line>(n+1)|line==1), #Flip lines above and including the middle
                     case_when(
                       str_detect(dir, 'ST') ~ "EN",
                       str_detect(dir, 'EN') ~ "ST",), dir),
        line= if_else(line<=(n+1), n+2-line, line), #Tranform point to be sequential
        line= if_else(line>n, line*2-n*2, line*2-1), #Apply distancing algorithm
      )  %>% 
      filter(line<=n*2) #Drop the last line
    } else {lines_fixed <-  lines %>% mutate(
      line= if_else(line<=(n+1), n+2-line, line)) #Tranform point to be sequential
    }
    
    lines_fixed <- lines_fixed %>% 
      arrange(line, desc(dir)) %>% 
      mutate(Name = paste(line, dir, sep="-"))
      
    if (input$sequence) {
      lines_fixed <- lines_fixed %>% mutate(Name = row_number())
    }
    
    if (input$prefix!=""){
      prefix <- input$prefix
      lines_fixed <- lines_fixed %>% mutate(Name = paste0(prefix, Name))
    }
    
    return(lines_fixed)
  })
  
  output$finished_plot <- renderPlot({
    req(lines())
    #plot these adjusted waypoints
    ggplot(lines(), aes(LONG, LAT)) +
      geom_line(aes(group=line), linetype="dotted", alpha=0.5) +
      geom_point(colour='white', size =15) +
      geom_text(aes(label=Name, colour=dir),size=6, fontface = "bold") +
      labs(title="Waypoints after adjustments", caption = "*not to scale") + theme(legend.position="none") +
      scale_x_continuous(expand = expansion(mult = c(.1, .1)))
  }, height = sliderHeight, width=800)
  
  lines_output <- reactive({
    req(lines())
    lines() %>%
      select(Name, LAT, LONG)->
      lines_output
  })
  
  
  output$table <- renderTable({
    if (is.null(lines_output()))
      return(NULL)
    lines_output()
  }, digits=10)
  
  output$downloadData <- downloadHandler(
    filename = "lines_fixed.csv",
    content = function(file) {
      write_csv(lines_output(), file)
    }
  )
  
  output$downloadGPX <- downloadHandler(
    filename = "lines_fixed.gpx",
    content = function(file) {
      writeGPX(lines_output()$LAT,lines_output()$LONG, lines_output()$Name, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
