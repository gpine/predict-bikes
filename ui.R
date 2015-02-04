library(rCharts)


shinyUI(navbarPage(tags$head(tags$style("body {background-image: url(\"bikes2.jpg\"); }")),"DC Bike Share Analysis",
  tabPanel("Predict Demand", pageWithSidebar(
    headerPanel("D.C. Bike Share: Real-time Prediction of Demand by Station"),  
    # Sidebar with a slider input for the number of bins
    sidebarPanel(
      selectInput("s2", label="Select station from list below",
                  choices = fullnames, selected="10th St & Constitution Ave NW (31219)"),
      p("This chart shows expected demand for bikes over the next several days.
        Predictions derive from a Poisson modeling of historic usage patterns accounting for:"
      ),
      tags$ul(
        tags$li("hour of day (e.g. 3 PM)"), 
        tags$li("workday vs. weekend"), 
        tags$li("systemwide demand trends"),
        tags$li("weather")
      ),
      p("The system makes real-time weather-based predictions using an", a('openweathermap.org', href='http://openweathermap.org/', target='_blank') ,"API.
        Historic bike data comes from",a('Capital Bikeshare', href='https://www.capitalbikeshare.com/trip-history-data', target='_blank'), 
        "and has been constructed from raw data on millions of individual trips."
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(  
      #verbatimTextOutput("futureDemand")
      plotOutput("futurePlot", height="100%")
    )
  )),

  tabPanel("Map", pageWithSidebar(
    # Application title
    headerPanel("Most Popular Routes and Annual Demand by Station"),  
    # Sidebar with a slider input for the number of bins
    sidebarPanel(
      p("This map allows for easy visualization of the most popular routes and stations in the system. 
        Darker, bigger dots are stations with higher annual demand. 
        Yellow lines show the most popular routes in the system; larger lines represent more trips taken.
        Click on the dots or the lines for more information."
      ),
      sliderInput("year", "Year:", 
                  min=2011, max=2014, value=2012, format="####")
    ),
    mainPanel(  
      showOutput('map_container','leaflet')
    )
  )),
  
  tabPanel("Historic Demand", pageWithSidebar(
    # Application title
    headerPanel("Historic Hourly Bike Demand by Station"),  
    # Sidebar with a slider input for the number of bins
    sidebarPanel(
      selectInput("s1", label="Select station from list below",
                  choices = fullnames, selected="10th St & Constitution Ave NW (31219)"),
      p("This violin plot shows historic hourly demand for bikes by station.
        The width of the blobs represents the density of data at given values. Dark blue dots are medians."
      )
    ),    
    # Show a plot of the generated distribution
    mainPanel(  
      plotOutput("demandPlot")
    )
  ))  
))