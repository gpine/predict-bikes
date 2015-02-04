library(shiny); library(ggplot2); library(arcdiagram); library(reshape2); library(plyr);
library(leaflet); library(stringr); library(ggmap); library(downloader); library(XML);
library(httr); library(RColorBrewer); library(rCharts); library(httr); library(RJSONIO)
library(timeDate); library(caret); library(Metrics);
library(ada); library(boot); library(biglm); library(chron); library(scales);

demandPlot <- function(station) {
  title = paste("Hourly demand for bikes at",station)
  p <- qplot(demand[["hour"]],demand[[station]],data=demand)
  p + geom_violin(scale = "width",fill="#9ecae1") + 
    stat_summary(fun.y="median", geom="point",size=3, col="#084594") +
    ylab("Number of bikes rented\n(red dot = median)") + xlab("Hour") + ggtitle(title) +
    theme(axis.text.x = element_text(face = "bold", color = "#525252", size = 12),
          axis.text.y = element_text(face = "bold", color = "#525252", size = 12),
          plot.title = element_text(face = "bold", color = "#252525", size = 16),
          axis.title.x = element_text(face = "bold", color = "#252525", size = 14),
          axis.title.y = element_text(face = "bold", color = "#252525", size = 14),
          plot.background=element_blank()) 
}

plotMap <- function(filename) {
  colchoice = "red"
  ptl_str = paste("#! function(feature, latlng){
        return L.circleMarker(latlng, {
                  radius: feature.properties.weight || 5,
                  fillColor: feature.properties.fillColor || '", colchoice, "',    
                  color: '#000',
                  weight: 1,
                  fillOpacity: 0.8})
                  } !#", sep="")
  filename <- paste("mygeojson", filename, ".js", sep="")
  geojson2 <- RJSONIO::fromJSON(filename)
  map3 <- Leaflet$new()
  map3$setView(c(38.892682, -77.031681), zoom = 12)
  map3$geoJson(geojson2, 
               onEachFeature = "#! function(feature, layer){
            layer.bindPopup(feature.properties.popup);
            if (layer instanceof L.Polyline) {
              layer.setStyle({
                'color': '#fec44f',
                'weight': feature.style.weight,
                'opacity': .6
              });
            }
          } !#",        
               pointToLayer = ptl_str)
  map3$tileLayer(provider = 'Stamen.TonerLite')
  map3
  }

predictDemand <- function(station) {
  train_data = cbind(demand[ ,c(1:6)], demand[, station])
  
  # Do the predicting
  i=7
  set.seed(503)
  formula <- train_data[,i] ~ workingday + weather + temp + hour + windspeed + hourspassed
  train_data[,i] = as.numeric(train_data[,i])
  fitPoisson2 <- glm(formula, data = train_data, family = "poisson")
  answersPoisson2 <- exp(predict(fitPoisson2, fdf))
  # Plot the results  
  blues =  c("#084594", "#1B539C", "#2E62A5", "#4170AE", "#557FB7", "#688DC0", "#7B9CC9" ,"#8EAAD2", "#A2B9DB",
  "#B5C7E4", "#C8D6ED" ,"#DBE4F6" ,"#DBE4F6", "#C8D6ED", "#B5C7E4", "#A2B9DB", "#8EAAD2", "#7B9CC9",
  "#688DC0" ,"#557FB7", "#4170AE" ,"#2E62A5" ,"#1B539C" ,"#084594")  
  hr_factor = format(futuredates, format="%H:%M")
  futuredates <- as.POSIXct(strptime(futuredates, "%Y-%m-%d %H:%M:%S"))
  title = paste("Demand for bikes beginning",
                format(futuredates[1], format="%l %p"),
                "on", format(futuredates[1], format="%b %e, %Y"),
                "\n(using real-time weather-based prediction)
                \n",station)
  p <- qplot(futuredates, answersPoisson2, fill=hr_factor)
  p + ylab("Predicted # bikes rented") + xlab("Upcoming day and time") + ggtitle(title) + 
    scale_x_datetime(breaks = date_breaks("8 hours"), labels = date_format("%a\n%l %p")) +
    theme(axis.text.x = element_text(face = "bold", color = "#525252", size = 12),
          axis.text.y = element_text(face = "bold", color = "#525252", size = 12),
          plot.title = element_text(face = "bold", color = "#252525", size = 16),
          axis.title.x = element_text(face = "bold", color = "#252525", size = 14),
          axis.title.y = element_text(face = "bold", color = "#252525", size = 14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          plot.background=element_blank()) +
    guides(fill=guide_legend(ncol=2, title="Hour of the day")) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values=blues)
}


# Server workflow
shinyServer(function(input, output) {

  output$demandPlot <- renderPlot({demandPlot(input$s1)}, bg="transparent")
  output$futurePlot <- renderPlot({predictDemand(input$s2)}, bg="transparent",height = 470)
  output$fullnames <- renderPrint({as.character(namesdf[,1])})
  output$map_container <- renderMap({ plotMap(input$year) })
})