require(httr); library(chron);
namesdf <- read.csv("by_name.csv")
demand<-read.csv("hourly_demand_2011.csv", check.names=FALSE)
fullnames = as.character(namesdf[[1]])
demand$hour <- factor(demand$hour)
demand$weather <- factor(demand$weather)
demand$workingday <- factor(demand$workingday)
#remove weather = 4
demand$weather[demand$weather == 4] <- 3
#reorder columns
demand <- demand[ ,c(152:153,3:151)]


# Read in the weather forecast
json_file = "http://api.openweathermap.org/data/2.5/forecast/city?id=4140963&APPID=fc2569a082fdb0a17c4cf94785880008"
json_data <- content(GET(json_file))
jsonlength = length(json_data[[5]])
fdf <- data.frame(matrix(ncol = 6, nrow = jsonlength)) 
names(fdf) <- names(demand[ ,1:6])
futuredates = character(jsonlength)
for (i in 1:jsonlength) {
  fdf$temp[i] = json_data$list[[i]]$main$temp - 273.15
  fdf$windspeed[i] = json_data$list[[i]]$wind$speed
  fdf$weather[i] = json_data$list[[i]]$weather[[1]]$description
  fdf$hour[i] = substring(json_data$list[[i]]$dt_txt,12,13)
  fdf$workingday[i] = is.weekend(json_data$list[[i]]$dt_txt) * 1
  futuredates[i] = json_data$list[[i]]$dt_txt
}
fdf$hourspassed = 17520
futuredates <- strptime(futuredates, "%Y-%m-%d %H:%M:%S")

# Factorize the weather
for (i in 1:jsonlength) {
  if (grepl("snow", fdf$weather[i])) {
    fdf$weather[i] = 3
  }
  else if (grepl("rain", fdf$weather[i])) {
    fdf$weather[i] = 3
  }
  else if (grepl("thunderstorm", fdf$weather[i])) {
    fdf$weather[i] = 3
  }
  else if (grepl("mist", fdf$weather[i])) {
    fdf$weather[i] = 2
  }
  else{
    fdf$weather[i] = 1
  }
}
fdf$weather = as.factor(fdf$weather)
fdf$workingday = as.factor(fdf$workingday)
fdf$hour = as.factor(as.integer(fdf$hour))  # Remove leading zeros

# Fill in the missing hours with the assumption of consistent weather
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

for (i in length(fdf[,1]):2) {
  fdf = insertRow(fdf, fdf[i,], i)
  fdf = insertRow(fdf, fdf[i,], i)
}

i = 1
while (i < (length(futuredates))) {
  futuredates <- append(futuredates, futuredates[i] + 3600, after=i)
  futuredates <- append(futuredates, futuredates[i+1] + 3600, after=i+1)
  i = i + 3
}

fdf$hour = as.integer(as.character(fdf$hour))
for (i in 2:length(fdf[,1])) {
  fdf$hour[i] = fdf$hour[i-1] + 1
  if (fdf$hour[i] %% 24 == 0) {
    fdf$hour[i] = 0
  } 
}

fdf$hour = as.factor(as.integer(fdf$hour))  # Remove leading zeros