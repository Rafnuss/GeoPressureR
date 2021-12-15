#' Import PAM data
#'
#' @description Imports and formats many datasets into one big nested list containing all the data from the different sensors. A subset of sensors can be selected using `measurements`.
#'
#' @param pathname path where files are stored
#' @param measurements a series of measurements logged by the PAM logger which are to be imported. Currently supports these file extentions: ".pressure", ".glf", ".gle",".acceleration", ".temperature", "AirTemperature", ".BodyTemperature" and ".magnetic"
#'
#' @return a list of measurements for the one individual
#'
#' @export
create_import <- function(pathname = pathname,
                          measurements = c(".pressure", ".glf", ".acceleration", ".temperature", ".magnetic")){
  dta=list()

  id = substring(list.files(pathname,pattern=measurements[1],full.names = FALSE),1,4)
  dta$id = id
  if(".pressure" %in% measurements){
    tryCatch({
    pressure = utils::read.delim(list.files(pathname,pattern=".pressure",full.names = TRUE),skip=6,sep="",header = FALSE)
    pressure = as.data.frame(list(date=as.POSIXct(strptime(paste(pressure[,1],pressure[,2]),
                                                           tz="UTC",format="%d.%m.%Y %H:%M")),obs=pressure[,3]))
    dta$pressure = pressure
    }, error = function(e) return("no pressure data"))
  }
  if(".glf" %in% measurements){
    tryCatch({
    light = utils::read.delim(list.files(pathname,pattern=".glf",full.names = TRUE)[1],skip=6,sep="",header = FALSE)
    light = as.data.frame(list(date=as.POSIXct(strptime(paste(light[,1],light[,2]),
                                                        tz="UTC",format="%d.%m.%Y %H:%M")),obs=light[,3]))
    dta$light = light
    }, error = function(e) return("no light data"))
  }
  if(".gle" %in% measurements){
    tryCatch({
    light = utils::read.delim(list.files(pathname,pattern=".gle",full.names = TRUE)[1],skip=6,sep="",header = FALSE)
    light = as.data.frame(list(date=as.POSIXct(strptime(paste(light[,1],light[,2]),
                                                        tz="UTC",format="%d.%m.%Y %H:%M")),obs=light[,3]))
    dta$light = light
    }, error = function(e) return("no light data"))
  }
  if(".acceleration" %in% measurements){
    tryCatch({
    acceleration = utils::read.delim(list.files(pathname,pattern=".acceleration",full.names = TRUE),skip=6,sep="",header = FALSE)
    acceleration = as.data.frame(list(date=as.POSIXct(strptime(paste(acceleration[,1],acceleration[,2]),
                                                               tz="UTC",format="%d.%m.%Y %H:%M")),pit=acceleration[,3], act=acceleration[,4]))
    dta$acceleration = acceleration
    }, error = function(e) return("no acceleration data"))
  }
  if(".temperature" %in% measurements){
    tryCatch({
    temperature = utils::read.delim(list.files(pathname,pattern=".temperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    temperature = as.data.frame(list(date=as.POSIXct(strptime(paste(temperature[,1],temperature[,2]),
                                                              tz="UTC",format="%d.%m.%Y %H:%M")),obs=temperature[,3]))
    dta$temperature = temperature
    }, error = function(e) return("no temperature data"))
  }
  if(".AirTemperature" %in% measurements){
    tryCatch({
    temperature = utils::read.delim(list.files(pathname,pattern=".AirTemperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    temperature = as.data.frame(list(date=as.POSIXct(strptime(paste(temperature[,1],temperature[,2]),
                                                              tz="UTC",format="%d.%m.%Y %H:%M")),obs=temperature[,3]))
    dta$temperature = temperature
    }, error = function(e) return("no air temperature data"))
  }
  if (".BodyTemperature" %in% measurements){
    tryCatch({
    bodytemperature = utils::read.delim(list.files(pathname,pattern=".BodyTemperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    bodytemperature = as.data.frame(list(date=as.POSIXct(strptime(paste(bodytemperature[,1],bodytemperature[,2]),
                                                                  tz="UTC",format="%d.%m.%Y %H:%M")),obs=bodytemperature[,3]))
    dta$bodytemperature = bodytemperature
    }, error = function(e) return("no light data"))
  }
  if(".magnetic" %in% measurements){
    tryCatch({
    magnetic = utils::read.delim(list.files(pathname,pattern=".magnetic",full.names = TRUE),skip=6,sep="",header = FALSE)
    magnetic = as.data.frame(list(date=as.POSIXct(strptime(paste(magnetic[,1],magnetic[,2]),
                                                           tz="UTC",format="%d.%m.%Y %H:%M")),
                                  gX=magnetic[,4],gY=magnetic[,5],gZ=magnetic[,6],
                                  mX=magnetic[,7],mY=magnetic[,8],mZ=magnetic[,9]))
    dta$magnetic = magnetic
    }, error = function(e) return("no magnetic data"))
  }

  dta
}






#' Crop all sensor data to the same timeframe
#'
#' @description Get rid of excess data. e.g. when a logger is kept in a rucksack or a lab before being downloaded.
#'
#' @param dta path where files are stored
#' @param start posicxt object for date that PAM data should start
#' @param end posicxt object for date that PAM data should end
#'
#' @return shortened PAM data
#'
#' @export
create_crop <- function(dta, start, end){
  test <- lapply(1:length(dta), function(x) {if(x>=2) {
    as.data.frame(dta[[x]])[(as.data.frame(dta[[x]])[,1] >= start & as.data.frame(dta[[x]])[,1] < end),]
  }else{ as.character(dta[[x]])}
  })
  names(test) = names(dta)
  return(test)
}










#' Classify flapping flight
#'
#' @description This function uses activity data to classify migratory flapping flight.
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param period number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 3x5min = 15 minutes of intense activity is considered flight
#' @param tz timezone, default is "UTC"
#'
#' @return timetable: a timetable for when the species was migrating or not,
#' @return classification: a classification timeseries where datetime corresponds to activity, and
#' @return no_movement: the value in classification which corresponds to no movement
#' @return low_movement: the value in classification which corresponds to low activity
#' @return high_movement: the value in classification which corresponds to high activity
#' @return migration: the value in classification which corresponds to migratory flapping flight
#' @return threshold: the threshold between low and high activity
#'
#'
#' @export
classify_flap <- function(dta ,
                          period = 12,
                          tz= "UTC"){

  km = stats::kmeans(dta$act,centers=2)
  dta$clust = km$cluster

  type = "flapping"
  threshold = sum(min(max(dta$act[dta$clust==1],na.rm=TRUE), max(dta$act[dta$clust==2],na.rm= TRUE)),
                  max(min(dta$act[dta$clust==1],na.rm= TRUE), min(dta$act[dta$clust==2],na.rm= TRUE)))/2

  # Count the length of each category
  start=0
  end=0

  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz=tz,format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz=tz,format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_movement = as.numeric(which(table(dta$clust) == min(table(dta$clust),na.rm= TRUE)))
  low_movement = as.numeric(which(table(dta$clust) == max(table(dta$clust),na.rm= TRUE)))
  dta$clust[is.na(dta$clust)] =  low_movement

  # get rid of 1-off missclassifications
  x = c(high_movement,low_movement,high_movement)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = high_movement

  x = c(low_movement,high_movement)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  x = c(high_movement, low_movement)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]

  if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end)<length(start)) start= start[1:length(end)]
  if (length(end)>length(start)) end= end[1:length(start)]


  # make sure only periods where birds is flying longer than the flapping duration are stored
  index = which((end-start) >= period)
  start = start[index]
  end = end[index]

  index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
  dta$clust[index] = 3


  #look for start and end of migration
  end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
  start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)+ 1] ))

  dur = difftime(dta$date[end], dta$date[start], tz= tz, units = "hours")
  info = data.frame(dta$date[start], dta$date[end], dur)
  names(info) = c("start","end","Duration (h)")
  Duration_table = rbind(Duration_table, info)


  # order so that low movement is lower than high movement
  if (high_movement == 1){
    dta$clust[dta$clust == 1] = 999
    dta$clust[dta$clust == 2] = 1
    dta$clust[dta$clust == 999] = 2
  }

  Duration_table = Duration_table[-c(1,2),]
  dta$clust[dta$act == 0] = 0
  return(list(timetable = Duration_table,
              classification = dta$clust,
              no_movement = 0,
              low_movement = 1,
              high_movement = 2,
              migration = 3,
              threshold = threshold))
}

