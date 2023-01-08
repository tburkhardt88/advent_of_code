lines <- readLines('input.txt')

#Sensor coordinates
sensors <- sapply(lines, function(sns){sub(":.+$", "", sns)}, simplify = T, USE.NAMES = F)
sensors <- lapply(sensors, function(sns){strsplit(sub("^Sensor\\sat\\s", "", sns), ",\\s")[[1]]})
sensors <- lapply(sensors, function(sns){
  sapply(sns, function(crd){
    as.numeric(sub("^.=", "", crd))
  }, simplify = T, USE.NAMES = F)
})

#Beacon coordinates
beacons <- sapply(lines, function(bcs){sub("^.+:\\s", "", bcs)}, simplify = T, USE.NAMES = F)
beacons <- lapply(beacons, function(bcs){strsplit(sub("^closest\\sbeacon\\sis\\sat\\s", "", bcs), ",\\s")[[1]]})
beacons <- lapply(beacons, function(bcs){
  sapply(bcs, function(crd){
    as.numeric(sub("^.=", "", crd))
  }, simplify = T, USE.NAMES = F)
})

#In dataframe format
locations <- data.frame(
  sensor_x = sapply(sensors, function(sns){sns[1]}),
  sensor_y = sapply(sensors, function(sns){sns[2]}),
  beacon_x = sapply(beacons, function(sns){sns[1]}),
  beacon_y = sapply(beacons, function(sns){sns[2]})
)
locations$distance = abs(locations$sensor_x - locations$beacon_x) + abs(locations$sensor_y - locations$beacon_y)

#Testing row 2000000 (Part 1)
x_coords <- seq(min(locations$sensor_x - locations$distance), max(locations$sensor_x + locations$distance))
coords_to_test <- cbind(
  as.matrix(x_coords),
  as.matrix(rep(2000000, length(x_coords)))
)
no_beacon = logical(length = length(x_coords))

for(i in 1:length(no_beacon)){
  for(s in 1:nrow(locations)){
    if(sum(abs(sensors[[s]] - coords_to_test[i,])) <= locations$distance[s]){
      no_beacon[[i]] <- TRUE
      break
    }
  }
}

beacons_on_line_2E6 = unique(locations[locations$beacon_y == 2E6,3:4])

length(which(no_beacon)) - nrow(beacons_on_line_2E6)
