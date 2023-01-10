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

#Distances 
distances <- numeric(length = length(sensors))
for(i in 1:length(sensors)){distances[i] <- sum(abs(sensors[[i]] - beacons[[i]]))}

#Testing row 2000000 (Part 1)
x_coords <- seq(min(sapply(1:length(sensors), function(i){sensors[[i]][1] - distances[i]})), 
                max(sapply(1:length(sensors), function(i){sensors[[i]][1] + distances[i]})))

no_beacon = logical(length = length(x_coords))

for(i in 1:length(no_beacon)){
  for(s in 1:length(sensors)){
    if(sum(abs(sensors[[s]] - c(x_coords[i], 2E6))) <= distances[s]){
      no_beacon[[i]] <- TRUE
      break
    }
  }
}

beacons_on_line_2E6 = unique(locations[locations$beacon_y == 2E6,3:4])

length(which(no_beacon)) - nrow(beacons_on_line_2E6)


#Part 2
perimeters <- list()
for(i in 1:length(sensors)){
  perimeter_x <- seq(sensors[[i]][1] - distances[i] - 1, sensors[[i]][1] + distances[i] + 1) #add and subtract 1 to get outside perimeter
  perimeter_y_positive <- sensors[[i]][2] + ((distances[i] + 1) - abs(perimeter_x - sensors[[i]][1]))
  perimeter_y_negative <- sensors[[i]][2] - ((distances[i] + 1) - abs(perimeter_x - sensors[[i]][1]))
  perimeters[[i]] <- rbind(cbind(perimeter_x, perimeter_y_positive), cbind(perimeter_x, perimeter_y_negative))
}
rm(list = c('perimeter_y_negative','perimeter_y_positive','perimeter_x','i'))

check_coords <- perimeters[[1]]
for(i in 2:length(perimeters)){
  check_coords <- rbind(check_coords, perimeters[[i]])
}

check_coords <- check_coords[check_coords[,1] <= 4E6 & check_coords[,1] >= 0 & check_coords[,2] <= 4E6 & check_coords[,2] >= 0,] #confine to within the 0 - 4E6 box

unchecked = rep(T, nrow(check_coords))
for(i in 1:length(unchecked)){
  for(s in 1:length(sensors)){
    if(sum(abs(sensors[[s]] - check_coords[i,])) <= distances[s]){
      unchecked[[i]] <- F
      break
    }
  }
}

distress_beacon <- unique(check_coords[which(unchecked),])
format(distress_beacon[1] * 4E6 + distress_beacon[2], scientific = F)
