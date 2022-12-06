signal <- strsplit(scan('input.txt',character()),'')[[1]]

marker_found <- FALSE
marker <- 4-1
while(!marker_found){
  marker <- marker + 1
  sequence <- signal[(marker - 3):marker]
  marker_found <- length(unique(sequence)) == 4
}
marker

message_marker_found <- FALSE
message_marker <- 14-1
while(!message_marker_found){
  message_marker <- message_marker + 1
  sequence <- signal[(message_marker - 13):message_marker]
  message_marker_found <- length(unique(sequence)) == 14
}
message_marker
