library(TSP)
library(cpm)
concorde_path("D://Concorde/C/")
#concorde_help()
#linkern_help()

### find cpm price changing point
find_change <- function(x) { 
  for (i in 1:length(x)){
    j <- x[x>7]
    j <- j[j<13]
    ifelse (is.na(j[1]) == F, return(max(j)), return(NA))
  }
}

### find arrive cities
find_in <- function(from, to) {
  p2p <- subset( dfLongLat, city %in% c(from, to))

  to_area <- dfLongLat[sqrt((dfLongLat$lon-p2p[2,2])^2 + (dfLongLat$lat-p2p[2,3])^2)*110 < days*100,]
  to_area_t <- tf[tf$from %in% from & tf$to %in% to_area$city,]
  q.25 <- quantile(to_area_t$cp, probs = seq(0, 1, 0.25), names = F)[2]
  to_area_t <- to_area_t[to_area_t$cp < q.25 & to_area_t$time < 6,]
  to_area_t <- to_area_t[order(to_area_t$cp),]
  cpm <- processStream(to_area_t$cp,"Student",ARL0=500,startup=20) # cpm to find change in price
  cpm_cp <- ifelse(is.na(find_change(cpm$changePoints)), cpm$changePoints, find_change(cpm$changePoints))

  if (is.na(cpm_cp)==F){
    to_area_f <- to_area_t[1:cpm_cp,]
  } else {
    to_area_f <- to_area_f[1:5,]
  }
  to_area_f <- to_area_f[1:5,]
  return(to_area_f)
}

### find return cities
find_out <- function(from, to) {
  back_cities <- tf[tf$from %in% to_area_f$to & tf$to %in% from,]
  bq.25 <- quantile(back_cities$cp, probs = seq(0, 1, 0.25), names = F)[2]
  back_cities <- back_cities[back_cities$cp < bq.25 & back_cities$time < 6,]
  back_cities <- back_cities[order(back_cities$cp),]
  cpm <- processStream(back_cities$cp,"Student",ARL0=500,startup=20) #package cpm to find change in price
  cpm_cp <- ifelse(is.na(find_change(cpm$changePoints)), cpm$changePoints, find_change(cpm$changePoints))

  if (is.na(cpm_cp)==F){
    back_cities <- back_cities[1:cpm_cp,]
  } else {
    back_cities <- back_cities[1:5,]
  }
  back_cities <- back_cities[1:5,]
  return(back_cities)
}

### find path
find_path <- function(cty, min_trf, matrix_cbn, cty_consist,i){
  tmp <- matrix_cbn[rownames(matrix_cbn) %in% cty_consist[i,] ,colnames(matrix_cbn) %in% cty_consist[i,]]
  tmp <- rbind(tmp, tmp[rownames(tmp)%in%cty,])
  tmp <- cbind(tmp, tmp[,colnames(tmp)%in%cty])
  rownames(tmp)[nrow(tmp)] <- "Back"
  colnames(tmp)[ncol(tmp)] <- "Back"
  tsp <- ATSP(tmp)
  start <- which(labels(tsp) == cty)
  end <- which(labels(tsp) == "Back")
  atsp <- ATSP(tmp[-c(start,end), -c(start,end)])
  atsp <- insert_dummy(atsp, label = "st/ed")
  st_ed <- which(labels(atsp) == "st/ed")
  atsp[st_ed, ] <- c(tmp[start, -c(start,end)], 0)
  atsp[, st_ed] <- c(tmp[-c(start,end), end], 0)
  tour <- solve_TSP(atsp, method ="nearest_insertion") #nearest_insertion; two_opt
  tour <- solve_TSP(atsp, method ="2-opt", control = list(tour = tour))
  path_labels <- c(cty, labels(cut_tour(tour, st_ed)), "Back")
  pid <- match(path_labels, labels(tsp))
  pcy <- path_labels
  pcy[length(pcy)] <- cty
  tmp <- min_trf
  x <- sapply(tmp, is.factor)
  tmp[x] <- lapply(tmp[x], as.character)
  myList[[length(myList)+1]] <- c(attr(tour, "tour_length"), list(sapply(1:(length(pid)-1), function(x) {tmp[tmp$from == pcy[x] & tmp$to == pcy[x+1],]})))
  return(c(attr(tour, "tour_length"), list(sapply(1:(length(pid)-1), function(x) {tmp[tmp$from == pcy[x] & tmp$to == pcy[x+1],]}))))
}

### shortest path 2
s_path_each
function(i) {
  tmp1 <- replicate(10, {
    find_path1(cty_consist, i)
  })
  tmp1 <- tmp1[,!duplicated(lapply(tmp1[1,], round))]
  ifelse(length(tmp1)<3,
         return(tmp1),
         return(tmp1[,which(unlist(tmp1[1,] %in% c(min(unlist(tmp1[1,])),min( unlist(tmp1[1,])[unlist(tmp1[1,])!=min(unlist(tmp1[1,]))])) ))]) )
}

days <- 7 #7 days trip
from <- "Paris"
to <- "Barcelona"
time <- 10 #max travel hrs
go_typ <- unique(tf$go) 
tfc_trf_minall <- data.frame()

num_cities <- ceiling(days/2.5) #num of travel cities

###filter travel cities
tra_cities <- unique(c(from, to, as.character(find_out_1(from, to, days)$from), as.character(find_in1(from, to, days)$to)))
tfc_cities <- tf[tf$from %in% tra_cities & tf$to %in% tra_cities & tf$go %in% go_typ & tf$time < time,]

tfc_cities$from <- as.character(tfc_cities$from)
tfc_cities$to <- as.character(tfc_cities$to)
tfc_cities$by <- as.character(tfc_cities$by)

trf_min <- data.frame()

### filter lowest cost of each mode of traffic
trf_min <- sapply(seq(nrow(unique(tfc_cities[,1:2]))), function(i){
  tmp <- unique(tfc_cities[,1:2])[i,]
  tmp <- tfc_cities[tfc_cities$from %in% tmp$from[1] & tfc_cities$to %in% tmp$to[1],]
  tmp <- tmp[order(tmp$cp),][1,]
})

trf_min <- t(as.data.frame(trf_min,stringsAsFactors = F))
trf_min <- apply(trf_min, 2, unlist)
trf_min <- as.data.frame(trf_min)
trf_min$cp <- as.numeric(as.character(trf_min$cp))

### initial  distance matrix
matx <- matrix(10000, nrow=length(unique(trf_min$from)),ncol=length(unique(trf_min$to)),dimnames=c(list(as.character(c(from, as.character(unique(trf_min$from[!trf_min$from %in% from]))))),list(as.character(unique(trf_min$to)))))
for (i in 1:nrow(matx)) {
  for(j in 1:ncol(matx)) {
    # dist matrix for CP = 7
    # sim_001[i,j] <- ifelse(length(trf_min[trf_min$from %in% rownames(matx)[i] & trf_min$to %in% rownames(matx)[j] ,7]) == 0, trf_min[trf_min$from %in% rownames(matx)[i] & trf_min$to %in% rownames(matx)[j] ,7], 10000)
    matx[i,j] <- ifelse(rownames(matx)[i] == colnames(matx)[j], 10000, trf_min[trf_min$from %in% rownames(matx)[i] & trf_min$to %in% colnames(matx)[j] ,7])
  }
}

matx[is.na(matx)] <- 10000
matx <- matx[match(colnames(matx),rownames(matx)),]

### possible combination of travel relay points 
trip_ctys <- tra_cities[!tra_cities %in% c(from, to)]
ifelse(length(trip_ctys)>num_cities, ci_cbn <- t(combn(trip_ctys, num_cities)),ci_cbn <- t(combn(trip_ctys, length(trip_ctys)-1)) )

ci_cbn <- cbind(rep(from, nrow(ci_cbn)), ci_cbn, rep(to, nrow(ci_cbn)))
print(ci_cbn)

list_1 <- list()
list_2 <- list()

### optimize TSP 
tmp3 <- matrix(1,nrow =2)
for(i in 1:nrow(ci_cbn)){
  tmp1 <- replicate(10, {
    find_path1(from, trf_min, matx, ci_cbn,i)
  })
  tmp1 <- tmp1[,!duplicated(lapply(tmp1[1,], round))]

  if(length(tmp1)<3){
    tmp1_2 <- tmp1
    x <- as.matrix(tmp1_2[[2]])
    list_2[[length(list_2)+1]] <- list(x)
    list_1[[length(list_1)+1]] <- tmp1_2[[1]]
    tmp3 <- cbind(tmp3, tmp1_2)
  }else{
    tmp1_2 <- tmp1[,which(unlist(tmp1[1,]) %in% c(min(unlist(tmp1[1,])),
              min( unlist(tmp1[1,])[unlist(tmp1[1,])!=min(unlist(tmp1[1,]))])) )] 
    x <- lapply(tmp1_2[2,], as.matrix)
    list_2[[length(list_2)+1]] <- list(x)
    list_1[[length(list_1)+1]] <- tmp1_2[1,]
    tmp3 <- cbind(tmp3, tmp1_2)
  }
}

### outcome routes list
tmp3 <- tmp3[,order(unlist(tmp3[1,]))][,-1]
tmp3 <- tmp3[,1:8]
tmp4 <- data.frame()
for (i in 1:ncol(tmp3)) {
  x <- matrix(unlist(tmp3[2,i]), ncol  = 7, byrow = TRUE)
  x <- cbind(x, rep(tmp3[1,i], nrow(x)))
  x <- data.frame(x , stringsAsFactors = F, row.names = 1:nrow(x))
  tmp4 <- rbind(tmp4, x)
}

tmp4$X8 <- unlist(unname(tmp4$X8))
lapply(1:8, function(x) rep(x, nrow(tmp4[tmp4$X8 %in% unique(tmp4$X8)[x],])))
nrow(tmp4$X8[tmp4$X8 %in% unique(tmp4$X8)[x],])

cbn_list <- lapply(1:nrow(ci_cbn), s_path_each)

for (i in which(unlist(lapply(list_2, length))>3)) {
  list_2[[length(cbn_list)+1]] <- list_2[[i]][,2]
  list_2[[i]][3:4] <- NULL
}

trf_mincbn_d_p <- sapply(cbn_list,"[[",1)
trf_mincbn <- cbn_list[order(trf_mincbn_d_p)][1:5]

##### return travel route selections
trf_mincbn <- path_cbn(from, to, days)

path <- function(trf_min, x) {
  tmp <- t(as.data.frame(trf_mincbn[[x]][2], stringsAsFactors = F))
  tmp <- unname(tmp)
  return(paste0(paste0(c(unlist(tmp[,1]), unlist(tmp[,1][1])), collapse = " > "), " £", trf_mincbn_d_p[order(trf_mincbn_d_p)][x] ))
}

