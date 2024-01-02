print(rawdata)
clean_batsman <- rawdata[is.na(rawdata$End), ]
print(clean_batsman)

names(clean_batsman)[names(clean_batsman) == '100'] <- 'centuries'
names(clean_batsman)[names(clean_batsman) == '50'] <- 'fifties'
names(clean_batsman)[names(clean_batsman) == '0'] <- 'zeroes'
names(clean_batsman)[names(clean_batsman) == '4s'] <- 'fours'
names(clean_batsman)[names(clean_batsman) == '6s'] <- 'sixes'
print(clean_batsman)

#Consistency = 0.4262*average + 0.2566*no. of innings + 0.1510*SR + 0.0787*Centuries + 0.0556*Fifties – 0.0328*Zeros
clean_batsman$consistency <- (0.4262 * clean_batsman$Ave +
                                0.2566 * clean_batsman$Inns +
                                0.1510 * clean_batsman$SR +
                                0.0787 * clean_batsman$centuries +
                                0.0556 * clean_batsman$fifties) -
                                (0.0328 * clean_batsman$zeroes)

r_inns <- c()
for (x in 1:47){
  if(clean_batsman$Inns[x] >= 150){
    r_inns <- c(r_inns, 5)
  } else if(clean_batsman$Inns[x] >= 125 & clean_batsman$Inns[x] <= 149){
    r_inns <- c(r_inns, 4)
  } else if(clean_batsman$Inns[x] >= 100 & clean_batsman$Inns[x] <= 124){
    r_inns <- c(r_inns, 3)
  } else if(clean_batsman$Inns[x] >= 50 & clean_batsman$Inns[x] <= 99){
    r_inns <- c(r_inns, 2)
  } else {
    r_inns <- c(r_inns, 1)
  }
}

r_ave <- c()
for (x in 1:47){
  if(clean_batsman$Ave[x] >= 33){
    r_ave <- c(r_ave, 5)
  } else if(clean_batsman$Ave[x] >= 27 & clean_batsman$Ave[x] < 33){
    r_ave <- c(r_ave, 4)
  } else if(clean_batsman$Ave[x] >= 21 & clean_batsman$Ave[x] < 27){
    r_ave <- c(r_ave, 3)
  } else if(clean_batsman$Ave[x] >= 15 & clean_batsman$Ave[x] < 21){
    r_ave <- c(r_ave, 2)
  } else {
    r_ave <- c(r_ave, 1)
  }
}

r_sr <- c()
for (x in 1:47){
  if(clean_batsman$SR[x] >= 138){
    r_sr <- c(r_sr, 5)
  } else if(clean_batsman$SR[x] >= 125 & clean_batsman$SR[x] < 135){
    r_sr <- c(r_sr, 4)
  } else if(clean_batsman$SR[x] >= 118 & clean_batsman$SR[x] < 125){
    r_sr <- c(r_sr, 3)
  } else if(clean_batsman$SR[x] >= 105 & clean_batsman$SR[x] < 118){
    r_sr <- c(r_sr, 2)
  } else {
    r_sr <- c(r_sr, 1)
  }
}


clean_batsman$r_inns <- r_inns
clean_batsman$r_ave <- r_ave
clean_batsman$r_sr <- r_sr
clean_batsman$r_agg <- r_inns + r_ave + r_sr
print(clean_batsman)

barplot(names.arg = clean_batsman$Player, clean_batsman$Inns)
barplot(names.arg = clean_batsman$Player, clean_batsman$r_agg)

plot(clean_batsman$r_inns, type = "o", col = "red")
lines(clean_batsman$r_ave, type = "o", col = "blue")
lines(clean_batsman$r_sr, type = "o", col = "black")

shortlist <- clean_batsman[clean_batsman$r_agg > 10, ]

names(ODIstats)[names(ODIstats) == '100s'] <- 'centuries'
names(ODIstats)[names(ODIstats) == '50s'] <- 'fifties'
names(ODIstats)[names(ODIstats) == '4s'] <- 'fours'
names(ODIstats)[names(ODIstats) == '6s'] <- 'sixes'

ODIstats$consistency <- (0.4262 * ODIstats$Ave +
                           0.2566 * ODIstats$Inns +
                           0.1510 * ODIstats$SR +
                           0.0787 * ODIstats$centuries +
                           0.0556 * ODIstats$fifties)
formOdi$form <- (0.4262 * formOdi$Avg +
                           0.2566 * formOdi$Inns +
                           0.1510 * formOdi$SR)

r_inns <- c()
for (x in 1:18){
  if(ODIstats$Inns[x] >= 170){
    r_inns <- c(r_inns, 5)
  } else if(ODIstats$Inns[x] >= 125 & ODIstats$Inns[x] < 170){
    r_inns <- c(r_inns, 4)
  } else if(ODIstats$Inns[x] >= 85 & ODIstats$Inns[x] < 125){
    r_inns <- c(r_inns, 3)
  } else if(ODIstats$Inns[x] >= 50 & ODIstats$Inns[x] < 85){
    r_inns <- c(r_inns, 2)
  } else {
    r_inns <- c(r_inns, 1)
  }
}
r_avg <- c()
for (x in 1:18){
  if(ODIstats$Ave[x] >= 47){
    r_avg <- c(r_avg, 5)
  } else if(ODIstats$Ave[x] >= 40 & ODIstats$Ave[x] < 47){
    r_avg <- c(r_avg, 4)
  } else if(ODIstats$Ave[x] >= 33 & ODIstats$Ave[x] < 40){
    r_avg <- c(r_avg, 3)
  } else if(ODIstats$Ave[x] >= 26 & ODIstats$Ave[x] < 33){
    r_avg <- c(r_avg, 2)
  } else {
    r_avg <- c(r_avg, 1)
  }
}
r_sr <- c()
for (x in 1:18){
  if(ODIstats$SR[x] >= 100){
    r_sr <- c(r_sr, 5)
  } else if(ODIstats$SR[x] >= 93 & ODIstats$SR[x] < 100){
    r_sr <- c(r_sr, 4)
  } else if(ODIstats$SR[x] >= 86 & ODIstats$SR[x] < 93){
    r_sr <- c(r_sr, 3)
  } else if(ODIstats$SR[x] >= 79 & ODIstats$SR[x] < 86){
    r_sr <- c(r_sr, 2)
  } else {
    r_sr <- c(r_sr, 1)
  }
}

ODIstats$r_inns <- r_inns
ODIstats$r_avg <- r_avg
ODIstats$r_sr <- r_sr
