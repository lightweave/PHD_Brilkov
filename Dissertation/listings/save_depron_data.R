library('readr')
library('dplyr')
library('tidyr')
library('lattice')
library('hexbin')
Sys.setenv(TZ='GMT')
setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/')
cat(memory.size())


lomonosov.start <- as.Date('2016-05-10 22:23')

# navigation load ---------------------------------------------------------

LoadNav <- function(n = 0){
  cat('day #', n, '\n')
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/download/lomonosov/coordinates/')
  # setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/all/nav')
  
  data.nav<-NULL
  
  pattern = paste0("^CS_",lomonosov.start + n-1)
  files <- list.files(pattern = pattern)
  
  pattern1 = paste0("^CS_",lomonosov.start + n)
  files <- rbind(files, list.files(pattern = pattern1))
  
  
  # pattern2 = paste0("^CS_",lomonosov.start + n+1)
  # files <- rbind(files, list.files(pattern = pattern2))
  
  cat(files)
  
  start <- as.POSIXct(lomonosov.start + n, 'GMT')
  end <- as.POSIXct(lomonosov.start + n+1, 'GMT')
  
  dt = seq(start, end, by = "sec")
  data.nav <- data_frame(dt)
  old.day <- NULL
  
  
  for (file in files){
    data.nav.day<-read_csv(file, col_names = FALSE)
    header <- c(
      'dt'          ,
      'xgeo'        ,
      'ygeo'        ,
      'zgeo'        ,
      'latgeo'      ,
      'longeo'      ,
      'altgeo'      ,
      'xigrf'       ,
      'yigrf'       ,
      'zigrf'       ,
      'modigrf'     ,
      'R'           ,
      'latdm'       ,
      'londm'       ,
      'xgsm'        ,
      'ygsm'        ,
      'zgsm'        ,
      'mlt'         ,
      'shad'        ,
      'l'           ,
      'b')
    
    data.nav.day <- setNames(data.nav.day, header)
    # cat(nrow(data.nav), nrow(data.nav.day))
    
    old.day <- rbind(old.day, data.nav.day)

    
  }
  
  # data.nav <- unique(data.nav)
  old.day <- distinct(old.day, dt)
  
  old.day$longeo.center <- ifelse(old.day$longeo > 180,
                                  old.day$longeo - 360,
                                  old.day$longeo)
  
  # old.day <-  arrange(old.day, dt)
  

  
  # print(summary(old.day$dt))
  # print(summary(data.nav$dt))
  
  # plot (old.day$dt)
  # points (data.nav$dt)
  # old.day<-old.day[old.day$dt >= start,]
  # old.day<-old.day[old.day$dt < end,]
  # data.nav <- left_join(data.nav, data.nav.day, by = c('dt'))
  # plot (old.day$dt, type = 'l')
  # points (data.nav$dt)
  data.nav <- merge(data.nav, old.day, all.x = TRUE)
  # data.nav$dates <- data.nav$dt
  # 

  


  return(data.nav)
}

# data.nav <- LoadNav(0)

# saving navigation data---------------------------------------------------
# день года - n начиная с дня 0
SaveNav <- function(n, combined){
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')

  n <- as.difftime(n, units = "days")
  doy <- strftime(lomonosov.start + n, format = "%j") 
  
  filename <- sprintf("y16_d%03s.tsv", doy)
  
  write_tsv(combined, filename,append = FALSE)
}

# depron data load --------------------------------------------------------
LoadDepron <- function(n){
setwd("d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/all/oneday/")
filename <- sprintf( "%03sAsec.tsv",n)
if (!file.exists(filename))
  return()


data.sec<-read_tsv(filename,
                   col_types = cols('c', 'i','i','i','i', 'i','i', 'i','i', 'c','c'))


# date correction ---------------------------------------------------------


data.sec<-separate(data.sec, 'YYYY-MM-DD hh:mm:ss-1s',c("date", "time"),
                   sep=' ')
data.sec<-separate(data.sec, 'date',c("year", "month", "day"),
                   sep='-', convert = TRUE)



# изготовление даты из года и месяца, первого дня месяца и 12:00 по умолчанию
data.sec$dates <- ISOdate(data.sec$year, data.sec$month, 1)
# получение правильного дня из дня который вышел за границы месяца
data.sec$dates <- data.sec$dates + (as.integer(data.sec$day) - 1) * 60*60*24
# установка 00:00  
data.sec$dates <- data.sec$dates - 60*60*12 
# установка вермени по часам прибора
data.sec$dates <- data.sec$dates + parse_time(data.sec$time)

# str(data.sec$dates)
 # table(as.Date(data.sec$dates))

# print(table(trunc(data.sec$dates,'day') ))
# data.sec$dates <- as.POSIXct(strptime(data.sec[,1], format="%Y-%m-%d %H:%M:%S", tz='MSK'))


# data.sec <- data.sec[-1,]

# data.sec<-read_tsv("d:/Ivan/depron/depron-57AD5F1E.datAsec.txt",
#                    col_types = cols('T', 'i','i','i','i', 'i','i', 'i','i', 'c','c'))

problems(data.sec)
data.sec<- distinct(data.sec, dates)
# 5000 повторов
summary(data.sec)
data.sec <-na.omit(data.sec)
data.sec <- arrange(data.sec, dates)

cat("Days of power up (unique): ", nrow(data.sec)/(60*60*24), '\n')
# Days of power up (unique):  69.50903 
cat("Hours of power up (unique): ", nrow(data.sec)/(60*60), '\n')
# Hours of power up (unique):  1668.217 
# names (data.sec)
summary(as.Date(data.sec$dates))
# plot(table(as.Date(data.sec$dates)))

# time correction ---------------------------------------------------------------

data.sec <- data.sec%>%
  mutate(dates.UTC = data.sec$dates  - 60*60*3 )

data.sec <- data.sec[,(-17:-22)]
# data.sec$dates <- data.sec$dates - 60*60*3
# 
# data.sec <- data.sec%>%
#   mutate(dates.correct.benghin =  data.sec$`YYYY-MM-DD hh:mm:ss-1s` - ceiling(
#     56.77315002 * (data.sec$`YYYY-MM-DD hh:mm:ss-1s` - as.POSIXct('2016-08-21 09:07:00'))/86400   + 60*60*3
#   ))

# константа постоянного ухода часов прибора
kt = (56.77315002) /86400
# kt=  (60) /86400
# вычитание постоянного ухода часов прибора
# data.sec <- data.sec %>%
#   mutate(dates.correct.benghin =  data.sec$dates.UTC - ceiling(
#     kt* (data.sec$dates.UTC - as.POSIXct('2016-05-10 19:17:00', 'GMT'))
#   ))

data.sec <- data.sec %>%
  mutate(dates.correct.benghin =  dates.UTC - ceiling(
    kt* (dates.UTC - min(dates.UTC))
  ))


# восстановление времени начала записи в файл
data.sec$timestamp.start <-gsub("depron-","0x",data.sec$filename)
data.sec$timestamp.start <-gsub(".dat","",data.sec$timestamp.start)
data.sec$timestamp.start <- as.POSIXct(as.integer(data.sec$timestamp.start), 
                                       origin="1970-01-01", 'GMT' )
# data.sec$timestamp.start <- data.sec$timestamp.start -60*60*3

# восстановление времни последней записи в файл
data.sec$timestamp.end <- as.POSIXct(strptime(data.sec$timestamp,format="%d.%m.%Y %H:%M"))


# получени разности между началом файла и горизонтальным приборным временем
data.sec$time.delta.file.start <- as.numeric(data.sec$dates.correct.benghin - data.sec$timestamp.start ,
                                             units = "secs") 


data.sec <- data.sec %>%
  group_by(filename) %>%
  distinct(filename) %>%
  summarise(delta.minimum = min(time.delta.file.start)) %>%
  left_join(data.sec, ., by = 'filename')

# table(data.sec$delta.minimum)
data.sec$time.correct.zolotarev <- data.sec$dates.correct.benghin - data.sec$delta.minimum 



# отбор перескоков времени в приборе более 120 с - меньшие значения возможны при нормальной работе, 
# большие только при отключениях питания
data.sec <-mutate(data.sec, lag.delta = delta.minimum - lag(delta.minimum))
table(data.sec$lag.delta)
data.sec.switches <-filter(data.sec, abs(lag.delta) >120)



data.sec <-  data.sec %>%
  mutate(switches = cut(data.sec$dates.UTC, 
                        breaks = c(min(data.sec$dates.UTC),
                                   data.sec.switches$dates.UTC,
                                   max(data.sec$dates.UTC) )))

  # xy1 <- xyplot( delta.minimum + switches ~ timestamp.start , data = data.sec,
  #                type = c("o","g"))
  # plot(xy1)


# plot(table(data.sec$delta.minimum))
# table(data.sec$delta.minimum)
# median(data.sec$delta.minimum)
# mfv(data.sec$delta.minimum)



# 
# if(nrow(data.sec.switches)>0){
# data.sec <-  data.sec %>%
#   group_by(switches)  %>%
#   mutate(med.delta =median(delta.minimum))
# }
# if(nrow(data.sec.switches)== 0){
# data.sec <-  data.sec %>%
#   mutate(med.delta =median(delta.minimum))
# }

# 
library('modeest')
if(nrow(data.sec.switches)>0){
  data.sec <-  data.sec %>%
    group_by(switches)  %>%
    mutate(mfv.delta =max(mfv(delta.minimum)))
}
if(nrow(data.sec.switches)== 0){
  data.sec <-  data.sec %>%
    mutate(mfv.delta =max(mfv(delta.minimum)))
}


data.sec <-  data.sec %>%
  mutate(dates.correct = dates.correct.benghin - mfv.delta)

# минус минута так как данные приходят по окончании минуты
data.sec$dates.correct <- as.POSIXct(data.sec$dates.correct, 'GMT') - 59

data.sec$dates.correct.copy <- as.POSIXct(data.sec$dates.correct, 'GMT')
# последняя проверка
# получениЕ разности между началом файла и правильным временем
data.sec$correct.time.delta.file.start <- as.numeric(data.sec$dates.correct - data.sec$timestamp.start ,
                                             units = "secs")
xy1 <- xyplot( correct.time.delta.file.start ~ data.sec$timestamp.start,
               data = data.sec, type = c("o","g"))
# plot(xy1)


setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
ppi <- 300
filename <- paste('depron_time_', n, '.png')
png(filename, width=20*ppi, height=12*ppi, res=ppi)
plot(xy1)

# --Ensure device finishes cleanly:
dev.off()


write.table(data.sec.switches, "switches.txt", sep='\t', 
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE)

cat('Depron Data loaded and time is clear for: ', nrow(data.sec), 'seconds\n')
return(data.sec)
}

# saving combined data-----------------------------------------------------
# день года - n с первого января
SaveDepronData <- function(n){
  # n<-151
  cat('Saving Depron Data for day: ', n)
  # это для первого прогона
  filename <- sprintf("y16_d%03s.tsv", n)
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
  
  data.nav <- read_tsv(filename)
  
  data.sec <- LoadDepron(n)
  LoadDepron(n+1)
  data.sec <- rbind(data.sec, LoadDepron(n+1))
  
  if (is.null(data.sec))
    return()
  
  if (is.null(data.nav))
    return()
  
  combined <- merge(data.nav, 
                    data.sec, 
                    by.x = 'dt',
                    by.y = 'dates.correct',
                    # all = FALSE,#natural join
                    all.x = TRUE)
    
  
  filename <- sprintf("y16_d%03sAsec.tsv", n)
  
  write_tsv(combined, filename , append = FALSE)

  
}







today <- as.difftime(as.Date(Sys.time()) - lomonosov.start , units = "days")

# последний запуск
# setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
# dir('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday',
#     pattern = "y16_d",  ignore.case = TRUE)

# for(n in 0:today)

# 299 дней на момент создания базы
# 
# start <- 299
#   
#   
# for(n in (289-131):today)
# {
#   
#   combined <- LoadNav(n)
#   
#   SaveNav(n, combined)
# }
# cat(memory.size())
# 


# start <-  strftime(lomonosov.start, format = "%j") 
# start<-215 problem
start<-215

today <- strftime(as.Date(Sys.time()), format = "%j") 
# today<- as.numeric(start) + 294

# 299 дней на момент создания базы
for(n in start:today)
{
  combined <- NULL
  
  SaveDepronData(n)
}

# перезауск возможно понадобится по причине неверного формата из с#
# Warning: 259 parsing failures.
# row col   expected     actual
# 60  -- 11 columns 21 columns
# 360  -- 11 columns 21 columns
# 660  -- 11 columns 21 columns
# 900  -- 11 columns 21 columns
# 1200  -- 11 columns 21 columns



# plot L B MLT -------------------------------------------------------------
PlotDepronData <- function(doy){
  # doy <-172
  
  # 2016-08-25 14:50:00 
  # 238
  #2016-08-21 11:40:00
  
  # doy <-238
  
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
  
  filename <- sprintf("y16_d%03sAsec.tsv", doy)
  if (!file.exists(filename))
    return()
  combined <- read_tsv(filename )

  
  # map
  n=20
  colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
   pie(rep(1,n), col=colr)
   
   legend_image <- as.raster(matrix(colr, ncol=1))
   plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Detector1 Count rate 1/s')
   text(x=1.5, y = seq(0,1,l=11), labels = ceiling(seq(0,10^(n/8),l=11)))
   rasterImage(legend_image, 0, 0, 1,1)
   
   
    # 2016-08-25 14:50:00 
   # time.start1 <-"2016-08-21 11:40:00"
   time.start1 <-"2016-08-25 14:50:00"
   combined.zoom<-combined[combined$dt >= as.POSIXct(time.start1 , 'GMT')- 60*10,]
   combined.zoom<-combined.zoom[combined.zoom$dt <= as.POSIXct(time.start1, 'GMT') ,]
   
   combined.zoom<-rev(combined.zoom)

   
  # m =max(combined$count1)
  
  ppi <- 300
  filename <- paste('depron_map_', doy, '.png')
  png(filename, width=5*ppi, height=5*ppi, res=ppi)
  
  plot(combined.zoom$longeo.center, combined.zoom$latgeo,
       main = paste("Depron counting rate in detector 1\n ",
                    min(combined.zoom$dt),
                    max(combined.zoom$dt), 
                    sep = " "),
       xlim = c(-180, 180),
       ylim = c(-90, 90),
       col = colr[ifelse(ceiling(8*log10(as.integer(combined.zoom$count1)+1))+1>n,
                         n,
                         ceiling(8*log10(as.integer(combined.zoom$count1)+1))+1)],
       xaxp  = c(-180, 180, 12),
       yaxp  = c(-90, 90, 6),
       asp=1
  )
  grid (12,7, lty = 6, col = "cornsilk2")
  map('world',
      xlim = c(-180, 180),
      ylim = c(-90, 90),
      col = 'grey',
      add=TRUE)
  
  # --Ensure device finishes cleanly:
  dev.off()
  
  
  
  
  
  ppi <- 300
  filename <- paste('depron_lat_map_', doy, '.png')
  png(filename, width=20*ppi, height=12*ppi, res=ppi)
  plot(combined$longeo.center, combined$count1,
       xlim = c(-180, 180),
       # log = 'y',
       col = colr[ifelse(ceiling(8*log10(as.integer(combined$count1)+1))+1>n,
                         n,
                         ceiling(8*log10(as.integer(combined$count1)+1))+1)]
  )
  grid (NULL,NULL, lty = 6, col = "cornsilk2")
  # --Ensure device finishes cleanly:
  dev.off()
  
  
  
  
  
  
  ppi <- 300
  filename <- paste('depron_lb_', doy, '.png')
  png(filename, width=20*ppi, height=12*ppi, res=ppi)
  plot(combined$l, combined$b,
       log = 'x',
       xlim = c(1, 1000),
       # ylim = c(-90, 90),
       col = colr[ifelse(ceiling(8*log10(as.integer(combined$count1)+1))+1>n,
                         n,
                         ceiling(8*log10(as.integer(combined$count1)+1))+1)]
       # xaxp  = c(0, 10, 10)
  )
  grid (NULL,NULL, lty = 10, col = "cornsilk2")
  # --Ensure device finishes cleanly:
  dev.off()
}

PlotDepronData3d <- function(doy){

  # doy<-146
  # 2016-08-25 14:50:00 
  # 238
  #2016-08-21 11:40:00
  

  
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
  
  filename <- sprintf("y16_d%03sAsec.tsv", doy)
  if (!file.exists(filename))
    return()
  combined <- read_tsv(filename )
  
  
  
  
  ##### OPTION 1: hexbin from package 'hexbin' 
  # Color housekeeping
  # library(RColorBrewer)
  # rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  # r <- rf(32)
  
  # library(hexbin)
  # Create hexbin object and plot
  
  # hexbinplot(b~l,
  #            filter(combined,combined$l>4 & combined$l<6 & combined$b>.2 & combined$b<.6),
  #            colramp=rf,
  #            aspect=1, 
  #            # trans=log,
  #            inv=exp,
  #            mincnt=1,
  #            xbins = 50)
  
  # 
  # 
  # 
  # hbin<-hexbin(log(combined$l),log(combined$b),xbins=25,IDs=TRUE)
  # hvp<-plot(hbin)
  # mtrans<-hexTapply(hbin,combined$count1,median,na.rm=TRUE)
  # pushHexport(hvp$plot.vp)
  # grid.hexagons(hbin,style='lattice',pen=0,border='red',use.count=FALSE,
  #               cell.at=mtrans)
  # 
  # ------------------------------------ 
  #   library(plotrix) 
  # library(hexbin) 
  # 
  # #creates a scale of colors 
  # myColorRamp <- function(colors, values) { 
  #   v <- (values - min(values))/diff(range(values)) 
  #   x <- colorRamp(colors)(v) 
  #   rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
  # } 
  # 
  # #generates data for three variables 
  # dat=data.frame( x = c(rep(1:10,3)), y = c(rep(1:10,3)), z = c(rep(1:10,3))) 
  # 
  # #generates hexbin with the x and y variables 
  # hbin<-hexbin(dat$x, dat$y, xbins=10, IDs=TRUE) 
  # 
  # #sums points falling inside bin 
  # SumHexBin<-data.frame(sums=hexTapply(hbin, dat$z, sum)) 
  # 
  # #do color scale based on values of points in a third variable 
  # cols <- myColorRamp(c("white","green","yellow", "red"), SumHexBin$sums) 
  # 
  # ## setup coordinate system of the plot 
  # P <- plot(hbin, type="n",legend=FALSE)# asp=1 
  # 
  # ##add hexagons (in the proper viewport): 
  # pushHexport(P$plot.vp) 
  # 
  # #plots hexbins based on colors of third column 
  # grid.hexagons(hbin, style= "lattice", border = gray(.9), pen = cols,  minarea = 1, maxarea = 1)
  # 
  # 
  #   df <-filter(combined,l>4 & l<6 & b>.2 & b<.6)
  #   plot(b~l,
  #        combined,
  #        log = 'x',
  #        # xlim = c(1, 1000),
  #        xlim = c(4, 6),
  #        ylim = c(.2, .6),
  #        col = colr[ifelse(ceiling(8*log10(as.integer(combined$count1)+1))+1>n,
  #                          n,
  #                          ceiling(8*log10(as.integer(combined$count1)+1))+1)]
  # 
  #   )
  
  # 
  # # Use text3D to label x axis
  # text3D(x = 1:nrow(counts), 
  #        y = rep(0.5, nrow(counts)), 
  #        z = rep(0, nrow(counts)),
  #        labels = rownames(counts),
  #        add = TRUE, adj = 0)
  # # Use text3D to label y axis
  # text3D(x = rep(1, 4),   y = 1:4, z = rep(0, 4),
  #        labels  = colnames(counts),
  #        add = TRUE, adj = 1)
  ## Save your images to files if you wish
  # rgl.snapshot(filename=(paste("hist",doy , ".png", sep = '')))
  
  
  
  
  # histograms
  
  breaksl <- seq(4, 6, length.out = 9)
  breaksb <- seq(.2, .6, length.out = 9)
  
  
  # combined<-combined[na.omit(combined$l),]
  # combined<-combined[na.omit(combined$b),]
  
  # if (!is.numeric(combined$b))
    # return()
  combined$binsl <- cut(as.numeric(combined$l), breaksl)
  combined$binsb <- cut(as.numeric(combined$b), breaksb)
  
  
  # 
  # 
  # 
  # 
  # hist.count1 <- combined %>%
  #   # filter(l>4, !is.na(binsb)) %>%
  #   group_by(., binsl, binsb) %>%
  #   summarise(., counts =n())
  # hist.count1 <- na.omit(hist.count1)
  # 
  # 
  # counts <- matrix( 
  #   NA, # the data elements 
  #   nrow=length(levels(combined$binsl)),              # number of rows 
  #   ncol=length(levels(combined$binsb)),              # number of columns 
  #   byrow = TRUE)        # fill matrix by rows
  # 
  # 
  # dimnames(counts) = list( 
  #   levels(combined$binsl),         # row names 
  #   levels(combined$binsb))         # column names 
  # 
  # 
  # for(i in levels(combined$binsl)){
  #   for(j in levels(combined$binsb)){
  #     # cat(i, j,'\t')
  #     counts[i,j] <-  as.numeric(hist.count1[(hist.count1$binsl==i)&(hist.count1$binsb == j),3])
  #     # print(as.numeric(hist.count1[(hist.count1$binsl==i)&(hist.count1$binsb == j),3]))
  #   }
  # }
  # 
  
  
  # table(combined$count1)
  
  hist.mean <- combined %>%
    # filter( !is.na(binsb)) %>%
    group_by(., binsl, binsb) %>%
    summarise(., mean =mean(na.omit(as.numeric(count1))))
  hist.mean <- na.omit(hist.mean)
  print(hist.mean)
  
  
  
  library(lattice)
  # ckey <- list(labels=list(cex=2))
  n=20
  colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
  # pie(rep(1,n), col=colr)
  # 
  # levelplot(mean ~ binsl * binsb, data = hist.mean,
  #           xlab = 'L', ylab = 'B',
  #           col.regions = level.colors(hist.mean$mean, 
  #                                      at = do.breaks(range(hist.mean$mean), 20),
  #                                    col.regions = colr,
  #                                    colors = TRUE),
  #           colorkey = list(col = colr, at = do.breaks(range(hist.mean$mean), 20)))
  #           # scales=list(x=list(at=seq(6,15,1), cex=2), y=list(at=seq(0,0.9,0.1), cex=2)),
  #           # xlab=list(label="x", cex=2),
  #           # ylab=list(label="y", cex=2), 
  #           
  
  
  # df <-filter(combined,l>4 & l<6 & b>.2 & b<.6)
  # plot(b~l,
  #      combined,
  #      log = 'x',
  #      # xlim = c(1, 1000),
  #      xlim = c(4, 6),
  #      ylim = c(.2, .6),
  #      col = colr[ifelse(ceiling(8*log10(as.integer(combined$count1)+1))+1>n,
  #                        n,
  #                        ceiling(8*log10(as.integer(combined$count1)+1))+1)]
  #      
  # )
  
  
  # 
  # sum <- matrix( 
  #   NA, # the data elements 
  #   nrow=length(levels(combined$binsl)),              # number of rows 
  #   ncol=length(levels(combined$binsb)),              # number of columns 
  #   byrow = TRUE)        # fill matrix by rows
  # 
  # 
  # dimnames(sum) = list( 
  #   levels(combined$binsl),         # row names 
  #   levels(combined$binsb))         # column names 
  # 
  # 
  # for(i in levels(combined$binsl)){
  #   for(j in levels(combined$binsb)){
  #     # cat(i, j,'\t')
  #     sum[i,j] <-  as.numeric(hist.mean[(hist.mean$binsl==i)&(hist.mean$binsb == j),3])
  #     # print(as.numeric(hist.mean[(hist.mean$binsl==i)&(hist.mean$binsb == j),3]))
  #   }
  # }
  # 
  # 
  
  
  library(latticeExtra)
  
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
  ppi <- 600
  filename <- paste0('norm_depron_lb3d_polar',doy,'.png')
  png(filename, width=8*ppi, height=8*ppi, res=ppi)
  
  print(cloud(mean ~ binsl * binsb, data = hist.mean, panel.3d.cloud=panel.3dbars,
        # col.facet='grey',
        xbase=0.4, ybase=0.4,
        scales=list(arrows=FALSE, col=1), 
        xlab = 'L', ylab = 'B',
        zlim = c(0,2500),
        # col.facet = level.colors(hist.mean$mean, at = do.breaks(range(hist.mean$mean), 20),
        #                          col.regions = colr,
        #                          colors = TRUE),
        col.facet = level.colors(hist.mean$mean, at = do.breaks(c(0, 2500), 20),
                                 col.regions = colr,
                                 colors = TRUE),
        colorkey = list(col = colr, at = do.breaks(c(0, 2500), 20)),
        screen = list(z = 40, x = -30),
        par.settings = list(axis.line = list(col = "transparent")))
  )
  
  # --Ensure device finishes cleanly:
  dev.off()
  
  
  
  # 
  # # 
  # library("car", "rgl")
  # # demo(hist3d)
  # library(plot3D) 
  # library(plot3Drgl)
  # # rgl.clear()
  # #  hist3D and ribbon3D with greyish background, rotated, rescaled,...
  # 
  # hist3Drgl(z = sum,
  #           x =  breaksl[-1],
  #           y =  breaksb[-1],
  #           # x = as.numeric(rownames(sum)),
  #           # y = as.numeric(colnames(sum)),
  #           xlab = 'L',#l
  #           ylab = 'B',#b
  #           zlab = 'mean(count1)',
  #           main = paste('Day: ', doy),
  #           # scale = FALSE,
  #           expand = 0.5, 
  #           bty = "g", 
  #           alpha = 0.8,
  #           border = "black",
  #           shade = 0.2,
  #           theta = 60, phi = 20,
  #           # ltheta = 180,
  #           space = 0.3, ticktype = "detailed", d = 2
  # )
  # rgl.snapshot(paste("3dbarplot27_",doy,".png"), fmt = "png", top = TRUE )
  
  
  
  breaksl <- seq(1, 1.6, length.out = 10)
  breaksb <- seq(.15, .25, length.out = 10)
  combined$binsl <- cut(as.numeric(combined$l), breaksl)
  combined$binsb <- cut(as.numeric(combined$b), breaksb)
  
  hist.mean <- combined %>%
    # filter( !is.na(binsb)) %>%
    group_by(., binsl, binsb) %>%
    summarise(., mean =mean(na.omit(as.numeric(count1))))
  hist.mean <- na.omit(hist.mean)
  print(hist.mean)
  
  library(latticeExtra)
  
  setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
  ppi <- 600
  filename <- paste0('norm_depron_lb3d_anom',doy,'.png')
  png(filename, width=8*ppi, height=8*ppi, res=ppi)
  
  print(cloud(mean ~ binsl * binsb, data = hist.mean,
              # panel.3d.cloud=panel.3dbars,
              # col.facet='grey',
              xbase=0.4, ybase=0.4,
              scales=list(arrows=FALSE, col=1), 
              xlab = 'L', ylab = 'B',
              zlim = c(0,2500),
              # col.facet = level.colors(hist.mean$mean, at = do.breaks(range(hist.mean$mean), 20),
              #                          col.regions = colr,
              #                          colors = TRUE),
              col.facet = level.colors(hist.mean$mean, at = do.breaks(c(0, 2500), 20),
                                       col.regions = colr,
                                       colors = TRUE),
              colorkey = list(col = colr, at = do.breaks(c(0, 2500), 20)),
              screen = list(z = 40, x = -30),
              par.settings = list(axis.line = list(col = "transparent")))
  )
  
  # --Ensure device finishes cleanly:
  dev.off()
}

# plot cicle --------------------------------------------------------------


start <-  as.numeric(strftime(lomonosov.start, format = "%j") )


today <- as.numeric(strftime(as.Date(Sys.time()), format = "%j") )
# today<- as.numeric(start) + 294

# 299 дней на момент создания базы
for(n in 146:today)
{
  # PlotDepronData(n)
  PlotDepronData3d(n)
  
}


# calibration -------------------------------------------------------------

# Детектор1: 3,4141 КэВ/канал 
# Детектор2: 4 КэВ/канал - очень большой разброс по калибровкам - не могу понять почему:15.4 – 16.3 КэВ/канал.
# в одной еденице по дозе детектора1 135 счетных имульсов по самому детектору 1,
# получается что умножив 135 на 3,4141  КэВ/канал  получаем 3687 КэВ/(дозовый канал) 

# Детектор квадратный, 10мм*10мм, 300 мкм, подбор значений толщины тормозящего слоя 
# вещества дает 0.0013 г/см^2, оно соответствует 5.58 мкм кремния или 1.04 см воздуха.
# масса детектора, полученная ан3алогично оценкам сиволапова: 6,87г
# Дет1: 8,69пГр/кодАЦП, 
k1 <-8.69 * 10^-12    *8      *60*60
#    k      power    left shift  hour
# Дет2: 9,32пГр/кодАЦП, 
k2 <-9.32 * 10^-12    *8      *60*60
# Далее можно посчитать на воду а также учесть разные геомфакторы для всех типов данных

combined$dose1Gr <-combined$dose1 *k1
combined$dose2Gr <-combined$dose2 *k2
# geom factor --------------------------------------------------------------


geom.factor.both = 0.145 * 4*pi
geom.factor.1 =(1/2)*4*pi
geom.factor.2 =(1/2)*4*pi

combined$count.both.geom = combined$count.both / geom.factor.both
combined$count1.geom = combined$count1 / geom.factor.1
combined$count2.geom = combined$count2 / geom.factor.2

# 


# plotting prepare ----------------------------------------------------------------


require(lattice)

#--The following was used to define the colours, but since it's a non-standard
#   package, I've used the hexadecimal colour codes explicity below to avoid the
#   need to install "RColorBrewer". See http://colorbrewer2.org/ for more info.
#require(RColorBrewer)
#colset <- brewer.pal(5, "Set1")

#--Change output device to pdf file:
# trellis.device(device="pdf", file="DEPRON_sec.pdf", height=11, 
#                width=7.5, color=TRUE)

#--Define plot titles:
lab.det1h <- expression(paste('Detector 1 hardware count rate (',s^-1*cm^-2*sr^-1,')',sep=''))
lab.det1 <- expression(paste('Detector 1 count rate (',s^-1*cm^-2*sr^-1,')',sep=''))
lab.det2 <- expression(paste('Detector 2 count rate (',s^-1*cm^-2*sr^-1,')',sep=''))
lab.coins <- expression(paste('Coincedense count rate (',s^-1*cm^-2*sr^-1,')',sep=''))
lab.dose1 <- expression(paste('Dose rate detector 1 ','mGr/h',sep=''))
lab.dose2 <- expression(paste('Dose rate detector 2 ','mGr/h',sep=''))
lab.l <- expression(paste('L ','',sep=''))
main = paste("Depron Dose and counting rates in seconds arrays\n ",
             min(combined.zoom$dt),
             # max(combined.zoom$dt), 
             sep = " ")


#--Custom strip function:
# (NB the colour used is the default lattice strip background colour)
my.strip <- function(which.given, which.panel, ...) {
  # strip.labels <- c(lab.l, lab.dose1,lab.coins, lab.det2, lab.det1  )
  strip.labels <- c(lab.l, lab.dose1, lab.coins, lab.det2, lab.det1  )
  panel.rect(0, 0, 1, 1, col="#ffe5cc", border=1)
  panel.text(x=0.5, y=0.5, adj=c(0.5, 0.55), cex=0.95,
             lab=strip.labels[which.panel[which.given]])
}

# Define X axis date range

xlim <- range(combined.zoom$dt)
if(nrow(combined.zoom)>3000){
  typeline <- "l"}else {
  typeline <- "p"
}
# xlim <- c(as.POSIXct("2016-08-13 23:40"), as.POSIXct("2016-08-14 00:20"))


#--Define annual quarters for plot grid line markers:
# d <- seq(from=as.Date(min(combined.zoom$dt)), to=as.Date(max(combined.zoom$dt)), by=1)
d <- seq(from=(min(combined.zoom$dt)), to=(max(combined.zoom$dt)), by=60)
#--Define colours for raw & smoothed data:
col.raw <- "#377EB8"	#colset[2] } see note above
col.smo <- "#E41A1C"	#colset[1] }
col.lm <- "grey20"

# Create multipanel plot --------------------------------------------------


# print(xyplot(count.h + count1 + count2 + count.both + dose1Gr~ dates, data=combined.zoom,
xy2 = xyplot(l +  dose1Gr*60*60 + count.both.geom + count2.geom+ count1.geom~ dt, data=combined.zoom,
             
             # xy2 = xyplot(l +  1000*dose1Gr + count.both + count2+ count1~ dates, data=combined.zoom,
             scales=list(y = list(log = 10, relation= "free", tick.number = 3), rot=0),
             # scales=list(y = list(log = T, relation= "free",tick.number = 3), rot=0),
             # scales=list(y="free", rot=0),
             xlim=xlim, 
             # main=main,
             strip=my.strip, outer=TRUE, layout=c(1, 5, 1), xlab="Time", ylab="",
             panel=function(x, y, ...) {panel.grid(h=-1, v=0)	# plot default horizontal gridlines
               panel.abline(v=d, col="grey90") # custom vertical gridlines
               # panel.abline(col="yellow", v=d1range)
               
               # panel.abline(col="yellow", v=d1,  lwd=67)
               # panel.abline(v=d1, col="red") # custom vertical gridlines
               
               panel.xyplot(x, y, ..., type=typeline, col=col.raw, pch = 20) # raw data
               
               # panel.xyplot(x, y, ..., type="p", col=col.raw, lwd=1.5) # raw data
               # panel.loess(x, y, ..., col=col.smo, span=0.1, lwd=0.5) # smoothed data
               panel.abline(h=median(y, na.rm=TRUE), lty=2, col=col.lm, lwd=1) # median value
             },
             key=list(text=list(c("raw data",  "median value")),#"smoothed curve",
                      title=main,
                      col=c(col.raw,  col.lm), #
                      lty=c(1,  2),
                      columns=2, cex=0.95,
                      lines=TRUE
             )
             
)


plot(xy2)

# save multipanel to file ------------------------------------------------------------
Sys.setlocale("LC_ALL","English")
# Sys.setlocale("LC_ALL","Russian")
setwd('d:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday')
ppi <- 600
filename <- paste('depron_sec_log',format(min(combined.zoom$dt), "%m-%d-%y %H-%M-%S"),'.png')
png(filename, width=8*ppi, height=8*ppi, res=ppi)

plot(xy2)

# --Ensure device finishes cleanly:
dev.off()


pdf(filename)

plot(xy2)

# --Ensure device finishes cleanly:
dev.off()


