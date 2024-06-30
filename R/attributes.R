#get attributes

attributes <- function(file, oth = .001, ourLabels=NULL, useLabels=FALSE,...){
 p <- list(...)
  egts <- FALSE
  if("deleteStart" %in% p){
    deleteStart <- TRUE

   # print("deleteStart true")
  }else{
    deleteStart<- FALSE
  }

  if("median" %in% p){
    median<-FALSE
    #print("median false")
  }else{
    median<-TRUE
  }
  if("minse" %in% p){
    minse<-FALSE
   # print("minse false")
  }else{
    minse <- TRUE
  }
  if("trendAll" %in% p){
    trendAll<-FALSE
   # print("trendall false")
  }else{
    trendAll<-TRUE
  }
  if("keepDiscrete" %in% p){
    keepDiscrete <-FALSE
    #print("keepdiscrete false")
  }else{
    keepDiscrete <-TRUE
  }
  if(useLabels==TRUE){
    data <- file
  }else{
  data <- utils::read.csv(paste0(file,".csv"))

  }




  useMinFLengths <- T
  #disThresh <- 10
  disPer <- 5
  minFLength <- 3

  slopeDifference <- 15
  vLengths <- c()
  #remove Constants inside feature vector (saves length of equilibrium at end for comparison)
  removeCon <- T # does not work with F - breaks the classification: to be fixed!

  joinVectors <- function(r, q, w){

    ri <- qj <- wh <- 1
    y <- c()

    for(i in 1:length(r)){
      if(qj > length(q)| wh > length(w)){

        return(y)
      }else{
        T1 <- (r[ri] == q[qj])
        T2 <- (q[qj] == w[wh])
        T3 <- (r[ri] == w[wh])

        if( T1 && T2){
          y <- c(y, r[ri])
          ri <- ri + 1
          qj <- qj + 1
          wh <- wh + 1
        }else if(T1){

          ri <- ri+1
          qj <- qj+1
        }else if(T2){

          ri <- ri + 1
        }else if(T3){

          ri <- ri + 1
          wh <- wh + 1
        }else{

          ri <- ri + 1
        }
      }
    }

    return(y)
  }
print("getting behaviour modes of data")
  getFeatures <- function(vector){
    featureVector <- c()
    featureVector <- lapply(vector, function(l){
      if(is.na(l)){
        featureVector=5
      }else if(l > oth){
        featureVector = 1
      }else if(l < -oth){
        featureVector = -1
      }
      else{
        featureVector=0
      }
    })
    featureVector  <- unlist(featureVector)
  }

  diffDF <- as.data.frame(apply(data, 2, diff))

  slopeFeatures <- as.data.frame(apply(diffDF, 2, getFeatures))


  featureL <- list()
  for(i in 1:ncol(slopeFeatures)){
    featureL[[i]] <- slopeFeatures[,i]
  }
  fLengths <- list()
  k <- list()
print("compressing feature vectors")
  for(j in 1:length(featureL)){
    o <- c()
    count <- 1
    diffDFCompressed <- data.frame()
    count <- 1
    l=1
    diffDFCompressed[l,1] = featureL[[j]][1]
    for(i in 2:length(featureL[[j]])){

      #if missing values, exit
      if(is.na(featureL[[j]][i]) ){
        break
      }
      #if any part of the feature vector is not the same as the previous, add it to the dataframe
      if(featureL[[j]][i]!=featureL[[j]][i-1] ){
        l=l+1
        o <- c(o, count)
        count <- 1
        diffDFCompressed[l,1] = featureL[[j]][i]

      }else{
        count <- count+1
      }
    }
    o <- c(o, count)
    fLengths[[j]]=o
    k[[j]] <- diffDFCompressed
    vLengths <- c(vLengths,nrow(diffDFCompressed))
  }

  fLengths2 <- fLengths #*
  fLengthsOriginal <- fLengths
  kOriginal <- k
  kTemp <- k #*
  if(useMinFLengths == T){

    c<-0
    n <- lapply(k, function(a){c<<-c+1;temp <-which(fLengths[[c]] < minFLength); if(length(temp) > 0){fLengths[[c]]<<- fLengths[[c]][-temp];n<- a[-temp,]}else{n <- a}})
    k <- n
    c <-0
    m <- lapply(k, function(a){c<<-c+1;a <- unlist(a);a <-as.numeric(a);i <- 1;while(i < length(a)){if(a[i+1]!=a[i]){i <- i+1}else{a<- a[-i];fLengths[[c]] <<- fLengths[[c]][-i];n[[c]]<<-a;}}})

    k <- n

  }

  if(removeCon == T){

    c <- 0

    reachEq <- c()
    n <- lapply(k, function(a){c<<-c+1;a<- unlist(a);a<- as.numeric(a);if(a[length(a)]==0){temp <-which(a[-length(a)] == 0);reachEq <- c(reachEq, c)}else{temp <- which(a[1:length(a)]==0)};if(length(temp) > 0){fLengths[[c]]<<- fLengths[[c]][-temp];n<- a[-temp]}else{n <- a}})
    k<- n
    c <-0
    m <- lapply(k, function(a){c<<-c+1;a <- unlist(a);a <-as.numeric(a);i <- 1;while(i < length(a)){if(a[i+1]!=a[i]){i <- i+1}else{a<- a[-i];fLengths[[c]] <<- fLengths[[c]][-i];n[[c]]<<-a}}})

    k <- n

  }
  if(keepDiscrete == F){

    for(i in 1:length(kTemp)){

      kTempI <- unname(unlist(kTemp[[i]]))
      if(length(kTempI) > 1){
        count <- 1
        tempR <- c()
        r<-0
        m<-1
        s <- 1
        invisible(lapply(fLengths2[[i]], function(e){

          disPerAc <- ((max(data[,i])-min(data[,i]))/100)*disPer

          w<- e+r

          # try( Vt <- k[[i]][s,],
          Vt <- kTempI[s]

          Vt2 <- kTempI[s]



          if(Vt!=0){

            # if(i == 14){
            if(abs(data[w,i] - data[m, i]) < disPerAc){



              tempR <<- c(tempR,s)

            }
            #}
          }
          #if(i==9){break}
          s <<- s+1
          m <<- m+e;
          r <<- m-1

        }))

        if(!is.null(tempR)){
          kTemp[[i]] <- kTempI[-tempR]

        }else{
          kTemp[[i]] <- kTempI
        }
        #count <<- count+1


        #})
      }
    }
  }

  kNew <- list()

  for(i in 1:length(kOriginal)){

    checkK <- joinVectors(unname(unlist(kOriginal[[i]])), k[[i]], unname(unlist(kTemp[[i]])))


    kNew[[i]] <- checkK
  }

  k <- kNew
print("creating attributes")
  lengths <- unlist(lapply(k, length))

   ClassData <- data.frame("Lengths"=lengths)

  count<- 1
  ClassData$tail <- unlist(lapply(k, function(a){a[length(a)]==0}))

  ClassData$trend <- FALSE
  ClassData$RangeFV <- FALSE
  # ClassData$MeanFV <- FALSE

  data <- apply(data,2,as.numeric)
  for(a in 1:length(k)){
    times <- 1:nrow(data)
    #try(tk <- nrow(k[[a]]),tk <- length(k[[a]]))
    count <- 0
    range <- max(data[,a])-min(data[,a])

    fv <- stats::lm(data[,a]~times)$fitted.values

    if(trendAll==FALSE){
      if(ClassData$Lengths[a] > 4){
        if(ClassData$tail[a] == FALSE){

          changeVector <- which(diff(sign(diff(data[,a])))!=0)

          x <- data[changeVector[1]:changeVector[length(changeVector)],a]

          t <- 1:length(x)
          fv2 <- stats::lm(x~t)$fitted.values


          fvSlope <- fv2[length(fv2)] - fv2[1] #max(fv)-min(fv)#get actual slope....?

          fvMeanSlope <- mean(diff(fv2))

          fvRange <- (range/100)*slopeDifference

          if(abs(fvSlope) > abs(fvRange)){
            ClassData$RangeFV[a] <- TRUE
          }
          if(abs(fvMeanSlope) > abs(fvRange)){
            #  ClassData$MeanFV[a] <- TRUE
          }
        }

        if(fv[length(fv)] > fv[1]){
          ClassData$trend[a] <- TRUE

        }

      }

    }else{
      if(ClassData$Lengths[a] > 4){
        if(ClassData$tail[a] == FALSE){

          changeVector <- which(diff(sign(diff(data[,a])))!=0)

          x <- data[changeVector[1]:changeVector[length(changeVector)],a]

          t <- 1:length(x)
          fv2 <- stats::lm(x~t)$fitted.values



          fvSlope <- fv2[length(fv2)] - fv2[1]#max(fv)-min(fv)# #get actual slope....?

          fvMeanSlope <- mean(diff(fv2))
          fvRange <- (range/100)*slopeDifference

          if(abs(fvSlope) > abs(fvRange)){
            ClassData$RangeFV[a] <- TRUE
          }
          if(abs(fvMeanSlope) > abs(fvRange)){
            #  ClassData$MeanFV[a] <- TRUE
          }
        }
      }
      if(fv[length(fv)] > fv[1]){
        ClassData$trend[a] <- TRUE
      }

    }
  }


  times <- 1:nrow(data)
  if(egts==TRUE){
    ClassData$EgtS <- apply(data, 2, function(a){a[length(a)] > a[1]})
  }

  if(median==TRUE){
    ClassData$median <- apply(data, 2,function(a){median(a) > a[1]})
  }
  if(minse==TRUE){
    if(deleteStart==TRUE){
      ClassData$minSE <- apply(data, 2, function(a){((a[1] == min(a)) || (a[length(a)] == min(a)))})

      for(i in 1:ncol(data)){


        if((as.numeric(unlist(kOriginal[[i]])[1])) != (unlist(k[[i]][1]))){

          tempCol <- data[,i]
          tempChange <- which(diff(sign(diff(tempCol)))!=0)
          if(length(tempChange)>0){

            change <- tempChange[1]

            a <- tempCol[-c(1:change)]#should this even be an option?????
            #ws going to use fLengthsOriginal but small changes classed as 0 and still decreasing.....
            ClassData$minSE[i] <- (a[1] == min(a)) || (a[length(a)] == min(a))

          }
        }
      }
    }else{
      ClassData$minSE <- apply(data, 2, function(a){((a[1] == min(a)) || (a[length(a)] == min(a)))})

    }
  }


  if(useLabels==TRUE){
    ClassData$classes <- ourLabels
  }
  return(ClassData)



}
