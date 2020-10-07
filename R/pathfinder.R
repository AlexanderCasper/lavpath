pathfinder <- function(model, xfactor, yfactor,
                       estplace = 0, distance = 0.2, aheadlength = 0.2, aheadangle = 20,
                       box1="black",  box2="lightgrey", textcol = "black", acol = "black", estcol ="black",
                       alength = 0.6, lwd=2,  cex=15, cex2=1, family="", font=2,
                       font2 =1, font3 = 1, pch=22)
{

  neu <-  dplyr::select(dplyr::filter(lavaan::parameterEstimates(model, standardized=TRUE),op == "~"),'Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, pvalue=pvalue, Beta=std.all)
  ## parameter runden
  neu[3:7] <- sapply(neu[3:7], round, 3)

  neu2 <- neu[2:7]
  neu$Indicator <- NULL
  names(neu)[1] <- "Indicator"
  neu3 <- rbind(neu,neu2) #Oben LV unten Indicator
  names(neu3)[1] <- "variables"
  neu3$variables <- as.factor(neu3$variables)
  levels(neu3$variables)
  neu3$y <- neu3$variables
  neu3$x <- neu3$variables
  levels(neu3$x) <- xfactor #an dieser stelle kommen die argumente ins Spiel
  levels(neu3$y) <- yfactor #an dieser stelle kommen die argumente ins Spiel
  neu <- neu3[1:(nrow(neu3)/2),]
  neu2 <- neu3[(nrow(neu3)/2+1):nrow(neu3),]
  names(neu)[1] <- "LV"
  names(neu)[7:8] <- c("y1","x1")
  names(neu)
  neu$I <- neu2$variables
  neu$y0 <- neu2$y
  neu$x0 <- neu2$x
  neu$y0 <- as.numeric(as.character(neu$y0))
  neu$y1 <- as.numeric(as.character(neu$y1))
  neu$x0 <- as.numeric(as.character(neu$x0))
  neu$x1 <- as.numeric(as.character(neu$x1))


  ############################################### 1.3 Steigung,Winkel und Achsenabschnitt ###############################################
  neu$m <- (neu$y1 - neu$y0)/(neu$x1-neu$x0)
  for (x in 1:nrow(neu)) {
    i <- x
    if (neu$m[i] == -Inf){neu$m[i] <- -99999999999999999999999999999999999999999}
    if (neu$m[i] == Inf){neu$m[i] <- 99999999999999999999999999999999999999999}
  }
  neu$s <- atan((neu$y1-neu$y0)/(neu$x1-neu$x0))/1.570796*90
  # 1.57 irgendwie die maximale steigung in r, warum auch immer
  neu$b <- -(neu$x0*neu$m-neu$y0)

  neu$sig<-cut(neu$pvalue , c(0,0.001,0.01,0.05,1), right=FALSE, labels=c("***","**","*","(n.s.)"))

  for (i in 1:nrow(neu)) {
    neu$full[i] <- paste(" ",neu$B[i]," | (",neu$Beta[i],") ",as.character(neu$sig[i]), sep="")
  }
  for (i in 1:nrow(neu)) {
    neu$beta2[i] <- paste("(",neu$Beta[i],")",as.character(neu$sig[i]), sep="")
  }


  ############################################### 1.6 R2 ##############################################################################################
  r <- lavaan::inspect(model, 'r2')
  r <- as.data.frame(r)
  r$names <- rownames(r)
  for (i in 1:nrow(neu)) {
    neu$r[i] <- r[which(neu$LV[i] == r$names),"r"]
  }
  neu$r <- round(neu$r,3)
  for (i in 1:nrow(neu)) {
    neu$rfull[i] <- paste("R2: ",neu$r[i], sep="")
  }
  for (i in 1:nrow(neu)) {
    if (sum(table(neu$I[i] == r$names)["TRUE"],na.rm = TRUE) >=1) {
      neu$rI[i] <- r[which(neu$I[i] == r$names),"r"]
    }
    else {
      neu$rI[i] <- NA
    }

  }
  neu$r <- round(neu$rI,3)
  for (i in 1:nrow(neu)) {
    if (sum(table(neu$I[i] == r$names)["TRUE"],na.rm = TRUE) >=1){
      neu$rIfull[i] <- paste("R2: ",neu$r[i], sep="")}
    else {
      neu$rIfull[i] <- paste(" ", sep="")
    }
  }

  # 2. Visualisierung ####
  ############################################### 2.1 Leerer Plot
  plot(1,1, type="n",cex=10,pch=19,xlim= c(0,10), ylim = c(0,10),xaxt='n',yaxt='n',
       ylab="",xlab = "")

  for (x in c(1:nrow(neu))) {
    i<- x
    lty <- 1
    if (neu$pvalue[i] > 0.05) {lty <- 4}


    #  a) Pfeil nach rechts & kleiner +45? & gR2?er -45?
    if(neu$x0[i] < neu$x1[i] & neu$s[i] < 45 & neu$s[i] > -45){
      arrows(neu$x0[i]+alength, (neu$x0[i]+alength)*neu$m[i]+neu$b[i], neu$x1[i]-alength, (neu$x1[i]-alength)*neu$m[i]+neu$b[i],lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+estplace-distance*( neu$s[i]/90), y=(neu$y0[i]+neu$y1[i])/2+distance*(1-(sqrt(neu$s[i]^2)/90))+   estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2
            ,col = estcol)
    }

    #   b) Pfeil nach links
    if(neu$x0[i] > neu$x1[i] & neu$s[i] < 45 & neu$s[i] > -45) {
      arrows(neu$x0[i]-alength, (neu$x0[i]-alength)*neu$m[i]+neu$b[i], neu$x1[i]+alength, (neu$x1[i]+alength)*neu$m[i]+neu$b[i], lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+distance*(neu$s[i]/90)-estplace, y=(neu$y0[i]+neu$y1[i])/2-distance*(1-(sqrt(neu$s[i]^2)/90))-  estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }

    #  c) Pfeil nach unten
    if ( round(neu$s[i]) == -90 )  {
      arrows(neu$x0[i] ,(neu$y0[i])-alength,neu$x1[i] ,(neu$y1[i])+alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+distance, y=(neu$y0[i]+neu$y1[i])/2- estplace
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }

    #  d) Pfeil nach oben
    if (round(neu$s[i]) == 90){
      arrows(neu$x0[i] ,(neu$y0[i])+alength,neu$x1[i] ,(neu$y1[i])-alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2-distance, y=(neu$y0[i]+neu$y1[i])/2+ estplace
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }





    # Pfeil nach rechts oben steil
    if(neu$x0[i] < neu$x1[i] & neu$s[i] >= 45){
      arrows(((neu$y0[i]+alength)-neu$b[i])/neu$m[i] , neu$y0[i]+alength, ((neu$y1[i]-alength)-neu$b[i])/neu$m[i] , neu$y1[i]-alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+estplace-distance*( neu$s[i]/90), y=(neu$y0[i]+neu$y1[i])/2+distance*(1-(sqrt(neu$s[i]^2)/90))+   estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }

    # Pfeil nach rechts unten steil
    if(neu$x0[i] < neu$x1[i] & neu$s[i] <= -45){
      arrows(((neu$y0[i]-alength)-neu$b[i])/neu$m[i] , neu$y0[i]-alength, ((neu$y1[i]+alength)-neu$b[i])/neu$m[i] , neu$y1[i]+alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+estplace-distance*( neu$s[i]/90), y=(neu$y0[i]+neu$y1[i])/2+distance*(1-(sqrt(neu$s[i]^2)/90))+   estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }


    # Pfeil nach links  steil 1
    if(neu$x0[i] > neu$x1[i] & neu$s[i] >= 45  ){
      arrows(((neu$y0[i]-alength)-neu$b[i])/neu$m[i] , neu$y0[i]-alength, ((neu$y1[i]+alength)-neu$b[i])/neu$m[i] , neu$y1[i]+alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+estplace-distance*( neu$s[i]/90), y=(neu$y0[i]+neu$y1[i])/2+distance*(1-(sqrt(neu$s[i]^2)/90))+   estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }

    # Pfeil nach links steil 2
    if(neu$x0[i] > neu$x1[i] & neu$s[i] <= -45  ){
      arrows(((neu$y0[i]+alength)-neu$b[i])/neu$m[i] , neu$y0[i]+alength, ((neu$y1[i]-alength)-neu$b[i])/neu$m[i] , neu$y1[i]-alength,lwd=lwd, lty=lty,length = aheadlength , angle = aheadangle, col = acol)
      text( x=(neu$x0[i]+neu$x1[i])/2+estplace-distance*( neu$s[i]/90), y=(neu$y0[i]+neu$y1[i])/2+distance*(1-(sqrt(neu$s[i]^2)/90))+   estplace * neu$m[i]
            ,label= neu[i,"full"]
            ,srt= neu$s[i],family=family
            ,font = font3,cex=cex2,col = estcol)
    }








    ############################################### 2.2 Textboxen
    # a) text I
    points(x= neu$x0[i], y=neu$y0[i],pch=pch, col= box1, bg = box2,cex=cex)
    text(x= neu$x0[i], y=neu$y0[i], label= neu$I[i],cex=cex2,family=family ,font = font, col = textcol)
    text(x= neu$x0[i], y=neu$y0[i]-0.3, label= neu$rIfull[i],cex=cex2,family=family,font = font2, col = textcol)
    # b) text LV
    points(x= neu$x1[i], y=neu$y1[i],pch=pch, col= box1, bg = box2,cex=cex)
    text(x= neu$x1[i], y=neu$y1[i], label= neu$LV[i],cex=cex2,family=family,font = font, col = textcol)
    text(x= neu$x1[i], y=neu$y1[i]-0.3, label= neu$rfull[i],cex=cex2,family=family,font = font2, col = textcol)

    # Closing for
  }

  # 3. Kommentar ####
  cat(crayon::blue("
                   In this format it doesn't looks great. It has to be 1:1.
                   when you export the plot you should do it in width: 1000 & height: 1000

                   "))



}
