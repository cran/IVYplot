#' @title IVY Plot
#' @description The function will draw an IVY Plot (similar to Dot Plot) with/without frequencies
#' @param data0 The data vector the user will input
#' @param showFreq Option for the user to show the frequencies at each value or not. TRUE = show/FALSE = do not show.
#'                 Default is TRUE
#' @param freqSize The font size of the frequencies if the user wants to show the frequencies. Default is 1.0
#' @param multiple The maximum number of observations each leaflet represents. Default is calculated to ensure at most 20 leaves
#'                 at each value
#' @param delta The gap between successive values. Default is 1
#' @param limA The lower limit on the horizontal axis. Default is minimum of the values
#' @param limB The upper limit on the horizontal axis. Default is maximum of the values
#'
#' @return Gives you an IVY plot
#' @export
#' @importFrom grDevices dev.new
#' @importFrom graphics axis frame lines plot.window text title
#' @importFrom plyr count
#' @examples
#'   IVYplot(data0 = c(rpois(500, 10), 30, 30, 30), freqSize = 1.5, multiple = 3)


IVYplot <- function(data0, showFreq = TRUE, freqSize = 1.0, multiple = NULL, delta = 1, limA = NULL, limB = NULL){
  dev.new(width = 12, height = 10)
  dataCount <- count(data0)
  dataName <- deparse(substitute(data0))
  dataTable <- table(data0)
  if (length(dataCount$x) > 100){
    data0 <- round(data0/delta, 0) * delta
    dataCount <- count(data0)
  }

  data1 <- 0
  if (is.null(multiple) == TRUE)
    multiple <- ceiling(max(dataCount$freq)/100)
  for (i in 1:length(dataCount$freq)){
    data1 <- ceiling(dataCount$freq/multiple)
  }
  maxFreq <- max(data1)
  z <- 1
  if (maxFreq < 20)
    maxFreq <- 60
  delta <- (10^(-2))/2
  m <- round((max(data0) - min(data0))/(2 * delta), 0)
  mid <- min(data0) + 2 * delta * seq(0, m)
  left <- mid - delta
  right <- mid + delta
  if (is.null(limA) == TRUE)
    limA = min(mid)
  if (is.null(limB) == TRUE)
    limB = max(mid)

  frame()
  plot.window(xlim = c(limA, limB), ylim = c(-15, maxFreq), xlab = dataName)#give the user the option to choose min and max xlim
  if (multiple == 1){
    title(main = paste("IVY Plot of ", dataName, " with non-missing n = ", sum(dataCount$freq), "\n (Each leaflet = 1 obs)"))
  }
  else{
    title(main = paste("IVY Plot of ", dataName, " with non-missing n = ", sum(dataCount$freq),  "\n (A leaflet = ", multiple, " obs, except the topmost in each stack maybe fewer)"))
  }
  axis(1, at = c(dataCount$x), lwd = 2, xpd = TRUE)
  if(showFreq == TRUE){
    lines(c(min(mid) - 2, max(mid) + 2), c(-15, -15), lwd = 2)
  }

  index <- rep(0, length(data0))
  for(i in 1:length(data0)){
    index[i] <- which(abs(mid - data0[i]) < delta)
  }
  dataMid <- mid[index]
  dataLeft <- left[index]
  dataRight <- right[index]
  midCount <- count(dataMid)
  leftCount <- count(dataLeft)
  rightCount <- count(dataRight)

  for (i in 1:length(mid)){
    i1 <- midCount$x[z]
    y <- -16
    if (showFreq == TRUE){
      y <- -12.5
      #if (multiple == 1)
      #y <- -7
    }
    if (leftCount$x[z] < mid[i] && mid[i] < rightCount$x[z]){
      if (data1[z] >= 5){
        times <- data1[z] %/% 5
        for (x in 1:times){
          text(i1, y, expression("V"), cex = 1.2, xpd = TRUE)
          y = y + maxFreq/360  #0.05
          text(i1, y, expression("I"), cex = 1.2, xpd = TRUE)
          y = y + maxFreq/90   #0.2
          text(i1, y, expression("v"), cex = 1.5, xpd = TRUE)
          y = y + maxFreq/90   #0.2
          text(i1, y, expression("I"), cex = 1.2, xpd = TRUE)
          if (x %% 5 == 0 && times >= 5)
            y = y + maxFreq/18
          else
            y = y + maxFreq/30   #0.7
          data1[z] = data1[z] - 5
        }
      }
      if (data1[z] == 4){
        text(i1, y, expression("V"), cex = 1.2, xpd = TRUE)
        y = y + maxFreq/600 #0.03
        text(i1, y, expression("I"), cex = 0.9, xpd = TRUE)
        y = y + maxFreq/90 #0.2
        text(i1, y, expression("v"), cex = 1.5, xpd = TRUE)
      }
      else if (data1[z] == 3){
        text(i1, y, expression("V"), cex = 1.2, xpd = TRUE)
        y = y + maxFreq/360 #0.05
        text(i1, y, expression("I"), cex = 1.2, xpd = TRUE)
      }
      else if (data1[z] == 2)
        text(i1, y, expression("V"), cex = 1.2, xpd = TRUE)
      else if (data1[z] == 1)
        text(i1, y, expression("I"), cex = 1.2, xpd = TRUE)

      if(showFreq == TRUE){
        text(i1, -16, dataCount$freq[z], cex = freqSize, xpd = FALSE, col = "blue", font = 3)
      }
      z = z + 1
    }
  }
}
