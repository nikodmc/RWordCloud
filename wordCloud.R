#Niko Drake-McLaughlin
#October 25th, 2016, updated December 3rd, 2016
#Word Cloud
#Computational Statistics 
#Welcome to word cloud!
#When given a text file or url to a text file, this program will create a word cloud.
#It is currently very slow, despite some measures taken to counteract this. It will
#only use words with more than one letter at the moment. There are
#also optional inputs. These other inputs are:
#wordNumber = number of words in the cloud (default = 60). Fewer words goes faster.
#userSpecifiedStopWords = additional stop words specified by the user. Must be
#   in a list of character strings.
#maxFontSize = maximum font size for the maximum weighted word
#scaleDownVert = how close words can get vertically. Lower values means closer, 
#    with overlaps possible. Must be positive. (default = .8) 
#scaleDownHoriz = how close words can get horizontally. Lower values means closer, 
#    with overlaps possible. Must be positive. (default = .65)
#weightingFNum = weighting function number, with three possibilities 1, 2, or 3
#             = 1 then the word frequency is the weight (this is the default)
#             = 2 then the square root of word frequency is used
#             = 3 then the natural log of word frequency is used
#             = anything else, then it is treated as a 1.
#userWeightFunction = a weighting function to be applied to the frequency count of 
#   the words. There are two built in option with weightingFNum, but the user can 
#   pass in their own function. If the userWeightFunction is not NULL, the default,
#   then that function will automatically be used, overriding what the weightingFNum 
#   is.
#tickMultiplier = how large of an angle the spiral should move before attempting to
#   plot a new word. Increasing this decreases run time. Default is 1, must be 
#   non zero
#rCoef = how quickly the spiral should expand before attempting to plot a new word.
#   default is 1, must be non zero. Increasing this should decrease run time, 
#   but should increase spaces between words.
#resize = whether the program should try to automatically refit the word cloud to 
#   better fit the plot. Seems to only work when maxFontSize = 5. Program will not
#   run if resize = 1 and maxFontSize != 5. Creates a crazy jumble, while still 
#   taking forever to run. I still need to figure that bit out.
#createPDF = whether a pdf document of the word cloud should be created. Default 
#   is TRUE.
#pdfName = text of what the word cloud pdf should be called, default 
#   is "WordCloud.pdf"
#   
#The final word cloud looks better with more words and default or low tickMultiplier
#and rCoef, but it takes longer to create.

#Here is a recommended function calls:
#wordCloud("http://www.awesomefilm.com/script/biglebowski.txt", weightingFNum = 2,
#          wordNum = 70, tickMultiplier = 1.5, scaleDownVert = .7)

#read in file
readInFile = function(filename) {
  isUrl = length(grep("^https?:", filename)) == 1
  isFile = length(grep("[.][[:alnum:]]{2,7}$", filename)) == 1
  if (isUrl || isFile) {
    text = suppressWarnings(readLines(filename))
    return(text)
  }
  return(filename)
}

#alter text into a vector of words
wordsToVector = function(text) {
  #Remove symbols, but keep apostrophes in the middle of words
  re = "[-[:space:]!@#$%^&*()_+=\":;<>.,?/\\]+"
  text = strsplit(text, re)
  
  #Convert all to lowercase, spaces and numbers
  text = tolower(unlist(text))
  #Only keep words greater than 1 in character length
  text = text[nchar(text) > 1]
  #Remove apostrophes at the end or beginning of words (like when used as quotes)
  text = sub("^'","", text)
  text = sub("'$", "", text)
  
  return(text)
}

#Returns a dataframe with the words and their counts when fed the word vector
countWords = function(text) {
  counts = sort(table(text), decreasing = TRUE)
  countsOrdered = data.frame(word=names(counts), count=as.vector(counts))
  return(countsOrdered)
}

#This function lets the user tamp down on those maximum weights. A weightFNum of
#2 applies a square root function, of 3 applies a log function (makes everything 
#almost equal). If a userWeightFunction is provided, then that is automatically used.
reweightFunction = function(wordCounts, weightFNum, userWeightFunction) {
  if(!is.null(userWeightFunction)) {
    wordCounts[,2] = userWeightFunction(wordCounts[,2])
    return(wordCounts)
  }
  
  if(weightFNum == 2){
    wordCounts[,2] = sqrt(wordCounts[,2])
  }
  
  if(weightFNum == 3) {
    wordCounts[,2] = log(wordCounts[,2])
  }
  
  return(wordCounts) 
}

#overlap function checks if a dataframe of word positions conflicts with the position
#of a new potential word placement

overlap = function(vecOld, vecNew) {
  #0 or 1 overlap indicators
  overlapUpper = 0
  overlapLower = 0
  overlapLeft = 0
  overlapRight = 0
  
  #vec[2] = upper, vec[3] = lower, vec[4] = left, vec[5] = right
  upperOld = vecOld[2]
  lowerOld = vecOld[3]
  leftOld = vecOld[4]
  rightOld = vecOld[5]
  
  upperNew = vecNew[2]
  lowerNew = vecNew[3]
  leftNew = vecNew[4]
  rightNew = vecNew[5]
  
  #this test if new upper is between old upper and old lower
  #the other lines use similar tests
  if(upperNew <= upperOld && upperNew > lowerOld) {
    overlapUpper = 1
  }
  
  if(leftNew >= leftOld && leftNew < rightOld) {
    overlapLeft = 1
  }
  
  #If there is an overlap, get out of here immediately
  if(overlapUpper == 1 && overlapLeft == 1) {
    return(TRUE)
  }
  
  if(rightNew <= rightOld && rightNew > leftOld) {
    overlapRight = 1
  }
  
  if(overlapUpper == 1 && overlapRight == 1) {
    return(TRUE)
  }
  
  if(lowerNew >= lowerOld && lowerNew < upperOld) {
    overlapLower = 1
  }
  
  if((overlapLower == 1 && overlapLeft ==1) | 
     (overlapLower == 1 & overlapRight ==1)) {
    return(TRUE)
  }
  #wide short word overlaps another
  if((overlapUpper == 1 || overlapLower == 1) &&
     leftNew <= leftOld &&
     rightNew >= rightOld) {
    return(TRUE)
  }
  #tall skinny word overlaps another
  if((overlapLeft == 1 || overlapRight == 1) &&
     upperNew >= upperOld &&
     lowerNew <= lowerOld) {
    return(TRUE)
  }
  #New word is completely overlays old word (possible with a weird weighting scheme)
  if(upperNew > upperOld & lowerNew < lowerOld & leftNew < leftOld &
     rightNew > rightOld) {
    return(TRUE)
  }
  
  return(FALSE)
  
}

#This function uses a function in polar coordinates to spit out the x-y 
#coordinates for the next possible plot of the word.
#Increasing theta more quickly really improves speed but makes worse 
#looking clouds.
plotPosition = function(tick, wordIndex, tickMultiplier, rCoef) {
  #theta increases with each tick (loop through) of the plotting function
  #the 1/log(wordIndex + 1) bit is to decrease the angle jump for smaller 
  #weighted words
  theta = (pi/17) * tick  * (1/log(wordIndex + 1)) * tickMultiplier
  r = rCoef*.05*theta
  x = r*cos(theta) + 5
  y = r*sin(theta) + 5
  
  return(c(x,y))
}

#The for loop that does the first plot of everything!
#Goes through the counts dataframe and plots each words
#size is based on the frequency of the word
plotTheWordCloud = function(wordCounts, maxFontSize, scaleDownVert, scaleDownHoriz,
                            tickMultiplier, rCoef) {
  
  countLength = nrow(wordCounts)
  
  #Create data frame that will hold all of the word position corners
  positionDF = data.frame(id = 1:countLength, upper = rep(0,countLength), 
                          lower = rep(0,countLength), left = rep(0,countLength), 
                          right = rep(0,countLength))
  #Create data frame that will hold all of the words and their plotted centers
  positionXY = data.frame(word = wordCounts[,1],
                          xpos = rep(0, countLength), ypos = rep(0, countLength))
  
  #Create new plot, that is completely blank, with no margins
  par(mar = c(0,0,0,0), xpd=TRUE)
  #plot.new()
  #Plot ranges from 0 to 10 on both axes
  plot(c(10,0),c(10,0), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
  
  #Set the tick to zero (this will be fed into the plot position function)
  tick = 0
  #Default maximum font size is 5, but user could change it if they want
  div = maxFontSize/wordCounts[1,2]
  
  #This variable will save where the last overlapping word is in the positionDF 
  #data frame
  lastHit = 0
  
  for (wordIndex in 1:(countLength)) {
    #variable for the while loop (a do-while loop)
    redo = TRUE
    
    #This while loop looks for an non-overlapping space
    while(redo == TRUE) {
      #Get the xy-coordinates to attempt to plot the word
      w = plotPosition(tick, wordIndex, tickMultiplier, rCoef)
      x = w[1]
      y = w[2]
      #x = plotPosition(tick, i, tickMultiplier, rCoef)[1]
      #y = plotPosition(tick, i, tickMultiplier, rCoef)[2]
    
      #Find the coordinates for the text box with the scale down factors
      ri = x + scaleDownHoriz*strwidth(wordCounts[wordIndex,1], 
                                              cex = wordCounts[wordIndex,2]*div)
      up = y + scaleDownVert*strheight(wordCounts[wordIndex,1], 
                                       cex = wordCounts[wordIndex,2]*div)
      lo = y - scaleDownVert*strheight(wordCounts[wordIndex,1], 
                                       cex = wordCounts[wordIndex,2]*div)
      le = x - scaleDownHoriz*strwidth(wordCounts[wordIndex,1], 
                                       cex = wordCounts[wordIndex,2]*div)
      #record the coordinates in the positionDF data frame
      positionDF[wordIndex,] = c(wordIndex, up, lo, le, ri)
      
      overlapping = 0
      #if there is a record for the last collision, check this spot first
      if(lastHit != 0) {
        overlapping = overlap(vecOld = positionDF[lastHit,], 
                              vecNew = positionDF[wordIndex,])
      }
      #this loops through all of the words already plotted, starting with the most
      #recently plotted. Once there is a collision, it breaks out of the loop to try
      #a new position. Originally I had an apply function, but I hoped that this
      #backwards loop and the lastHit feature could make it run fast. I was 
      # wrong.
      if(wordIndex > 1 && overlapping == 0) {
        for(checkPlacedWord in (wordIndex - 1):1) {
          overlapping = overlap(vecOld = positionDF[checkPlacedWord,], 
                                vecNew = positionDF[wordIndex,])
          if(overlapping == 1) {
            lastHit = checkPlacedWord
            break
          }
        }
        
      }
      #increase tick size to find a new space for text box in next loop
      tick = tick + 1
      #if word does not overlap with anything, record it's center and set redo to 0
      #to get out of the while loop
      if(overlapping == 0) {
        positionXY[wordIndex,2:3] = c(x, y)
        redo = FALSE
      }
    }
    #Plot the word! Fun to watch the words slowly plot, though not really necessary.
    #Though it takes so long, it's nice to watch something happen.
    #rgb function makes highest weighted words blue and then shift to green as 
    #weight decreases. 
    text(x, y, wordCounts[wordIndex,1], cex = wordCounts[wordIndex,2]*div, 
         col = rgb(0, wordCounts[1,2] - wordCounts[wordIndex,2],  
                   wordCounts[wordIndex,2], maxColorValue = wordCounts[1,2]))

  }
  return(list(positionDF, positionXY))
}

#This function resizes the word cloud to make best use of the plot fit. It doesn't
#always work though...
resizePlot = function(positionList, resize, wordCounts) {
  if(resize != 1)
    return()
  
  corners = positionList[[1]]
  centers = positionList[[2]]
  centers[,1] = wordCounts[,1]
  centers$weight = wordCounts$count
  
  #new blank plot
  par(mar = c(0,0,0,0), xpd=TRUE)
  #This new plot size is based on the most extreme values of the earlier plot
  plot(c(max(corners[,5]),min(corners[,4])),c(max(corners[,2]),min(corners[,3])), 
       ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
  #This formula below works well for reformatting the cex font size
  div = (3/centers[1,4]) * (17/(max(corners[,5])-min(corners[,4])))
  #Using the information recorded in the previous function, all of the words are 
  #replotted on the new axes.
  countLength = length(centers[,1])
  for(i in 1:countLength) {
    with(centers, text(xpos[i], ypos[i], word[i], cex = weight[i]*div, 
                       col = rgb(0, weight[1] - weight[i],  weight[i],
                                 maxColorValue = weight[1])))
    
  }
  
}


wordCloud = function(text, wordNumber = 60, userSpecifiedStopWords = list(),
                      maxFontSize = 5, scaleDownVert = .8, scaleDownHoriz = .65,
                      weightingFNum = 1, userWeightFunction = NULL, 
                      tickMultiplier = 1, rCoef = 1, resize = 1, 
                      createPDF = TRUE, pdfName = "WordCloud.pdf") {
  #Let's bust out of here right away if the user gave bad inputs
  if(wordNumber <= 0) {
    stop("You have asked for a word cloud with a strange number of words. 
         Please specify an integer greater than zero!")
  }
  if(!is.list(userSpecifiedStopWords)) {
    stop("Your stop words should be in a list!")
  }
  if(maxFontSize <=0) {
    stop("That's not a reasonable font size! 
         Please specify an integer greater than zero")
  }
  if(scaleDownVert <=0 || scaleDownHoriz <=0) {
    stop("Your scaling factors should be greater than 0! 
         Please specify an integer greater than zero")
  }
  if(tickMultiplier == 0 || rCoef == 0) {
    stop("Your tickMultiplier and the r coefficient should not be 0! 
        Please specify a non-zero number")
  }
  if(maxFontSize != 5 && resize == 1) {
    stop("resize only works when the maxFontSize is set to 5")
  }
  
  #Read in the file
  text = readInFile(text)
  #change the text file to a word vector
  text = wordsToVector(text)
  #Error out if the text file is empty
  if(length(text) <= 1) {
    stop("Either you fed me nothing, or only single letter words.
      This program does not work with this input.")
  }
  #get a dataframe with each individual word and their frequency
  countsTotal = countWords(text)
  
  #Read in stop list and append user specified stop words
  stopList = "http://xpo6.com/wp-content/uploads/2015/01/stop-word-list.txt"
  stopList = readLines(stopList, warn = FALSE)
  stopList = unlist(strsplit(stopList, "[[:space:]]*,[[:space:]]*"))
  stopList = c(stopList, tolower(userSpecifiedStopWords))
  
  #Remove text words in the stop list
  wordCounts = countsTotal[countsTotal$word %in% setdiff(text, stopList),]
  
  if(nrow(wordCounts) == 0) {
    stop("After removing stop words, there's nothing left!")
  }
  
  #Set length of list
  wordNumber = min(wordNumber, nrow(wordCounts))
  wordCounts = wordCounts[1:wordNumber,]

  wordCounts = reweightFunction(wordCounts, weightingFNum, userWeightFunction)
  
  positionList = plotTheWordCloud(wordCounts, maxFontSize, 
                                  scaleDownVert, scaleDownHoriz, 
                                  tickMultiplier, rCoef) 
  
  resizePlot(positionList, resize, wordCounts)


  if (createPDF) {
    #confirm that suffix for pdf name is correct
    pdfSuffix = length(grep("[.]pdf$", pdfName)) == 1
    
    #if suffix is incorrect, add ".pdf"
    if(!pdfSuffix) {
      pdfName = paste(pdfName, ".pdf", sep = "")
    }
    #create pdf with resized graph
    pdf(pdfName, width = 6, height = 4.5)
    resizePlot(positionList, 1, wordCounts)
    dev.off()
  }
  

}

