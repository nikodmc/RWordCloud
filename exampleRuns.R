#Here is some code that creates a word cloud for Hamlet, The Prince, and The Raven
source("wordCloud.R")

wordCloud("hamlet.txt", weightingFNum = 2, wordNum = 50, scaleDownVert = .7, 
          rCoef = -1, pdfName = "HamletTest.pdf")

wordCloud("the prince2.txt", wordNumber = 50, scaleDownVert = .75, 
          tickMultiplier = 1, rCoef = 1)

wordCloud("http://poetry.eserver.org/the-raven.txt", rCoef = .5)
