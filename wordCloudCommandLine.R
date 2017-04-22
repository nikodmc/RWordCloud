#!/usr/bin/env Rscript

#This script lets the user run wordCloud from the command line and lets them specify
#the following function inputs:
#     text
#     wordNumber 
#     scaleDownVert 
#     scaleDownHoriz
#     weightingFNum 
#     tickMultiplier 
#     rCoef
#     pdfName


#Here is the wordCloud function definition with all inputs:
# wordCloud = function(text, wordNumber = 60, userSpecifiedStopWords = list(),
#                      maxFontSize = 5, scaleDownVert = .8, scaleDownHoriz = .65,
#                      weightingFNum = 1, userWeightFunction = NULL, 
#                      tickMultiplier = 1, rCoef = 1, resize = 1,
#                      createPDF = TRUE, pdfName = "WordCloud.pdf")

#createPDF should always be true for running it from the command line.
#the resize option is automatically called when creating a pdf so input here is not
#necessary.

#Example calls from commandline:

#Rscript wordCloudCommandLine.R -t "hamlet.txt" -n "comlineHam.pdf"

#Rscript wordCloudCommandLine.R -t "star wars.txt" -n "comlineStarWars.pdf" 
#-v.65 -z .55 -f 2 -m 1.2 -r 1.1 -w 75

#Rscript wordCloudCommandLine.R -t "trainspotting.txt" -n "trainspottingComLine" 
#-f 2 -w 95 -m .75 -r .55 -z .6 -v .7



suppressMessages(library(methods))
suppressMessages(library("argparse"))
suppressMessages(source("wordCloud.R"))

parser = ArgumentParser()

parser$add_argument("-t", "--textFile", 
                    help="a text file to create a word cloud of")

parser$add_argument("-w", "--wordNumber", type = "integer", default = 50,
                    help="number of words to include in word cloud")

parser$add_argument("-v", "--scaleDownVert", default = .8, type = "double", 
                    help="vertical closeness of words, should be 0 to 1, lower is closer")


parser$add_argument("-z", "--scaleDownHoriz", default = .65, type = "double",
                    help="horizontal closeness of words, should be 0 to 1, lower is closer")

parser$add_argument("-f", "--weightFNum", type = "integer", default = 1,
                    help="select built in weighting functions")

parser$add_argument("-m", "--tickMultiplier", type = "double", default = 1,
                    help="how fast the spiral should spin and expand")

parser$add_argument("-r", "--rCoef", type = "double", default = 1,
                    help="how fast the spiral should expand")

parser$add_argument("-n", "--pdfName", default = "WordCloud.pdf",
                    help="name of pdf to be created")


args <- parser$parse_args()
#print(args)

#call the wordCloud function with the user specified inputs
wordCloud(text = args$textFile, 
          wordNumber = args$wordNumber, 
          userSpecifiedStopWords = list(),
          maxFontSize = 5, 
          scaleDownVert = args$scaleDownVert, 
          scaleDownHoriz = args$scaleDownHoriz,
          weightingFNum = args$weightFNum, 
          userWeightFunction = NULL, 
          tickMultiplier = args$tickMultiplier, 
          rCoef = args$rCoef, 
          resize = 1,
          createPDF = TRUE, 
          pdfName = args$pdfName)