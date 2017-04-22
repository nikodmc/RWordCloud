#!/usr/bin/env/Rscript

#Niko Drake-McLaughlin
#October 25th 2016, updated December 3rd, 2016
#Unit Testing attempt

library(testthat)

source("wordCloud.R")

test_that("Incorrect function inputs return error", {
  expect_error(wordCloud("trainspotting.txt", 
                         wordNumber = -70))
               
  expect_error(wordCloud("trainspotting.txt", 
                         userSpecifiedStopWords = "cheese")) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         maxFontSize = -4)) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         scaleDownVert = -.75)) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         scaleDownHoriz = 0)) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         tickMultiplier = 0)) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         rCoef = 0)) 
               
  expect_error(wordCloud("trainspotting.txt", 
                         resize = 1, maxFontSize = 1)) 
               
  expect_error(wordCloud("e a")) 

})


#Test overlap

test_that("overlap function: test the many scenarios of overlapping 
          rectangles", {
  #Create a series of vectors to test in the dataframe testVectorsDF
  testVectorsDF = rbind.data.frame(c(1, 0.2084948, -0.00849483, 0.3642100, 0.6357900),
                               c(2, 0.2820761,  0.13491356, 0.3496652, 0.6503348))
  colnames(testVectorsDF) = c("id", "upper", "lower", "left", "right")
  testVectorsDF[3,] = c(3,0.2084948, -0.00849483, 0.3642100, 0.6357900)
  testVectorsDF[4,] = c(4, 3, 1, 3, 5)
  testVectorsDF[5,] = c(5, 2, 1, 3, 5)
  testVectorsDF[6,] = c(5, 3, 2, 3, 5)
  testVectorsDF[7,] = c(7, 4, 2, 5, 6)
  testVectorsDF[8,] = c(8, 3.5, 2.5, 4, 7)
  testVectorsDF[9,] = c(9,0, 0, 0, 0)
  testVectorsDF[10,] = c(10, 1, 1, 1, 1)
  testVectorsDF[11,] = c(11, 1, 0, 0, 1)
  testVectorsDF[12,] = c(12, 2, 1, 0, 1)
  
  #first rectangles upper right and left corners overlap second
  expect_equal(overlap(testVectorsDF[1,],testVectorsDF[2,]), TRUE)
  #rectangles are identical, complete overlap
  expect_equal(overlap(testVectorsDF[1,],testVectorsDF[3,]), TRUE)
  #No overlap
  expect_equal(overlap(testVectorsDF[1,],testVectorsDF[4,]), FALSE)
  #first rectangle upper overlaps, both corners
  expect_equal(overlap(testVectorsDF[4,],testVectorsDF[5,]), TRUE)
  #first rectangles lower overlaps, both corners
  expect_equal(overlap(testVectorsDF[4,],testVectorsDF[6,]), TRUE)
  #No overlap, though risks an upper right corner overlap
  expect_equal(overlap(testVectorsDF[4,],testVectorsDF[7,]), FALSE)
  #Upper edge of first rectangle touches lower of second, but no overlap
  expect_equal(overlap(testVectorsDF[5,],testVectorsDF[6,]), FALSE)
  #upper right corner of first rectangle touches but does not overlap second
  expect_equal(overlap(testVectorsDF[5,],testVectorsDF[7,]), FALSE)
  #first rectangle taller and skinnier, second wider and shorter. Upper and lower of 
  #second overlap, but no corners overlap
  expect_equal(overlap(testVectorsDF[7,],testVectorsDF[8,]), TRUE)
  #reversal of above, now first rectangle wider and shorter, second taller and
  #skinnier
  expect_equal(overlap(testVectorsDF[8,],testVectorsDF[7,]), TRUE)
  #single points, no overlap
  expect_equal(overlap(testVectorsDF[9,],testVectorsDF[10,]), FALSE)
  #single point and itself, no overlap (must have area to overlap)
  expect_equal(overlap(testVectorsDF[9,],testVectorsDF[9,]), FALSE)
  #upper edge of first rectangle touches but does not overlap second
  expect_equal(overlap(testVectorsDF[11,],testVectorsDF[12,]), FALSE)
})

#Random testing with the overlap function
test_that("Using random testing for the overlap function to test 
            lower left and upper right corners of two rectangles overlapping", {
  #Lower Left/Upper Right
  vec2 = rep(2,1000)
  rand01Lower = runif(1000, min = 0, max = .9999)
  rand01Left = runif(1000, min = 0, max = .9999)
  lowerLeftOverlap = cbind(vec2, vec2, rand01Lower, rand01Left, vec2)
  colnames(lowerLeftOverlap) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  #The upper right hand corner of the unit rectangle vector will always overlap with 
  #the lower left hand corner of every row of lowerLeftOverlap

  expect_true(all(apply(lowerLeftOverlap, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(apply(lowerLeftOverlap, 1, overlap, vecOld = unitRect)))
})
  
test_that("Using random testing for the overlap function to test 
            lower left and upper right corners of two rectangles not overlapping", {
  #Now none should overlap
  vec2 = rep(2,1000)
  rand12Lower = runif(1000, min = 1, max = 1.9999)
  rand12Left = runif(1000, min = 1, max = 1.9999)
  lowerLeftNone = cbind(vec2, vec2, rand12Lower, rand12Left, vec2)
  colnames(lowerLeftNone) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  expect_true(all(!apply(lowerLeftNone, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(!apply(lowerLeftNone, 1, overlap, vecOld = unitRect)))
})


test_that("Using random testing for the overlap function to test 
            upper left and lower right corners of two rectangles overlapping", { 
  #Upper Left/Lower Right
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000)
  rand01Upper = runif(1000, min = 0, max = .9999)
  rand01Left = runif(1000, min = 0, max = .9999)
  upperLeftOverlap = cbind(vec2, rand01Upper, vecNeg2, rand01Left, vec2)
  colnames(upperLeftOverlap) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  #The lower right hand corner of the unit rectangle vector will always overlap with 
  #the upper left hand corner of every row of upperLeftOverlap

  expect_true(all(apply(upperLeftOverlap, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(apply(upperLeftOverlap, 1, overlap, vecOld = unitRect)))
})

test_that("Using random testing for the overlap function to test 
          lower left and upper right corners of two rectangles not overlapping", {  
  #Now none should overlap
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000)
  rand12Upper = runif(1000, min = -1, max = 0)
  rand12Left = runif(1000, min = 1, max = 1.9999)
  upperLeftNone = cbind(vec2, rand12Upper, vecNeg2, rand12Left, vec2)
  colnames(upperLeftNone) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  
  expect_true(all(!apply(upperLeftNone, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(!apply(upperLeftNone, 1, overlap, vecOld = unitRect)))

})

#After testing all corner combinations these next four testing groups look at cases
#where corners and entire sides can overlap from above and then from below.
#This first case uses a unit rectangle and another rectangle with a fixed upper
#side at height 2, with left, right and lower sides selected randomly in such a way
#that they must overlap
test_that("Using random testing for the overlap function to test 
            upper and lower sides of two rectangles overlapping", {   
  #Upper Lower Overlap
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000)            
  randLower = runif(1000, min = -1, max = .9999)
  randLeft = runif(1000, min = -1, max = .5)
  randRight = runif(1000, min = .5, max = 2)
  upperLowerOverlap = cbind(vec2, vec2, randLower, randLeft, randRight)
  colnames(upperLowerOverlap) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
 
  expect_true(all(apply(upperLowerOverlap, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(apply(upperLowerOverlap, 1, overlap, vecOld = unitRect)))

})

#This second case uses a unit rectangle and another rectangle with a fixed upper
#side at height 2, with left, right and lower sides selected randomly in such a way
#that they should never overlap  
test_that("Using random testing for the overlap function to test 
            upper and lower sides of two rectangles not overlapping", { 
  #Now none should overlap
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000) 
  randLower = runif(1000, min = 1, max = 2)
  randLeft = runif(1000, min = -1, max = .5)
  randRight = runif(1000, min = .5, max = 2)
  upperLowerNone = cbind(vec2, vec2, randLower, randLeft, randRight)
  colnames(upperLowerNone) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  
  expect_true(all(!apply(upperLowerNone, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(!apply(upperLowerNone, 1, overlap, vecOld = unitRect)))
})

#This third case uses a unit rectangle and another rectangle with a fixed lower
#side at height -2, with left, right and upper sides selected randomly in such a way
#that they must overlap  
test_that("Using random testing for the overlap function to test 
            lower and upper sides of two rectangles overlapping", { 
  #Lower Upper Overlap
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000) 
  randUpper = runif(1000, min = 0.0001, max = 1.5)
  randLeft = runif(1000, min = -1, max = .5)
  randRight = runif(1000, min = .5, max = 2)
  lowerUpperOverlap = cbind(vec2, randUpper, vecNeg2, randLeft, randRight)
  colnames(lowerUpperOverlap) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  
  expect_true(all(apply(lowerUpperOverlap, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(apply(lowerUpperOverlap, 1, overlap, vecOld = unitRect)))

})

#This fourth case uses a unit rectangle and another rectangle with a fixed lower
#side at height -2, with left, right and upper sides selected randomly in such a way
#that they should never overlap   
test_that("Using random testing for the overlap function to test 
            upper and lower sides of two rectangles not overlapping", {  
  #Now none should overlap
  vec2 = rep(2,1000)
  vecNeg2 = rep(-2,1000) 
  randUpper = runif(1000, min = -2, max = 0)
  randLeft = runif(1000, min = -1, max = .5)
  randRight = runif(1000, min = .5, max = 2)
  lowerUpperNone = cbind(vec2, randUpper, vecNeg2, randLeft, randRight)
  colnames(lowerUpperNone) = c("id", "upper", "lower", "left", "right")
  unitRect = c(1, 1, 0 , 0, 1)
  
  expect_true(all(!apply(lowerUpperNone, 1, overlap, vecNew = unitRect)))
  #now switch positions to check opposite relationship between input vectors
  expect_true(all(!apply(lowerUpperNone, 1, overlap, vecOld = unitRect)))

})
  



test_that("wordsToVector: test that text to vector is working properly",{
  expect_equal(wordsToVector("I like to move it move it"),
               c("like", "to", "move", "it", "move", "it"))
  expect_equal(wordsToVector("w h a t what?"), c("what"))
  expect_equal(wordsToVector("we ha an to what? can't won't"),
               c("we", "ha", "an", "to", "what", "can't", "won't"))
  expect_equal(wordsToVector("Ipso facto, the paradigm shift must be be disrupted 
                             quod erat demonstrandum"),
               c("ipso", "facto", "the", "paradigm", "shift", "must", "be", "be",
                 "disrupted", "quod", "erat", "demonstrandum"))
})


test_that("wordCounts: Make sure the word counts function behaves appropriately",{
  expect_true(all(countWords(c("duck", "duck", "goose", "duck","duck","grey duck"))
                  ==
                  cbind(c("duck", "goose", "grey duck"), c(4, 1, 1))))
  
  expect_true(all(countWords(c("jump", "frog", "jump", "frog", "jump", "toad", "fox", 
                            "fox", "duck","duck","grey duck"))
                  ==
               cbind(c("jump", "duck", "fox", "frog", "grey duck", "toad"), 
                     c(3, 2, 2, 2, 1, 1))))
  expect_true(all(countWords(c(rep("word", 1011), rep("count", 689)))
                  ==
                  cbind(c("word", "count"), c(1011, 689))))
})



