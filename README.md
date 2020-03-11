# Tuned Passing Influence Calculation 
# The attached CreateTPIdataframe.R file allows a user to take any Opta F24 event file and create the proposed network theory based metric of Tuned Passing Influence. 
# The attached PassingMapViz.R 
# This repo contains the underlying R code of a project that examines whether success –defined as a win (3 points), tie (1 point) or loss (0 points) -is affected by a passing rating which is compiled through network theory analysis of passing in the sport of soccer. The proposed measurement builds a weighted network that accounts for the total number of teammates passed to, and the number of passes to those unique players. The model is run on 2012 Major League Soccer (MLS) season using Opta F24 event data. The Multinomial Probit Model suggests that the rating has no significant correlation with success.
# This code utilizes the https://github.com/FCrSTATS/fc.rstats package to parse the F24 event data
