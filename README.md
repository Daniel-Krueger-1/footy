# Tuned Passing Influence Calculation 
###### This repo contains the underlying R code of a project that examines whether success – defined as a win (3 points), tie (1 point) or loss (0 points) - is affected by a passing rating which is compiled through network theory analysis of passing in the sport of soccer. The proposed measurement builds a weighted network that accounts for the total number of teammates passed to, and the number of passes to those unique players. The model is run on 2012 Major League Soccer (MLS) season using Opta F24 event data. Upon completion of the creation of the TPI metric for the 2012 MLS season a Multinomial Probit Model was run and showed that the rating has no significant correlation with success.
# CreateTPIdataframe.R
###### The attached CreateTPIdataframe.R file allows a user to take any Opta F24 event file and create the proposed network theory based metric of Tuned Passing Influence. This code utilizes https://github.com/FCrSTATS/fc.rstats package to parse the F24 event data.
# PassingMapViz.R
###### The attached PassingMapViz.R creates a passing network that takes into account the nuances of the TPI passing rating. The passing network visual built from f24-98-2012-429860-eventdetails.xml shows each Colorado Rapids player's average passing position throughout the game. As players complete passes higher up the field they are awarded a higher TPI tuning measure (𝛼). The shading on the connections between players represents the 𝛼 given to that player's base passing score. For a more detailed account of the TPI rating see **ThesisFinal.pdf** The visual output for the aforementioned F24 file is attached under PassingMapViz.png

![alt text](https://github.com/Daniel-Krueger-1/footy/blob/master/PassingMapViz.png?raw=true)
# Degree Centrality 
## 𝑇𝑃𝐼 = 𝐷𝐶𝛼(𝑖) = 𝑘𝑖 (𝑠𝑖/𝑘𝑖)^𝛼

## Degree Centrality Building Blocks 
###### ki:  Total number of unique teammates the given player has successfully passed to 
###### si: Total number of passes completed between the given player and the unique teammate 
###### a: Tuning Measure to account for position on the field the pass was completed into 

# Get in contact with questions, comments and collaboration ideas 
> danielpkrueger1 at gmail dot com
