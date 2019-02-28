# Regression Assignment

## MODELS FOR FITTING

- []      Depth ~ Phase + Area + Ind + Los + Sun + ODBA
- []      Click ~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua
- []       Call ~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua
- [] Strokerate ~ Phase + Area + Ind + Los + Sun + ODBA

Only the Phase variable is of interest the rest is for adjustment

## THE ONLY VARIABLES OF INTEREST
- [] Depth
- [] Click
- [] Call
- [] Strokerate
- [] Phase  -- > can be tested  
- [] Area
- [] Ind
- [] Los
- [] Sun
- [] ODBA
- [] Dive
- [] Acou.qua

## VARIABLES TO BE THROWN AWAY !
- [] VeDBA (strongly correlated with ODBA)
- [] Buzz (strongly correlated with Call)
- [] Lat & Long (we will use Area instead)
- [] Dist.to.Paamiut (to difficult to clean up)
- [] Dist.to.shore (too many NAs)


## Directory layout

See https://nicercode.github.io/blog/2013-04-05-projects/

The assignment has a "fullrun" variable in the first code chunk, such that one
does not accidently run long computations on the full data set.

## Easy setup with Rstudio:

1. Clik the green "Clone or download" botton and copy the link
2. Open R studio
3. Click " File > new Project > Version Control > Git " and fill out
    - "Repository URL" : paste in the lik
    - "Project directory name" : Group11Regression
    - "Create project as subdirectory of" : the folder on your

