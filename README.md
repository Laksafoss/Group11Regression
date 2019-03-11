# Regression Assignment

## MODELS FOR FITTING

- [ ]      Depth ~ Phase + Area + Ind + Los + Sun + ODBA
- [ ]      Click ~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua
- [ ]       Call ~ Phase + Area + Ind + Los + Sun + ODBA + Dive + Acou.qua
- [ ] Strokerate ~ Phase + Area + Ind + Los + Sun + ODBA

Only the Phase variable is of interest the rest is for adjustment

## THE ONLY VARIABLES OF INTEREST
- [X] Depth
- [X] Click
- [X] Call
- [X] Strokerate
- [X] Phase  -- > seismik is a subfactor
- [X] Area
- [X] Ind
- [X] Los
- [X] Sun
- [X] ODBA
- [X] Acou.qua
- [X] Dist.to.shore

## VARIABLES TO BE THROWN AWAY !
- [X] VeDBA (strongly correlated with ODBA)
- [X] Buzz (strongly correlated with Call)
- [X] Lat & Long (we will use Area instead)
- [X] Dist.to.Paamiut (to difficult to clean up)


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

