
rm(list=ls())

#load packages used in script
packages <- c("tidyverse","dplyr", "tm", "readxl", "NADA2", "NADA", "EnvStats", "stringr","ggspatial","sf","leaflet",
              "htmlwidgets")

#These next lines will check if you have the necessary packages
#It will download and install packages on your computer so
#Ive left them commented out. If you're okay with installing some packages
#Remove the # and run
## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
    library(x, character.only = TRUE)
  }
)

###############################
#compilation of all geochem observations that are part of the Billings GWIP water quality page on GWIC
#in addition to data from a "synoptic" run in the fall/winter of 2022/2023
x <- read.csv("H:/Keeshin_Billings_Workspace/Geochem/0_Figs_Final/Cl_Br/01_Input/N_Halogen_Compilation_ALL.csv")

#############################################
#Discrepancy with dates in the inorganic dataset. Some observations use just month day year with no time
#have to process the text independently then put them back together

#Short date format
short <- x[nchar(as.character(x$sample_date_time)) < 12, ]
short$sample_date_time <- as.POSIXct(short$sample_date_time, format = "%Y-%m-%d", tz = "America/Denver")

#long date format
long <- x[nchar(as.character(x$sample_date_time)) > 12, ]
long$sample_date_time <- as.POSIXct(long$sample_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")

#Put them back together
x <- rbind(long, short)
#################################################
plot_cens <- function(x, x_cens, y, y_cens, xlab, ylab, pch, col, bg, cex, log = "", xlim = NULL, ylim = NULL, cex.axis = 1, cex.lab =1) {
  plot(x, y, type = "n", xlab = xlab, ylab = ylab, log = log, xlim = xlim, ylim = ylim,cex.axis = cex.axis, cex.lab = cex.lab)
  
  for (i in seq_along(x)) {
    # Define limits based on whether log scales are used
    x_limit <- ifelse(grepl("x", log), 10^par("usr")[1], -10) #par("usr") is a way to retrieve
    #the current plotting limits (or "user coordinates") for the x and y axes. It returns a
    #numeric vector of length 4 that gives the limits of the plot in the form:
    #c(xmin, xmax, ymin, ymax)
    y_limit <- ifelse(grepl("y", log), 10^par("usr")[3], -10)
    
    if (x_cens[i] & y_cens[i]) {
      # Both x and y censored, draw a dashed box
      lines(c(x_limit, x[i]), c(y[i], y[i]), col = "black", lty = 2)
      lines(c(x[i], x[i]), c(y_limit, y[i]), col = "black", lty = 2)
    } else if (x_cens[i]) {
      # Only x is censored, draw a horizontal dashed line
      lines(c(x_limit, x[i]), c(y[i], y[i]), col = "black", lty = 2)
    } else if (y_cens[i]) {
      # Only y is censored, draw a vertical dashed line
      lines(c(x[i], x[i]), c(y_limit, y[i]), col = "black", lty = 2)
    } else {
      # Neither x nor y are censored, plot the point
      points(
        x[i], y[i],
        col = col[i],
        pch = pch[i],
        bg = bg[i],
        cex = cex[i]
      )
    }
  }
}

##############################################
#size of axis and axis labels
cex.axis <- 1.5
cex.lab <- 1.5
#######################################
#EXCLUDING censored data, must be noted!

#keeping an unaltered version of the dataframe
x_unfilt <- x

#removing any row with a censored bromide or chloride value
x <- filter(x, x$br_mg_l_Cens == FALSE & x$cl_mg_l_Cens == FALSE)

#Creating dataframe to see how many rows are censored for Br, its 87
x_br_cens <- filter(x_unfilt, x_unfilt$br_mg_l_Cens == TRUE)
#seeing for Cl, there are no censored chloride values
x_cl_cens <- filter(x_unfilt, x_unfilt$cl_mg_l_Cens == TRUE)
#Seeing how many rows are censored for either Br or Cl, its 87 because there are no censored Cl values
x_cl_br_cens <-filter(x_unfilt,x_unfilt$br_mg_l_Cens == TRUE | x_unfilt$br_mg_l_Cens == TRUE)



pdf(
  file = "H:/Keeshin_Billings_Workspace/Geochem/0_Figs_Final/Cl_Br/03_Incremental/Cl_BrCl.pdf",
  height = 10,
  width = 10
)


par(mar = c(5, 5, 2, 2))


plot(
  x = x$cl_mg_l,
  y = (x$cl_mg_l)/(x$br_mg_l), #this is a mass, not molar ratio
  xlab = "Cl (mg/L)",
  ylab = "Cl:Br mass ratio",
  log = "xy",
  pch = x$pch,
  col = x$col,
  bg = alpha(x$bg, 0.6),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = x$cex,
   ylim = c((40),(10000)),
   xlim = c(1, 20000)
)



#add halite to plot from Katz et al., 2011
rect(
  xleft = 10000,
  ybottom = 1000,
  xright = 80000,
  ytop = 10000,
  lty=2,
  lwd =2
)

text(
  x = 3000,
  y = 4000,
  labels = expression(bold("  Halite/\nRoad Salt")),
  cex =2
)

#add basin brine/seawater to plot
#combining seawater and basin brines from katz et al., 2011
rect(
  xleft = 19000,
  ybottom = 110,
  xright = 251000,
  ytop = 406,
  lty=2,
  lwd =2
)


text(
  x = 4550,
  y = 200,
  labels = expression(bold("Brines and\nmarine salts")),
  cex =2
)

#Dilute GW, eyeballed from Katz et al., 2011 figure
rect(
  xleft = 1,
  ybottom = 40,
  xright = 10,
  ytop = 300,
  lty=2,
  lwd =2
)

text(
  x = 3,
  y = 360,
  labels = expression(bold("      Dilute\n Groundwater")),
  cex =2
)

# Add septic dashed box to the plot from Katz Table 2
rect(
  xleft = 20,       #min cl
  ybottom = 275,    #min cl:br mass ratio
  xright =  396,    #max cl
  ytop =  1150,     #max cl:br mass ratio
  lty=2,
  lwd =2)

text(
  x = 75,
  y = 1350,
  labels = expression(bold("Septic")),
  cex =2
)

#Add janky animal waste box
rect(
  xleft = 10,       #min cl
  ybottom = 58,    #min cl:br mass ratio
  xright =  847,    #max cl
  ytop =  1654,     #max cl:br mass ratio
  lty=2,
  lwd =2,
  border  = "green")

text(
  x = 75,
  y = 2550,
  labels = expression(bold("Animal Waste")),
  cex =2,
  col = "green"
)

legend(
  "bottomright",
  pch = c(21,21,24,22,23),
  col = "black",
  pt.bg = c("blue","purple","yellow", "khaki3","darkgray"),
  pt.cex = 2,
  cex = 1.4,
  legend = c("Stream","Canal/Ditch","Spring", "Alluvial Aquifer","Colorado Gp. Shale")
)



dev.off()
############################
windows()

plot(
  x = x$cl_mg_l / x$br_mg_l,
  y = x$no3_as_n,
  xlab = "Cl:Br mass ratio",
  ylab = "Nitrate-N (mg/L)",
  log = "x",
  pch = x$pch,
  col = x$col,
  bg = alpha(x$bg, 0.6),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = x$cex,
)

CS <- filter(x, x$gwic_id == 158589)

points(
  x = CS$cl_mg_l /CS$br_mg_l,
  y = CS$no3_as_n,
  pch = 16
)

check <- filter(x, x$cl_mg_l/x$br_mg_l > 4000)


###########################
y <- read.csv("H:/Keeshin_Billings_Workspace/Geochem/0_Figs_Final/Cl_Br/01_Input/SW_Corner_Geochem.csv")

y <- filter(y, y$br_mg_l_Cens == FALSE)

windows()

plot(
  y = y$cl_mg_l / y$br_mg_l,
  x = y$cl_mg_l,
  ylab = "Cl:Br mass ratio",
  xlab = "Chloride (mg/L)",
  log = "x",
  pch = y$pch,
  col = y$col,
  bg = alpha(y$bg, 0.6),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = y$cex,
)

windows()

plot(
  x = y$cl_mg_l / y$br_mg_l,
  y = y$no3_as_n,
  xlab = "Cl:Br mass ratio",
  ylab = "Nitrate-N (mg/L)",
  log = "x",
  pch = y$pch,
  col = y$col,
  bg = alpha(y$bg, 0.6),
  cex.axis = 1.5,
  cex.lab = 1.5,
  cex = y$cex,
)