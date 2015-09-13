
# Author: Jonas Schöley, received Sep 11, 2015,
# modified by Tim Riffe Sep 12, 2015 (working directory)
# Init --------------------------------------------------------------------
# this script uses the HMDresults object to search for common patterns to the various defined measures.
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/APCT/APCT")
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//APCT//APCT")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/APCT/APCT"))
	}
}

library(grid)

# Diagram Draw Function ---------------------------------------------------

DrawDiatime <- function(x_label = "P", y_label = "A",
                        isoline = TRUE,
                        isoline_label = "C",
                        isoline_orientation = "diagup",
                        isoline_flow_direction = "E") {

  # label positions
  ylabelx <- 0.04
  ylabely <- 0.9
  xlabelx <- 0.9
  xlabely <- 0.04

  # arrowhead style
  arrowhead1 <- arrow(angle = 20, length = unit(0.18, "inches"), type = "closed")
  arrowhead2 <- arrow(angle = 20, length = unit(0.1, "inches"),  type = "closed")

  # translate specifications into graphical parameters
  if (isoline_orientation == "diagup")   {slope <-  1; intercept <- 0}
  if (isoline_orientation == "diagdown") {slope <- -1; intercept <- 1}
  if (isoline_flow_direction == "E") arrowx <- c(0.25, 0.75); arrowy <- c(0.5, 0.5)
  if (isoline_flow_direction == "W") arrowx <- c(0.75, 0.25); arrowy <- c(0.5, 0.5)

  # prepare viewport
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(5, "cm"),
                 height = unit(5, "cm"),
                 clip = "on")
  pushViewport(vp)

  # plot...
  # ...x and y axis
  grid.lines(x = c(0, 1), y = c(0.1, 0.1),
             arrow = arrowhead2,
             gp = gpar(fill = "black"))
  grid.lines(x = c(0.1, 0.1), y = c(0, 1),
             arrow = arrowhead2,
             gp = gpar(fill = "black"))
  # ...x and y labels
  grid.text(x_label, x = xlabelx, y = xlabely)
  grid.text(y_label, x = ylabelx, y = ylabely)
  if (isoline == TRUE) {
    # ...isolines
    grid.abline(intercept = intercept,       slope = slope, gp = gpar(lty = "dashed"))
    grid.abline(intercept = intercept + 0.5, slope = slope, gp = gpar(lty = "dashed"))
    grid.abline(intercept = intercept - 0.5, slope = slope, gp = gpar(lty = "dashed"))
    # ...isoline arrow
    grid.lines(x = arrowx, y = arrowy,
               arrow = arrowhead1,
               gp = gpar(fill = "black"))
    # ...embedded scale label
    grid.text(paste0("(", isoline_label,")"), x = 0.5, y = 0.6)
  }
}

DrawDiatimeIsotropic <- function(x_label = "P", y_label = "A",
                                 isoline = TRUE,
                                 isoline_label = "C",
                                 isoline_flow_direction = "E") {

  # label positions
  ylabelx <- 0.8
  ylabely <- 0.55
  xlabelx <- 0.5
  xlabely <- 0.04

  # arrowhead style
  arrowhead1 <- arrow(angle = 20, length = unit(0.18, "inches"), type = "closed")
  arrowhead2 <- arrow(angle = 20, length = unit(0.1, "inches"),  type = "closed")

  # translate specifications into graphical parameters
  if (isoline_flow_direction == "E") arrowx <- c(0.25, 0.7); arrowy <- c(0.4, 0.4)
  if (isoline_flow_direction == "W") arrowx <- c(0.75, 0.3); arrowy <- c(0.4, 0.4)

  # prepare viewport
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(5, "cm"),
                 height = unit(5, "cm"),
                 clip = "on")
  pushViewport(vp)

  # plot...
  # ...x and y axis
  grid.lines(x = c(0, 1), y = c(0.1, 0.1),
             arrow = arrowhead2,
             gp = gpar(fill = "black"))
  grid.lines(x = c(1, 0.5), y = c(0.1, 0.9),
             arrow = arrowhead2,
             gp = gpar(fill = "black"))

  # ...x and y labels
  grid.text(x_label, x = xlabelx, y = xlabely)
  grid.text(y_label, x = ylabelx, y = ylabely)
  if (isoline == TRUE) {
    # ...isolines
    grid.lines(x = c(0, 0.5), y = c(0.1, 0.9), gp = gpar(lty = "dashed"))
    grid.lines(x = c(0.33, 0.83), y = c(0.1, 0.9), gp = gpar(lty = "dashed"))
    grid.lines(x = c(0.66, 1.16), y = c(0.1, 0.9), gp = gpar(lty = "dashed"))
    # ...isoline arrow
    grid.lines(x = arrowx, y = arrowy,
               arrow = arrowhead1,
               gp = gpar(fill = "black"))
    # ...embedded scale label
    grid.text(paste0("(", isoline_label,")"), x = 0.5, y = 0.5)
  }
}

# Examples ----------------------------------------------------------------

# dimensions of exported pdf
width  <- unit(2.6, "cm"); height <- width

# APc
pdf(file = "./Figures/JonasTable/APc.pdf", width = width, height = width)
  DrawDiatime(y_label = "A", x_label = "P", isoline_label = "C",
              isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/APc_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "P", isoline_label = "C",
                     isoline_flow_direction = "E")
dev.off()

# ACp
pdf(file = "./Figures/JonasTable/ACp.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "C", isoline_label = "P",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/ACp_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "C", isoline_label = "P",
                     isoline_flow_direction = "E")
dev.off()

# CPa
pdf(file = "./Figures/JonasTable/CPa.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "P", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/CPa_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "P", isoline_label = "A",
                     isoline_flow_direction = "E")
dev.off()

# LDc
pdf(file = "./Figures/JonasTable/LDc.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "D", isoline_label = "C",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/LDc_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "D", isoline_label = "C",
                     isoline_flow_direction = "E")
dev.off()

# TLa
pdf(file = "./Figures/JonasTable/TLa.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "L", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/TLa_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "L", isoline_label = "A",
                     isoline_flow_direction = "E")
dev.off()

# TDp
pdf(file = "./Figures/JonasTable/TDp.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "D", isoline_label = "P",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/TDp_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "D", isoline_label = "P",
                     isoline_flow_direction = "E")
dev.off()

# ALt
pdf(file = "./Figures/JonasTable/ALt.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "L", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/ALt_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "L", isoline_label = "T",
                     isoline_flow_direction = "E")
dev.off()

# LP
pdf(file = "./Figures/JonasTable/LP.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "P", isoline = FALSE)
dev.off()
pdf(file = "./Figures/JonasTable/LP_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "P", isoline = FALSE)
dev.off()

# PDt
pdf(file = "./Figures/JonasTable/PDt.pdf", width = width, height = width)
DrawDiatime(y_label = "P", x_label = "D", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/PDt_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "P", x_label = "D", isoline_label = "t",
                     isoline_flow_direction = "E")
dev.off()

# CDl
pdf(file = "./Figures/JonasTable/CDl.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "D", isoline_label = "L",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/CDl_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "D", isoline_label = "L",
                     isoline_flow_direction = "E")
dev.off()

# CT
pdf(file = "./Figures/JonasTable/CT.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "T", isoline = FALSE)
dev.off()
pdf(file = "./Figures/JonasTable/CT_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "T", isoline = FALSE)
dev.off()

# TAl
pdf(file = "./Figures/JonasTable/TAl.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "A", isoline_label = "L",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/TAl_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "A", isoline_label = "L",
                     isoline_flow_direction = "W")
dev.off()

# AD
pdf(file = "./Figures/JonasTable/AD.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "D", isoline = FALSE)
dev.off()
pdf(file = "./Figures/JonasTable/AD_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "D", isoline = FALSE)
dev.off()

# LCd
pdf(file = "./Figures/JonasTable/LCd.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "C", isoline_label = "D",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./Figures/JonasTable/LCd_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "C", isoline_label = "D",
                     isoline_flow_direction = "W")
dev.off()

# TR: needed to modify this because it's indeed informative

##################################
# remove
# TP
#pdf(file = "./fig/TP.pdf", width = width, height = width)
#DrawDiatime(y_label = "T", x_label = "P", isoline = FALSE)
#dev.off()
#pdf(file = "./fig/TP_iso.pdf", width = width, height = width)
#DrawDiatimeIsotropic(y_label = "T", x_label = "P", isoline = FALSE)
#dev.off()
##################################

# TPd
pdf(file = "./Figures/JonasTable/TPd.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "P", isoline_label = "D", isoline = TRUE)
dev.off()
pdf(file = "./Figures/JonasTable/TPd_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "P", isoline_label = "D", isoline = TRUE,
		isoline_flow_direction = "E")
dev.off()
