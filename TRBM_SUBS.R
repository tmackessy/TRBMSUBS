
#==============================================================================
# Engineering Design Curves for TRBM/SUBS Recommendation
# Original Author: Dr. Joseph Park, Nov - Dec 2012
# Maintained by Trevor Mackessy-Lloyd
#==============================================================================
# TRBM platform selection and SUBS mooring configuration recommendations are
#   based on balancing the hydrodynamic forces with frictional forces to
#   prevent platform/anchor drag or lift, and to assess the vertical
#   depression of a SUBS.
#
# The methods were validated against measurements of SUBS performance:
#   Ewald, J. Paternostro, C., (2005). Assessment of sub-surface mooring
#     dynamics with acoustic Doppler current meters in high speed environments,
#     MTS/IEEE OCEANS 2005, p 1692 - 1696 Vol. 2, Washington, DC.
#   Bourgerie, R.W., Garner, T.L., Shih, H.H., (2002). Coastal current
#     measurements using an ADCP in a streamlined sub-surface mooring buoy,
#     MTS/IEEE OCEANS 2002, p 736 - 741 vol.2, Biloxi MS.
#
# Design guidance is also derived from CO-OPS SOP 3.2.3.6 (F10), Risk
#   Assessment and Criteria for Mount Selection for Current Meter Deployments,
#   03/22/2012
#==============================================================================
# CHANGELOG:
#   07/2016, TKM - Combined SUBS.R, SUBS_Classes.R, SUBS_Functions.R and
#                    CurrentMeterForces.R into one R file.
#                  Revised SUBS() to take .subs file as an argument
#                  Extended SUBS plots to 10 knots
#                  Revised code base to conform to R Programming Style Guide
#                  Renamed all functions and variables for improved context
#   09/2016, TKM - Renamed and added various elements
#   01/2017, TKM - Added AnalyzeAllSUBS function to bulk process .subs files
#                  Corrected RR Wheel weight to 600#
#                  Added 800# RR wheel weight variants
#   02/2017, TKM - Added deep water components from Risk Deep.xlsm
#                  Minor adjustments to component physical properties
#                  Made .subs filetype inclusion optional
#                  Added optional .subs file input kmaxspeed, assume NA if not 
#                    provided and should not plot vertical line
#                  Reworded variable output on plots for clarity
#                  Rewrote errors and messages
#                  Enhanced .subs file parsing
#==============================================================================
# TODO:
#   Investigate modeling dor-mor anchors
#   Merge with S4-based re-write of element properties
#   Rewrite plotting funcitons using ggplot2
#   Enhance filetype checking
#   Generalize TRBM functions
#   Integrate bottom platforms
#==============================================================================

#==============================================================================
# SUBS
#==============================================================================
# Requirements:
#   1) CART tension must be > 150 lb
#   2) drag force must ensure no anchor movement
#   3) mooring length and depression must keep ADCP within range of the surface
#         Frequency   Normal (m)
#         1200         15
#          600         40
#          300        110
#           75        600
#==============================================================================

#------------------------------------------------------------------------------
# Batch process all .subs files in current working directory and export png
#------------------------------------------------------------------------------
AnalyzeSurvey <- function() {
  allsubs <- Sys.glob('*.subs')
  if (length(allsubs) == 0) {
    stop('No .subs configuration files are present!')
  } else {
    for (i in 1:length(allsubs)) {
      suppressMessages(AnalyzeSUBS(allsubs[i]))
    }
  }
  dev.off()
}

#------------------------------------------------------------------------------
# Read in .subs file and compute/plot results
#------------------------------------------------------------------------------
# File: example.subs
# Example
# kfriction 0.4 kdepth 40
# Name          Element           Length
# A2+           SUBS_A2+          NA
# A2Chain       3/8_Steel_Chain   1.5
# B3            SUBS_B3           NA
# CART          CART              NA
# ChainBridle   3/8_Steel_Chain   0.3
# AnchorCable   5/16_Steel_Wire   3.
# Anchor        SmAnchor          NA
#------------------------------------------------------------------------------
# Typical Friction Values
# Sand          0.3
# Organics      0.3
# Rock          0.5
# Gravel        0.5
# Sandy Gravel  0.5
# Clay          0.3
# Mud           0.1
AnalyzeSUBS <- function(subs_file) {
  #subs_file <- 'example.subs'
  kcurrent_knots <- seq(0, 8, 0.5)
  plot_png       <- NULL
  plot           <- TRUE
  
  # If the '.subs' filetype was not passed, append it to the filename and try
  #   opening it. Should perform more robust checking to allow for variant text
  #   file types (.txt, .dat, etc.).
  if (substr(subs_file,
             nchar(subs_file) - 4,
             nchar(subs_file)) == '.subs') {
    subs_label <- substr(subs_file, 1, nchar(subs_file) - 5)
  } else {
    subs_label <- subs_file
    subs_file <- paste0(subs_file, '.subs')
  }
  
  # Read in the subs file and parse the header lines
  subs_list  <- ReadSUBSFile(paste0('./', subs_file))
  kstation   <- subs_list[['kstation']]
  kfriction  <- subs_list[['kfriction']]
  kdepth     <- subs_list[['kdepth']]
  kmaxspeed  <- subs_list[['kmaxspeed']]
  kpropadcp  <- subs_list[['kpropadcp']]
  subs_frame <- subs_list[['subs_frame']]
  
  # Parse the subs data and create Elements in element_list
  element_list <- list()
  for (i in 1:nrow(subs_frame)) {
    name   <- as.character(subs_frame[i, 'Name'])
    type   <- as.character(subs_frame[i, 'Element'])
    length <- as.numeric(subs_frame[i, 'Length' ])
    
    if (type == '1/2_Steel_Chain' ||
        type == '3/8_Steel_Chain' ||
        type == '1/4_Steel_Wire'  ||
        type == '5/16_Steel_Wire' ||
        type == '5/8_PP_Line') {
      element_list[[name]] <- new('Element',
                                  name           <- name,
                                  type           <- type,
                                  kcurrent_knots <- kcurrent_knots,
                                  length         <- length)
    } else {
      element_list[[name]] <- new('Element',
                                  name           <- name,
                                  type           <- type,
                                  kcurrent_knots <- kcurrent_knots)
    }
  }
  
  # Create a configuration string for the system
  subs_string <- ''
  for (i in 1:length(element_list)) {
    subs_string <- paste0(subs_string, element_list[[i]]@type)
    if (element_list[[i]]@type == '1/2_Steel_Chain' ||
        element_list[[i]]@type == '3/8_Steel_Chain' ||
        element_list[[i]]@type == '1/4_Steel_Wire'  ||
        element_list[[i]]@type == '5/16_Steel_Wire' ||
        element_list[[i]]@type == '5/8_PP_Line') {
      subs_string <- paste0(subs_string, ':', element_list[[i]]@length, 'm')
    }
    if (i < length(element_list)) {
      subs_string <- paste0(subs_string, '|')
    }
  }
  
  # Process the Elements
  # Assume element 1 is the top
  element_list[[1]] <- ComputeTopElement(element_list[[1]])
  # Assume elements are listed in order of depth
  for (i in 2:length(element_list)) {
    element_list[[i]] <- ComputeElement(element_list[[(i - 1)]],
                                        element_list[[i]])
  }
  
  # create png file
  if (!is.null(plot_png)) {
    png(filename  <- paste0('./', plot_png),
        width     <- 1000,
        height    <- 733,
        units     <- "px",
        pointsize <- 6,
        bg        <- "white",
        res       <- 300)
  }
  
  # Generate plots
  if (plot) {
    PlotSUBS(element_list,
             subs_string,
             kstation,
             kfriction,
             kdepth,
             kmaxspeed,
             kpropadcp)
  }
  
  if (!is.null(plot_png)) {
    dev.off()
  }

  dev.off(dev.copy(png,
                   filename = paste0(gsub(" ", "", subs_label, fixed = TRUE),
                                     '.png'),
                   width = 800,
                   height = 600))
  
  invisible(element_list)
}

#==============================================================================
#SUBS_Functions.R
#==============================================================================
# ComputeTopElement
# Remember that R 'classes' don't use references
# You need to reassign the elements or the whole object
#==============================================================================
ComputeTopElement <- function(element) {
  if (class(element)[1] != 'Element') {
    stop('element not of class Element')
  }
  
  if (!(element@type == 'DW_40' ||
        element@type == 'DW_49' ||
        element@type == 'SUBS_A2' ||
        element@type == 'SUBS_A2+' ||
        element@type == 'SUBS_D2' ||
        element@type == 'CROM')) {
    stop('Top element type not A2, D2 or CROM')
  }
  
  element@phi <- atan2(element@drag, element@buoyancy) #y = drag, x = buoyancy
  element@load_down <- element@buoyancy / cos(element@phi)
  return(element)
}

#------------------------------------------------------------------------------
# ComputeElement
#------------------------------------------------------------------------------
ComputeElement <- function(element_nminus1, element_n) {
  if (class(element_nminus1)[1] != 'Element' || 
      class(element_n)[1] != 'Element') {
    stop('element not of class Element')
  }
  
  if (sum(element_nminus1@load_down) < 0.1) {
    stop('Element ', element_nminus1@name, ' has no load_down')
  }
  
  element_n@load_up <- element_nminus1@load_down
  element_n@theta   <- element_nminus1@phi
  
  theta <- element_n@theta
  
  Dy <- element_n@drag + element_n@load_up * sin(theta)
  Bx <- element_n@buoyancy + element_n@load_up * cos(theta)
  
  element_n@phi       <- atan2(Dy, Bx) # y = Dy, x = Bx
  element_n@load_down <- Bx / (cos(element_n@phi))
  
  # depression is only computed for anchor and cable
  if (element_n@type == '1/2_Steel_Chain' ||
      element_n@type == '3/8_Steel_Chain' ||
      element_n@type == '1/4_Steel_Wire' ||
      element_n@type == '5/16_Steel_Wire' ||
      element_n@type == '5/8_PP_Line') {
    element_n@depression <- element_n@length - element_n@length *
      cos(element_n@phi)
  }
  return(element_n)
}


#------------------------------------------------------------------------------
# PlotElementListAngles
#------------------------------------------------------------------------------
PlotElementListAngles <- function(element_list) {
  # Find elements that have an angle
  angle_list  <- list()
  angle_names <- list()
  for (element in element_list) {
    if (element@type == '1/2_Steel_Chain' ||
        element@type == '3/8_Steel_Chain' ||
        element@type == '1/4_Steel_Wire'  ||
        element@type == '5/16_Steel_Wire' ||
        element@type == '5/8_PP_Line') {
      angle_list[[element@name]]  <- element
      angle_names[[element@name]] <- element@name
    }
  }
  if (length(angle_list) == 0) {
    stop('Failed to find any line elements')
  }
  
  angle_colors <- rainbow(length(angle_list))
  plot(angle_list[[1]]@kcurrent_knots,
       angle_list[[1]]@theta * 180 / pi,
       type = 'l',
       lwd  = 2,
       col  = angle_colors[1],
       xlab = 'Current (kts)',
       ylab = expression(paste0(theta, ', ', phi, ' (', plain(deg), ')')))
  lines(angle_list[[1]]@kcurrent_knots,
        angle_list[[1]]@phi * 180 / pi,
        lwd = 2,
        lty = 2,
        col = angle_colors[1])
  
  if (length(angle_list) > 1) {
    for (i in 2:length(angle_list)) {
      lines(angle_list[[i]]@kcurrent_knots,
            angle_list[[i]]@theta * 180 / pi,
            lwd = 2,
            col = angle_colors[i])
      lines(angle_list[[i]]@kcurrent_knots,
            angle_list[[i]]@phi * 180 / pi,
            lwd = 2,
            lty = 2,
            col = angle_colors[i])
    }
  }
  legend('topleft',
         legend = angle_names,
         lwd    = 2,
         col    = angle_colors,
         bty    = 'n')
}

#------------------------------------------------------------------------------
# PlotSUBS
#------------------------------------------------------------------------------
PlotSUBS <- function(element_list,
                     subs_string,
                     kstation  = NA,
                     kfriction = NA,
                     kdepth    = NA,
                     kmaxspeed = NA,
                     kpropadcp = NA) {

  # Accumulate Anchor Drag, Height, depression, buoyancy
  drag         <- 0.
  height       <- 0.
  depression   <- 0.
  buoyancy     <- 0.
  
  for (element in element_list) {
    drag       <- drag       + element@drag
    height     <- height     + element@length
    depression <- depression + element@depression
    buoyancy   <- buoyancy   + element@buoyancy
  }
  
  # hardware/shackles add 0.4 m in length, -10 lb buoyancy
  height   <- height   + 0.4
  buoyancy <- buoyancy - 10.
  
  # Define max sensor range
  # Frequency   Normal (m)
  # 1200         15
  #  600         40
  #  300        110
  #   75        600
  if (is.na(kpropadcp)) {
    kmaxrange = NA
  } else {
    if (kpropadcp == '75') {
      kmaxrange = 600
    } else if (kpropadcp == '300') {
      kmaxrange = 110
    } else if (kpropadcp == '600') {
      kmaxrange = 40
    } else if (kpropadcp == '1200') {
      kmaxrange = 15
    } else {
      kmaxrange = NA
    }
  }
  
  # Get copy of the CART
  for (element in element_list) {
    if (element@type == 'CART') {
      cart <- element
      break
    }
  }
  
  buoyant_force      <- buoyancy / 4.4482216 #convert newtons to pounds force
  anchor_slide_limit <- -buoyant_force * kfriction
  
  par(mfrow = c(2, 2))         # stacked plots
  par(mgp   = c(1.8, 0.5, 0))  # set margin line for axis title, labels, line
  par(mar   = c(3, 3, 1.3, 1)) # set margin widths in line units
  
  # Top Left Plot: CART tension
  plot(cart@kcurrent_knots,
       cart@load_up / 4.45,
       type = 'l',
       lwd  = 2,
       xlab = 'Current (kts)',
       ylab = 'CART Tension (lb)')
  abline(h = -buoyant_force, lty = 2, col = 'red')
  mtext(kstation,
        line = -1.5,
        cex = 1.3)
  mtext(paste0('Buoyancy ', round(buoyant_force, 0), ' lb'),
        line = -2.8,
        cex  = 0.8)
  abline(v = kmaxspeed, lty = 3, col = 'orange')
  abline(v = 1.2 * kmaxspeed, lty = 3, col = 'red')
  
  # Top Right Plot: Anchor Drag
  par(mar = c(3, 3, 1.3, 1)) # set margin widths in line units
  plot(element_list[[1]]@kcurrent_knots,
       drag / 4.45,
       type = 'l',
       lwd  = 2,
       xlab = 'Current (kts)',
       ylab = 'Anchor Drag (lb)')
  ## Configuration String Header
  mtext(subs_string,
        adj = 1,
        line = 0.25,
        cex = 0.8)
  mtext(bquote(~~~mu[s] == .(kfriction)),
        line = -1.5,
        adj = 0,
        cex = 0.8)
  mtext(paste0('  Slide Limit = ', round(anchor_slide_limit, 0), ' lb'),
        line = -2.5,
        adj  = 0,
        cex = 0.8)
  abline(h = anchor_slide_limit, lty = 2, lwd = 2, col = 'red')
  if (!is.na(kmaxspeed)) {
    mtext(paste0('  Design Speed = ', kmaxspeed, ' kts'),
          line = -3.5,
          adj  = 0,
          cex = 0.8)
    abline(v = kmaxspeed, lty = 3, col = 'orange')
    abline(v = 1.2 * kmaxspeed, lty = 3, col = 'red')
    }
  
  # Bottom Left Plot: Incline angle
  par(mar = c(3, 3, 1, 1)) # set margin widths in line units
  PlotElementListAngles(element_list)
  #abline(h = 20, lty = 2, col = 'red')
  abline(v = kmaxspeed, lty = 3, col = 'orange')
  abline(v = 1.2 * kmaxspeed, lty = 3, col = 'red')
  
  # Bottom Right Plot: Depression
  par(mar = c(3, 3, 1, 1)) # set margin widths in line units
  plot(element_list[[1]]@kcurrent_knots,
       depression,
       type = 'l',
       lwd  = 2,
       xlab = 'Current (kts)',
       ylab = 'Depression (m)')
  mtext(paste0('  Mooring Length = ', round(height, 1), ' m,',
              ' Bottom ', kdepth, ' m'),
        line = -1.3,
        adj = 0,
        cex = 0.8)
  abline(h = 0.5, lty = 3, col = 'grey')
  abline(h = 1.0, lty = 3, col = 'grey')
  abline(h = 2.0, lty = 3, col = 'grey')
  abline(h = 4.0, lty = 3, col = 'grey')
  abline(h = 8.0, lty = 3, col = 'grey')
  
  if (kdepth > height) {
    if (!is.na(kmaxrange)) {
      mtext(paste0('  ', kpropadcp, 'kHz ADCP Max = ', kmaxrange, ' m'),
            line = -2.5,
            adj  = 0,
            cex  = 0.8)
      headroom <- round(kmaxrange - (kdepth - height), 1)
      if (headroom >= 0) {
        if (headroom < kmaxrange) {
          test = '#006400'
        } else {
          test = '#8B0000'
        }
      } else {
        test = '#8B0000'
      }
      mtext(paste0('  Headroom @ Rest = ', headroom, ' m'),
            line = -3.5,
            adj  = 0,
            cex  = 0.8,
            col = test)
    }
  }
  if (!is.na(kmaxspeed)) {
    abline(v = kmaxspeed, lty = 3, col = 'orange')
    abline(v = 1.2 * kmaxspeed, lty = 3, col = 'red')
  }
}

#------------------------------------------------------------------------------
# ReadSUBSFile function
#------------------------------------------------------------------------------
ReadSUBSFile <- function(subs_file) {
  file <- readLines(subs_file)
  
  kstation <- file[1]
  message(kstation)

  vars <- strsplit(file[2], ' ')[[1]]
  if (any(grepl('kfriction', vars))) {
    kfriction <- as.numeric(vars[grep('kfriction', vars) + 1])
    message(paste0('kfriction = ', kfriction))
  } else {
    stop('kfriction variable is missing.')
  }
  
  if (any(grepl('kdepth', vars))) {
    kdepth <- as.numeric(vars[grep('kdepth', vars) + 1])
    message(paste0('kdepth    = ', kdepth, ' m'))
  } else {
    stop('kdepth variable is missing.')
  }

  if (any(grepl('kmaxspeed', vars))) {
    kmaxspeed <- as.numeric(vars[grep('kmaxspeed', vars) + 1])
    message(paste0('kmaxspeed = ', kmaxspeed, ' kts'))
  } else {
    kmaxspeed <- NA
    message('kmaxspeed not provided')
  } 

  if (any(grepl('kpropadcp', vars))) {
    kpropadcp <- as.numeric(vars[grep('kpropadcp', vars) + 1])
    message(paste0('kpropadcp = ', kpropadcp, ' kHz'))
  } else if (any(grepl('kpropsensor', vars))) {
    kpropadcp <- as.numeric(vars[grep('kpropsensor', vars) + 1])
    message(paste0('kpropadcp = ', kpropadcp, ' kHz'))
  } else {
    kpropadcp <- NA
    message('kpropadcp not provided')
  }

  # Table starting on line 3
  subs_frame <- read.table(subs_file, header = TRUE, quote = '', skip = 2)
  
  # Make sure there are no duplicate names since they are used
  # as keys in the element_list
  duplicates <- duplicated(subs_frame[, 1]) # TRUE / FALSE
  for (i in duplicates) {
    if (i) {
      stop('Element names must be unique. Duplicate names not allowed :\n',
           subs_frame[duplicates, 1])
    }
  }
  
  return(list(kstation   = kstation,
              kfriction  = kfriction,
              kdepth     = kdepth,
              kmaxspeed  = kmaxspeed,
              kpropadcp  = kpropadcp,
              subs_frame = subs_frame))
}

#==============================================================================
# SUBS_Classes.R
#==============================================================================
# class Element
#
# Represents a generic body with buoyancy, drag, a mooring
# line at angle phi and a leading line at angle theta.
#
#    buoyancy  load_up                |  /
#          |   /                      |-/ theta
#          |  /                       |/
#         XXXXXX---> drag            /|
#           /                  phi  /-|
#          /                       /  |
#     load_down
#
#==============================================================================
setClass("Element",
         representation(
           name           = "character",   # descriptive name
           type           = "character",   # A2, B3, Float, Chain, etc.
           buoyancy       = "numeric",     # (N)
           kcurrent_knots = "numeric",     # (knots)
           drag           = "numeric",     # (N)
           drag_coeff     = "numeric",     # (N / (m / s) ^ 2)
           load_up        = "numeric",     # line load above this Element
           theta          = "numeric",     # vertical angle of load_up
           load_down      = "numeric",     # line load below this Element
           phi            = "numeric",     # vertical angle of load_down
           length         = "numeric",     # length if chain/cable, else height
           depression     = "numeric",     # vertical depression
           Properties     = "data.frame"), # table of element properties
         prototype = list(
           name           = "None",
           type           = NULL,
           buoyancy       = 0.,
           kcurrent_knots = seq(0, 8, 0.5),
           drag           = 0.,
           drag_coeff     = 0.,
           load_up        = 0.,
           theta          = 0.,
           load_down      = 0.,
           phi            = 0.,
           length         = 0.,
           depression     = 0.,
           Properties     = data.frame()))

#------------------------------------------------------------------------------
# Element constructor
# It would be better to have an Element parent class
# and then subclasses of Element corresponding to each type.
#
# The arguments to initialize are .Object and ...
# initialize() is called from new(), not directly.
# The .Object argument is then the prototype object from the class.
#------------------------------------------------------------------------------
setMethod("initialize", signature("Element"),
          function(.Object,
                   name,
                   type,
                   kcurrent_knots = seq(0, 8, 0.5),
                   length = 0.) {
            .Object@name <- name
            # The type must be one of:
            types <- c('CROM',
                       'SUBS_A2',
                       'SUBS_A2+',
                       'SUBS_B3',
                       'SUBS_C2',
                       'SUBS_D2',
                       'Float',
                       'DW_40',
                       'DW_49',
                       'SBE37-SMP',
                       '1/2_Steel_Chain',
                       '3/8_Steel_Chain',
                       '1/4_Steel_Wire',
                       '5/16_Steel_Wire',
                       '5/8_PP_Line',
                       'CART',
                       'SmAnchor',
                       'Dual-SmAnchor',
                       'LgAnchor',
                       'Dual-LgAnchor',
                       'Trip-LgAnchor',
                       '50Lb',
                       '100Lb',
                       '200Lb',
                       '300Lb',
                       '500Lb',
                       '1MTon',
                       '2MTon',
                       '3MTon')
            # Ensure the new( 'Element' ) has a valid type
            for (t in types) {
              if (type == t) {
                .Object@type <- type
                break
              }
            }
            if (is.null(.Object@type)) {
              stop("Element [", name, "] constructor type: ", type)
            }
            
            #------------------------------------------------------------------
            # Properties Table
            #------------------------------------------------------------------
            .Object@Properties <- data.frame(
              cbind(
                #--------------------------------------------------------------
                # buoyancy ( N or N/m )
                #--------------------------------------------------------------
                c(320,    # CROM 80 lbs - 8 lb ADCP
                  320,    # Open Seas SUBS A2
                  490,    # Open Seas SUBS A2+
                  512,    # Open Seas SUBS B3
                  463,    # Open Seas SUBS C2
                  900,    # Open Seas SUBS D2
                  169,    # 14" Glass Sphere Float
                 2225,    # 40" Deep Water Sphere
                 3560,    # 49" Deep Water Sphere
                  -35,    # SBE37-SMP with strongback
                  -35.31, # 1/2" Steel Chain
                  -23.36, # 3/8" Steel Chain
                   -2.05, # 1/4" Steel Wire
                   -3.1,  # 5/16" Steel Wire
                  0.09,   # 5/8" Polypropylene line
                  -111,   # EdgeTech CART Release
                  -2669,  # 600# Railroad Wheel
                  -5338,  # 2x 600# Railroad Wheel
                  -3559,  # 800# Railroad Wheel
                  -7117,  # 2x 800# Railroad Wheel
                 -10677,  # 3x 800# Railroad Wheel
                  -222,   # 50 LB
                  -444,   # 100 LB
                  -889,   # 200 LB
                  -1334,  # 300 LB
                  -2224,  # 500 LB
                  -9804,  # 1 metric ton
                  -19608, # 2 metric tons
                  -29412),# 3 metric tons
                #--------------------------------------------------------------
                # drag_coeff ( N/(m/s)^2 ) actually c_d * A
                #--------------------------------------------------------------
                c(212.,  # CROM Cd = 0.47 (89 N/(m/s)^2) : 1.17 (212)
                  75.3,  # Open Seas SUBS A2
                  75.3,  # Open Seas SUBS A2+
                  63.5,  # Open Seas SUBS B3
                  63.5,  # Open Seas SUBS C2
                  100.,  # Open Seas SUBS D2
                  25.2,  # 14" Glass Sphere Float
                  259.1, # 40" Deep Water Sphere
                  318.8, # 49" Deep Water Sphere
                  3.17,  # SBE37-SMP
                  5.3,   # 1/2" Steel Chain (estimate based on 3/8" value)
                  4.0,   # 3/8" Steel Chain
                  3.17,  # 1/4" Steel Wire
                  3.97,  # 5/16" Steel Wire
                  4.0,   # 5/8" Polypropylene line (4.0 is a guess)
                  33.4,  # EdgeTech CART Release
                  0.,    # 600#  Railroad Wheel
                  0.,    # 2x 600#  Railroad Wheel
                  0.,    # 800#  Railroad Wheel
                  0.,    # 2x 800#  Railroad Wheel
                  0.,    # 3x 800# Railroad Wheel
                  0.,    # 50 LB
                  0.,    # 100 LB
                  0.,    # 200 LB
                  0.,    # 300 LB
                  0.,    # 500 LB
                  0.,    # 1 metric ton
                  0.,    # 2 metric tons
                  0.),   # 3 metric tons
                #--------------------------------------------------------------
                # height (m)
                #--------------------------------------------------------------
                c(0.58 - 0.9, # CROM - CART (CART is inside CROM)
                  0.7,        # Open Seas SUBS A2
                  0.7,        # Open Seas SUBS A2+
                  0.7,        # Open Seas SUBS B3
                  0.8,        # Open Seas SUBS C2 (0.8 is a guess)
                  0.8,        # Open Seas SUBS D2
                  0.6,        # 14" Glass Sphere Float
                  1.02,       # 40" Deep Water Sphere
                  1.25,       # 49" Deep Water Sphere
                  0.,         # SBE37-SMP
                  0.,         # 1/2" Steel Chain
                  0.,         # 3/8" Steel Chain
                  0.,         # 1/4" Steel Wire
                  0.,         # 5/16" Steel Wire
                  0.,         # 5/8" Polypropylene line
                  0.9,        # EdgeTech CART Release
                  0.3,        # 600# Railroad Wheel
                  0.3,        # Dual 600# Railroad Wheel
                  0.3,        # 800# Railroad Wheel
                  0.3,        # Dual 800# Railroad Wheel
                  0.3,        # 3x 800# Railroad Wheel
                  0.2,        # 50 LB
                  0.2,        # 100 LB
                  0.2,        # 200 LB
                  0.2,        # 300 LB
                  0.2,        # 500 LB
                  0.5,        # 1 metric ton
                  0.5,        # 2 metric tons
                  0.5)),      # 3 metric tons
              row.names = types)
            names(.Object@Properties) <- c('buoyancy', 'drag_coeff', 'height')
            
            # Set the length (which is the object height if not a line)
            if (type == '1/2_Steel_Chain' ||
                type == '3/8_Steel_Chain' ||
                type == '1/4_Steel_Wire'  ||
                type == '5/16_Steel_Wire' ||
                type == '5/8_PP_Line') {
              if (length <= 0. || length > 50) {
                stop("Element [", name, "] constructor length: ", length)
              }
              .Object@length <- length
            } else {
              .Object@length <- .Object@Properties[type, 'height']
              if (is.null(.Object@length)) {
                stop("Element [", name, "] constructor height not found")
              }
            }
            .Object@kcurrent_knots <- kcurrent_knots
            .Object@buoyancy       <- GetBuoyancy(.Object)
            .Object@drag_coeff     <- GetDragCoeff(.Object)
            .Object@drag           <- ComputeDrag(.Object)
            return(.Object)
})

#------------------------------------------------------------------------------
# Element GetBuoyancy method
#------------------------------------------------------------------------------
setGeneric("GetBuoyancy", function(self) {
  standardGeneric("GetBuoyancy")
})

setMethod("GetBuoyancy", signature("Element"), function(self) {
            buoyancy <- self@Properties[self@type, 'buoyancy']
            if (is.null(buoyancy)) {
              stop("ComputeBuoyancy [", self@name,
                   "] Properties type ", self@type)
            }
            if (self@type == '1/2_Steel_Chain' ||
                self@type == '3/8_Steel_Chain' ||
                self@type == '1/4_Steel_Wire' ||
                self@type == '5/16_Steel_Wire' ||
                self@type == '5/8_PP_Line') {
              buoyancy <- buoyancy * self@length
            }
            return(buoyancy)
})

#------------------------------------------------------------------------------
# Element GetDragCoeff method
#------------------------------------------------------------------------------
setGeneric("GetDragCoeff", function(self) {
  standardGeneric("GetDragCoeff")
})

setMethod("GetDragCoeff", signature("Element"), function(self) {
            drag_coeff <- self@Properties[self@type, 'drag_coeff']
            if (is.null(drag_coeff)) {
              stop("GetDragCoeff [",
                   self@name,
                   "] Properties type ",
                   self@type)
            }
            return(drag_coeff)
})

#------------------------------------------------------------------------------
# Element ComputeDrag method
#------------------------------------------------------------------------------
setGeneric("ComputeDrag", function(self) {
  standardGeneric("ComputeDrag")
})

setMethod("ComputeDrag", signature("Element"), function(self) {
            # Convert current from knots to m/s
            kcurrent.mps <- self@kcurrent_knots * 1852 / 3600 # Knots to m/s
            if (self@type == '1/2_Steel_Chain' ||
                self@type == '3/8_Steel_Chain' ||
                self@type == '1/4_Steel_Wire'  ||
                self@type == '5/16_Steel_Wire' ||
                self@type == '5/8_PP_Line') {
              drag <- self@length * self@drag_coeff * kcurrent.mps ^ 2
            } else {
              drag <- self@drag_coeff * kcurrent.mps ^ 2
            }
            return(drag)
})

#==============================================================================
# CurrentMeterForces.R
#==============================================================================
# Estimate sliding forces on a TRBM-style housing.
#
# The drag coefficient for a streamlined half body (a body with one
# side attached flush to an infinite plane) is roughly twice that
# of a fully immersed body. For a streamlined shape these values
# are roughly 0.09 (half body) and 0.04 (foil). Since the TRBM shape
# is approximately streamlined, a value of 0.2 is a reasonable start.
#
# Static Friction Coefficients
# Rubber to Wet Asphalt   0.25 - 0.75  mean = 0.5
# Rubber to Wet Concrete  0.45 - 0.75  mean = 0.6
#

#------------------------------------------------------------------------------
# Estimate transverse force on a ROMOR C-ROM mount.
#------------------------------------------------------------------------------
CROMForce = function(weightSW      = 18,  # lbs 
                     anchor        = 100, # lbs
                     buoyant       = 80,  # lbs
                     diameter      = 25,  # inches
                     height        = 23,  # inches
                     drag_coeff    = 0.47,
                     frictionCoeff = 0.5,
                     current       = seq(0.2, 3, 0.05), # knots
                     plot          = TRUE,
                     plot_png      = NULL,
                     title         = 'C-ROM Bottom Mount') {
  
  # projected area to flow in ft^2
  projectedArea <- (diameter * height) / 144
  
  # convert current in knots to current in ft/s
  # 1 foot/second = 0.5924838013 knot
  current_fps <- current * 0.5924838013
  
  # Compute the fluid force on the projected area
  # seawater density is 64 lb/ft^3
  currentForce <- drag_coeff * 64 * current_fps ^ 2 * projectedArea / 2
  
  # Compute the bottom drag force
  # total weight in lbs
  weight          <- weightSW + anchor - buoyant
  bottomDragForce <- frictionCoeff * weight
  
  # Find the intersection of the currentForce & bottomDragForce
  max_x = which(currentForce > bottomDragForce)
  if (length(max_x) > 0 && max_x[1] > 1) {
    maxCurrent <- current[(max_x[1] - 1)]
  } else {
    maxCurrent <- current[length(current)]
  }
  message(paste0('Max current allowed ', maxCurrent, ' knots'))
  
  #----------------------------------------------------------------------------
  if (plot) {
    if (!is.null(plot_png)) {
      cex <- 0.9
      png(filename = plot_png,
          width    = 5,
          height   = 4,
          units    = 'in',
          bg       = "white",
          res      = 600)
      par(mar = c(3, 3, 3, 2) + 0.1) # c(5, 4, 4, 2) + 0.1 )
      par(mgp = c(1.8, 0.5, 0)) # c(3, 1, 0) axis title, labels and line
    } else {
      cex = 1.3
      par(mar = c(3, 3, 3, 2) + 0.1) # c(5, 4, 4, 2) + 0.1 )
      par(mgp = c(1.8, 0.5, 0)) # c(3, 1, 0) axis title, labels and line
    }
    
    plot(current,
         currentForce,
         type     = 'l',
         lwd      = 3,
         xlab     = 'Current Speed (kts)',
         ylab     = 'Fluid Force (lbs)',
         cex.axis = cex,
         cex.lab  = cex)
    
    abline(v = seq(current[1], current[length(current)], 0.2), col = 'gray')
    abline(h = bottomDragForce, col = 'blue', lwd = 3)
    abline(v = maxCurrent, lwd = 2, lty = 2, col = 'red')
    
    mtext(side = 3,
          paste0('  ', title),
          line = 1.7,
          cex  = cex)
    mtext(side = 3,
          paste0('   Anchor ', round(anchor, 0), ' lbs.'),
          line = 0.4,
          cex  = cex)
    mtext(side = 3,
          bquote(~~~C[d] == .(drag_coeff)),
          line = -7,
          cex  = cex,
          adj  = 0)
    mtext(side = 3,
          bquote(~~~mu[s] == .(frictionCoeff)),
          line = -8.3,
          cex  = cex,
          adj  = 0)
    
    legText = c('Current Force', 'Bottom Drag')
    legCol  = c('black', 'blue')
    
    legend('topleft',
           legend = legText,
           lwd    = c(5, 5, 5),
           col    = legCol,
           cex    = cex,
           bg     = 'white')
    
    if (!is.null(plot_png)) {
      dev.off()
    }
  }
  # Return a data frame table
  invisible(data.frame(current_kt    = current,
                       force_lb      = currentForce,
                       bottomDrag_lb = bottomDragForce))
}

#------------------------------------------------------------------------------
# Estimate transverse force on a Flotation Technologies TRBM mount.
# The plan view is a square, so the side slopes are all the same.
# The cross-section is considered trapezoidal.
#------------------------------------------------------------------------------
TRBMForce <- function(weightSW      = 198, # lbs
                      addWeight     = 0,   # lbs
                      buoyant       = 0,   # lbs
                      bottomWidth   = 72,  # inches
                      topWidth      = 24,  # inches
                      height        = 18,  # inches
                      drag_coeff    = 0.2,
                      frictionCoeff = 0.5,
                      slope         = NULL, # If NULL, compute from dimensions
                      current       = seq(0.2, 5, 0.05), # knots
                      plot          = TRUE,
                      plotDownForce = FALSE,
                      plot_png      = NULL,
                      title         = 'TRBM Bottom Mount') {
  if (is.null(slope)) {
    slope <- atan2(height, (bottomWidth - topWidth) / 2)
  }
  # message( paste('Slope of side is', round( 180/pi * slope, 1 ), 'degrees.'))
  
  # projected area to flow in ft^2
  projectedArea <- (topWidth * height + 
                      height * (bottomWidth - topWidth) / 2) / 144
  # convert current in knots to current in ft/s
  # 1 foot/second = 0.5924838013 knot
  current_fps <- current * 0.5924838013
  
  # Compute the fluid force on the TRBM projected area
  # seawater density is 64 lb/ft^3
  currentForce <- drag_coeff * 64 * current_fps ^ 2 * projectedArea / 2
  
  # Estimate the downward component of force from the slope of the mount
  # This is a guesstimate for slopes near 45 degrees
  downForce <- 0.8 * currentForce * cos(slope)
  
  # The downForce is a component of the total currentForce
  # but doesn't contribute to moving the platform sideways
  # currentMinusDownForce = currentForce - downForce
  
  # Compute the bottom drag force
  # total weight in lbs
  weight <- weightSW + addWeight + downForce - buoyant
  bottomDragForce <- frictionCoeff * weight
  
  # Find the intersection of the currentForce & bottomDragForce
  max_x = which(currentForce > bottomDragForce)
  if (length(max_x) > 0 && max_x[1] > 1) {
    maxCurrent = current[(max_x[1] - 1)]
  } else {
    maxCurrent = current[length(current)]
  }
  message(paste0('Max current allowed ', maxCurrent, ' knots'))
  
  #----------------------------------------------------------------------------
  if (plot) {
    if (!is.null(plot_png)) {
      cex = 0.9
      png(filename = plot_png,
          width  = 5,
          height = 4,
          units  = 'in',
          bg     = "white",
          res    = 600)
      par(mar = c(3, 3, 3, 2) + 0.1) # c(5, 4, 4, 2) + 0.1 )
      par(mgp = c(1.8, 0.5, 0)) # c(3, 1, 0) axis title, labels and line
    } else {
      cex = 1.3
    }
    plot(current,
         currentForce,
         type     = 'l',
         lwd      = 3,
         xlab     = 'Current Speed (kts)',
         ylab     = 'Fluid Force (lbs)',
         cex.axis = cex,
         cex.lab  = cex )
    
    abline(v = seq(current[1], current[ length(current) ], 0.2), col = 'gray')
    lines(current, bottomDragForce, col = 'blue', lwd = 3)
    
    abline(v = maxCurrent, lwd = 2, lty = 2, col = 'red')
    
    if (plotDownForce) {
      lines(current, downForce, col = 'red', lwd = 3)
    }
    
    mtext(side = 3,
          paste0('  ', title),
          line = 1.7,
          cex  = cex)
    mtext(side = 3,
          paste0('   Additional Weight ', round(addWeight, 0),' lbs.'),
          line = 0.4,
          cex  = cex)
    mtext(side = 3,
          bquote(~~~C[d] == .(drag_coeff)),
          line = -7,
          cex  = cex,
          adj  = 0)
    mtext(side = 3,
          bquote(~~~mu[s] == .(frictionCoeff)),
          line = -8.3,
          cex  = cex,
          adj  = 0)
    
    if (plotDownForce) {
      legText <- c('Current Force', 'Down Force', 'Bottom Drag')
      legCol  <- c('black', 'red', 'blue')
    } else {
      legText <- c('Current Force', 'Bottom Drag')
      legCol  <- c('black', 'blue')
    }
    legend('topleft',
           legend = legText,
           lwd    = c(5, 5, 5),
           col    = legCol,
           cex    = cex,
           bg     = 'white')
    
    if (!is.null(plot_png)) {
      dev.off()
    }
  }
  
  # Return a data frame table
  invisible(data.frame(current_kt    = current,
                       force_lb      = currentForce,
                       down_lb       = downForce,
                       bottomDrag_lb = bottomDragForce))
}

#------------------------------------------------------------------------------
# Estimate transverse force on a non-symmetric MSI MTRBM type mount.
# The plan view is a rectangle, so the side slopes are not the same
# nor is the projected area.
# Assumes a standard 60 lb 'disk' weight included in weightSW
#------------------------------------------------------------------------------
MTRBMForce <- function(weightSW      = 120,  # lbs
                       addWeight     = 0,    # lbs
                       buoyant       = 0,    # lbs
                       bottomWidth   = 70,   # inches
                       topWidth      = 38,   # inches
                       height        = 18.5, # inches
                       drag_coeff    = 0.2,
                       frictionCoeff = 0.5,
                       slope         = 49 * pi / 180, # radians
                       current       = seq(0.2, 5, 0.05), # knots
                       plot          = TRUE,
                       plotDownForce = FALSE,
                       plot_png      = NULL,
                       title         = 'MTRBM Bottom Mount with 60 lb disk') {
  currentForceTable <- TRBMForce(weightSW      = weightSW,
                                 addWeight     = addWeight,
                                 buoyant       = buoyant,
                                 bottomWidth   = bottomWidth,
                                 topWidth      = topWidth,
                                 height        = height,
                                 drag_coeff    = drag_coeff,
                                 frictionCoeff = frictionCoeff,
                                 slope         = slope,
                                 current       = current,
                                 plot          = plot,
                                 plotDownForce = plotDownForce,
                                 plot_png      = plot_png,
                                 title         = title)
  invisible(currentForceTable)
}

#------------------------------------------------------------------------------
# Estimate transverse force on a non-symmetric MSI uTRBM type mount.
# The plan view is a rectangle, so the side slopes are not the same
# nor is the projected area.
#------------------------------------------------------------------------------
uTRBMForce <- function(weightSW      = 40,   # lbs
                       addWeight     = 0,    # lbs
                       buoyant       = 0,    # lbs
                       bottomWidth   = 51,   # inches
                       topWidth      = 26,   # inches
                       height        = 12.5, # inches
                       drag_coeff    = 0.2,
                       frictionCoeff = 0.5,
                       slope         = 44.7 * pi / 180, # radians
                       current       = seq(0.2, 5, 0.05), # knots
                       plot          = TRUE,
                       plotDownForce = FALSE,
                       plot_png      = NULL,
                       title         = 'Micro MTRBM Bottom Mount') {
  currentForceTable <- TRBMForce(weightSW      = weightSW,
                                 addWeight     = addWeight,
                                 buoyant       = buoyant,
                                 bottomWidth   = bottomWidth,
                                 topWidth      = topWidth,
                                 height        = height,
                                 drag_coeff    = drag_coeff,
                                 frictionCoeff = frictionCoeff,
                                 slope         = slope,
                                 current       = current,
                                 plot          = plot,
                                 plotDownForce = plotDownForce,
                                 plot_png      = plot_png,
                                 title         = title)
  invisible(currentForceTable)
}

#------------------------------------------------------------------------------
# Estimate transverse force on a non-symmetric ES2 type mount.
# The plan view is a rectangle.
# Table 4.  Specifications For ES II
# Shape/Size: Trapezoidal-shaped with bottom plate 79" x 64" x 28" high
# Weight in Air:   450 lbs. (Shell only)
# Weight in Water: 100 lbs.(Platform) and 200-400 lbs. (Lead)
# Popup Float:     15" overall dimension; net buoyancy ~ 57 lbs.
# Retrieval Line:     Spectra .25" x 250' rope
# Standard Use Depth: 200 meters
# Workload:           1200 lbs. with 1 knot current
#------------------------------------------------------------------------------
ES2Force <- function(weightSW      = 250, # lbs    : From Sprenke risk.xls
                     addWeight     = 200, # lbs    : From Sprenke risk.xls
                     buoyant       = 0,   # lbs
                     bottomWidth   = 82,  # inches : From Sprenke risk.xls
                     topWidth      = 35,  # inches : From Sprenke risk.xls
                     height        = 35,  # inches : From Sprenke risk.xls
                     drag_coeff    = 0.2,
                     frictionCoeff = 0.5,
                     slope         = 45 * pi / 180, # radians
                     current       = seq(0.2, 5, 0.05), # knots
                     plot          = TRUE,
                     plotDownForce = FALSE,
                     plot_png      = NULL,
                     title         = 'ES2 Bottom Mount') {
  currentForceTable <- TRBMForce(weightSW      = weightSW,
                                 addWeight     = addWeight,
                                 buoyant       = buoyant,
                                 bottomWidth   = bottomWidth,
                                 topWidth      = topWidth,
                                 height        = height,
                                 drag_coeff    = drag_coeff,
                                 frictionCoeff = frictionCoeff,
                                 slope         = slope,
                                 current       = current,
                                 plot          = plot,
                                 plotDownForce = plotDownForce,
                                 plot_png      = plot_png,
                                 title         = title)
  invisible(currentForceTable)
}
