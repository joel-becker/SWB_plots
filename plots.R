#----------------------------------------------------------------------------------#
# Purpose: Plots data for Yotam's charts
# Author: Joel Becker
# Date: 08/15/2019

# Notes:
#
#----------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
packages <- c("readstata13", "dplyr", "tidyr", "ggplot2", "gridExtra", "grid", "gtable")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# source and set directory
source("directory_paths.R")
dir <- paste0(maindir, "/data/META/")
setwd(data_dir)

# colour pallette for use throughout
colours <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")
# make sure to use colorbrewer2 for colour schemes:
# http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=8


########################################################
####################### Read data ######################
########################################################

data_fig1a <- read.dta13("Figure_1_A.dta")
data_fig1b <- read.dta13("Figure_1_B.dta")
data_fig1c <- read.dta13("Figure_1_C.dta") %>% mutate(scale_name = scale)
data_fig1d <- read.dta13("Figure_1_D.dta")


########################################################
##################### Order scales #####################
########################################################

scales_fig1a <- c(
  "Volunteering, activism",
  "Social status",
  "Work & work relationships",
  "Quality of environment",
  "Safe in neighborhood",
  "Hobbies & leisure",
  "Relationships w/friends",
  "Possibilities in life",
  "Live personal values",
  "Purpose & meaning",
  "Security re life & future",
  "Ment. health & emo. life",
  "Physical health",
  "Family life & relationships",
  "Income and fin. security"
)

scales_fig1c <- c(
  "%(Larger Group>0)",
  "Larger Group"
)

scales_fig1d <- c(
  "World",
  "Country",
  "Community",
  "Friends",
  "Other relatives",
  "Immediate family",
  "Yourself"
)


data_fig1a$scale_name <- factor(
  data_fig1a$scale_name,
  levels = scales_fig1a
)

data_fig1c$scale_name <- factor(
  data_fig1c$scale_name,
  levels = scales_fig1c
)

data_fig1d$scale_name <- factor(
  data_fig1d$scale_name,
  levels = scales_fig1d
)


########################################################
############ Functions for producing graphs ############
########################################################

joel.theme <- function(legend = TRUE, title_size = 10, hjust = 0.5) {

  # core theme for all plots

  theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = hjust, size = title_size),
      plot.margin = unit(c(0, 0, 0, 0), "mm"),
      axis.ticks = element_blank()
    )

  if (legend == TRUE) {
    theme <- theme +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank()
      )
  }

  return(theme)

}

select.colour.data <- function(data, select_scales) {

  # selects columns whose data will be coloured in

  data_selectcolours <- filter(data, Q_id_name %in% select_scales) # data to be coloured in

  return(data_selectcolours)

}

figure.one.layout <- function(data,
                              data_selectcolours,
                              selectcolours,
                              title,
                              error_width = 0.1,
                              ylim_lower = 30,
                              ylim_upper = 80,
                              show_lines = TRUE,
                              show_title = TRUE,
                              y_axis_text = TRUE) {

  # core layout of figure one

  layout <- ggplot(
      data = data_selectcolours,
      aes(
        x = scale_name,
        y = mean,
        colour = Q_id_name,
        group = Q_id_name
      )
    ) +

    # gray features
    geom_errorbar(
      data = data,
      aes(
        ymin = mean - se,
        ymax = mean + se
      ),
      width = error_width,
      colour = "gray90"
    )
  if (show_lines == TRUE) {
    layout <- layout +
      geom_line(
        data = data,
        colour = "gray90"
      )
  }
  layout <- layout +
    geom_point(
      data = data,
      colour = "gray90"
    ) +

    # coloured features
    geom_errorbar(
      aes(
        ymin = mean - se,
        ymax = mean + se
      ),
      width = error_width
    )
  if (show_lines == TRUE) {
    layout <- layout +
      geom_line()
  }
  layout <- layout +
    geom_point() +

    # plot format
    ylim(
      ylim_lower,
      ylim_upper
    ) +
    xlab("") +
    ylab("")
  if (show_title == TRUE) {
    layout <- layout +
      ggtitle(title)
  }
  layout <- layout +
    coord_flip() +
    scale_color_manual(values = selectcolours) +
    joel.theme()
  if (y_axis_text == FALSE) {
    layout <- layout +
      theme(axis.text.y = element_blank())
  }

  return(layout)

}

plot.fig1a <- function(panel1, panel2, panel3) {

  # arranges panels from fig 1c

  grob <- arrangeGrob(
    nullGrob(),
    title.row("(A) Life domains"),
    nullGrob(),

    title.row("All SWB questions", hjust = 0, fontsize = 10, main = FALSE),
    title.row("Ladder, Life Satisfaction, Happiness", fontsize = 10, main = FALSE),
    title.row("Options & Possibilities, Meaning & Value", fontsize = 10, main = FALSE),

    panel1 + theme(legend.position="none"),
    panel2 + theme(legend.position="none"),
    panel3 + theme(legend.position="none"),

    nrow = 3,
    heights = c(1, 1, 9),
    widths = c(4, 3, 3)
  )

  return(grob)

}

plot.fig1c.subpanel <- function(data_selectcolours, scale_names, selectcolours, title, y_axis_text = T, show_title = F) {

  # produces upper subpanel from fig 1c

  plot <- figure.one.layout(
    data = data_fig1c,
    data_selectcolours = select.colour.data(data_selectcolours, scale_names),
    selectcolours = selectcolours,
    title = title,
    ylim_lower = 0,
    ylim_upper = 100,
    show_lines = FALSE,
    show_title = show_title,
    y_axis_text = y_axis_text
  )

  return(plot)

}

plot.fig1d.subpanel <- function(data_selectcolours, scale_names, selectcolours, y_axis_text = T) {

  # produces lower subpanel from fig 1c

  plot <- figure.one.layout(
    data = data_fig1d,
    data_selectcolours = select.colour.data(data_selectcolours, scale_names),
    selectcolours = selectcolours,
    title = "",
    show_title = FALSE,
    y_axis_text = y_axis_text
  )

  return(plot)

}

plot.fig1c.panel <- function(plot1 = fig1_c1, plot2 = fig1_d1, grob_widths = c(3.7, 10, 0.3), bottom_widths = c(0.2, 9.8)) {

  # creates grob for individual panel of fig 1c

  top <- arrangeGrob(
    plot1 + theme(legend.position="none", axis.text.x=element_blank()),
    nrow = 1,
    widths = 10
  )

  line <- grob.separator(grob_widths = grob_widths)

  bottom <- arrangeGrob(
    nullGrob(),
    plot2 + theme(legend.position="none"),
    nrow = 1,
    widths = bottom_widths
  )

  grob <- arrangeGrob(
    top,
    line,
    bottom,
    nrow = 3,
    heights = c(3, 0.5, 10)
  )

  return(grob)

}

plot.fig1c <- function(panel1, panel2, panel3) {

  # arranges panels from fig 1c

  grob <- arrangeGrob(
    nullGrob(),
    nullGrob(),
    nullGrob(),
    nullGrob(),

    nullGrob(),
    nullGrob(),
    title.row("(C) Social circles"),
    nullGrob(),

    nullGrob(),
    nullGrob(),
    nullGrob(),
    title.row("Family Well-Being, Personal Well-Being", fontsize = 10, main = FALSE),

    nullGrob(),
    panel1,
    panel2,
    panel3,

    nrow = 4,
    heights = c(0.5, 1, 0.5, 9),
    widths = c(0.3, 5, 4, 4)
  )

  return(grob)

}

title.row <- function(title, fontsize = 20, widths = c(6, 8, 6), main = TRUE, hjust = NULL) {

  # creates grob object of title row

  if (main == TRUE) { # if main title
    title_row <- arrangeGrob(
      nullGrob(),
      textGrob(
        title,
        gp = gpar(fontsize = fontsize, font = 8),
        hjust = hjust
      ),
      nullGrob(),
      nrow = 1,
      widths = widths
    )
  } else { # else if sub-title
    title_row <- arrangeGrob(
      textGrob(
        title,
        gp = gpar(fontsize = fontsize, font = 8),
        hjust = hjust
      ),
      nrow = 1
    )
  }

  return(title_row)
}

draw.separator <- function(line_width = 2, margins) {

  # draws separator in ggplot

  separator <- ggplot(
      data = data.frame(x = 0:100, y = 0),
      aes(x = x, y = y)
    ) +
    geom_line(linetype = "dashed", colour = "gray", width = line_width) +
    theme(
      line = element_blank(),
      text = element_blank(),
      title = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(margins, "cm")
    )

  return(separator)

}

grob.separator <- function(line_width = 2, margins = c(0, 0, 0.5, 0), grob_widths = c(3.7, 10, 0.3)) {

  # produces separator as a grob

  line_grob <- arrangeGrob(
    nullGrob(),
    draw.separator(margins = margins),
    nullGrob(),
    nrow = 1,
    widths = grob_widths
  )

  return(line_grob)

}

combined.legend <- function(plot) {

  # extracts combined legend

  temp <- ggplot_gtable(ggplot_build(plot))
  legend <- which(sapply(temp$grobs, function(x) x$name) == "guide-box")
  legend <- temp$grobs[[legend]]

  return(legend)

}

plot.fig1 <- function(row1, row3, legend) {

  # arranges rows of grobs into full figure 1

  grob <- arrangeGrob(
    row1,
    row3,
    legend,
    nrow = 3,
    heights = c(9, 9, 1)
  )

  return(grob)

}

########################################################
############ Sub-panels for Figure 1 Panel A ###########
########################################################

fig1_a1 <- figure.one.layout(
  data = data_fig1a,
  data_selectcolours = select.colour.data(data_fig1a, unique(data_fig1a$Q_id_name)),
  selectcolours = colours,
  show_title = FALSE
)

fig1_a2 <- figure.one.layout(
  data = data_fig1a,
  data_selectcolours = select.colour.data(data_fig1a, c("Life Satisfaction", "Ladder", "Happiness")),
  selectcolours = colours[c(3, 4, 5)],
  y_axis_text = FALSE,
  show_title = FALSE
)

fig1_a3 <- figure.one.layout(
  data = data_fig1a,
  data_selectcolours = select.colour.data(data_fig1a, c("Options & Possibilities", "Meaning & Value")),
  selectcolours = colours[c(6, 7)],
  y_axis_text = FALSE,
  show_title = FALSE
)


########################################################
#### Sub-panels for Figure 1 Panel C (with two axes) ###
########################################################

fig1_c1 <- plot.fig1c.subpanel(
  data_selectcolours = data_fig1c,
  scale_names = unique(data_fig1c$Q_id_name),
  selectcolours = colours
)

fig1_d1 <- plot.fig1d.subpanel(
  data_selectcolours = data_fig1d,
  scale_names = unique(data_fig1d$Q_id_name),
  selectcolours = colours
)


fig1_c2 <- plot.fig1c.subpanel(
  data_selectcolours = data_fig1c,
  scale_names = c("Life Satisfaction", "Ladder", "Happiness"),
  selectcolours = colours[c(3, 4, 5)],
  y_axis_text = FALSE
)

fig1_d2 <- plot.fig1d.subpanel(
  data_selectcolours = data_fig1d,
  scale_names = c("Life Satisfaction", "Ladder", "Happiness"),
  selectcolours = colours[c(3, 4, 5)],
  y_axis_text = FALSE
)


fig1_c3 <- plot.fig1c.subpanel(
  data_selectcolours = data_fig1c,
  scale_names = c("Family Well-Being", "Personal Well-Being"),
  selectcolours = colours[c(2, 1)],
  y_axis_text = FALSE
)

fig1_d3 <- plot.fig1d.subpanel(
  data_selectcolours = data_fig1d,
  scale_names = c("Family Well-Being", "Personal Well-Being"),
  selectcolours = colours[c(2, 1)],
  y_axis_text = FALSE
)


fig1_cd1 <- plot.fig1c.panel(fig1_c1, fig1_d1, grob_widths = c(3, 10, 0))
fig1_cd2 <- plot.fig1c.panel(fig1_c2, fig1_d2, grob_widths = c(0.5, 10, 0), bottom_widths = c(0, 9.8))
fig1_cd3 <- plot.fig1c.panel(fig1_c3, fig1_d3, grob_widths = c(0.5, 10, 0), bottom_widths = c(0, 9.8))


########################################################
########## Combine Figure 1 panels; plot; save #########
########################################################

fig1_a <- plot.fig1a(fig1_a1, fig1_a2, fig1_a3)
fig1_c <- plot.fig1c(fig1_cd1, fig1_cd2, fig1_cd3)

fig1 <- plot.fig1(fig1_a, fig1_c, combined.legend(fig1_a1))

grid.arrange(fig1)
ggsave("Figure_1.png", fig1, width=16, height=12)
