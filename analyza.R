### Data manipulation:
### Stochl too high observations deleted
2863/30
## Libraries
rm(list=ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
## Paths
path_project <- "d:/04_praca/Konzultacie/LukasKasala/"
setwd(path_project)
print(paste0("current project path : " , getwd()))

path_data_control_csv <-"data_kontrolna_csv"
path_data_control_xlsx <-"data_kontrolna_xlsx"
path_data_hockey_players <- "hokejisti"
### Output directory #

dir_output <- "outputs"
dir.create(dir_output, showWarnings = FALSE)

###### Functions read data and merge data #####

read_data_ <- function(path_){
  ### Reads data expected in the same csv format in the same directory
  ### Replaces "," for "." for decimals
  ### Checks all observations are read
  return_extension<- function(file_){
    return(unlist(strsplit(file_, "\\."))[[2]])
  }
  read_csv_ <- function(path_){
    return(read.table(text = gsub(",", ".", readLines(path_)), sep = ";", dec=".", header=TRUE))
  }
  read_xlsx_ <- function(path_){
    return(read_excel(path_))
  }
  
  data_files <- list.files(path_)
  files_num <- length(data_files)
  correct_number_of_rows <- function(x) return(x==files_num*8)
  data_ <- data.frame()
  for (file_n in c(1:files_num)){
    file_ <- data_files[file_n]
    file_extension <- return_extension(file_)
    print(paste0(path_,"/",file_))
    if(file_extension == "csv"){
      data_ <- read_csv_(paste0(path_,"/",file_))
    } else if (file_extension == "xlsx"){
      data_ <- read_xlsx_(paste0(path_, "/", file_))
    }else{
        data_<- NULL
        stop("File extension unkknown")
      }
    
    if (file_n ==1){
      data_output<-data_
    }
    else{
      if (!("Subject" %in% names(data_))){data_$Subject<-file_}
      common_cols <- intersect(names(data_output), names(data_))
      data_output <- rbind(data_output[,c(common_cols)],
                            data_[,c(common_cols)] )
    }
    print(dim(data_output))
  }
  
  rows_ <- dim(data_output)[1]
  #stopifnot(correct_number_of_rows(rows_))
  print(paste0("OK: Merged data in ", path_, " rows ", as.character(dim(data_output)[1])))
  return(data_output)
}



do_bind_and_filter <- function(data_control,
                               data_hockey_players,
                               names_keep = c("Footwear", "Stance.Position", "Sensory.Manipulation" ),
                               measurments_keep = c("Sway.path...total..mm.", 
                                                    "Sway.V...total..mm.s.", 
                                                    "Sway.area.per.second...total..mm.2.s.")){
### Binds read data, check there are no missing values
  data_control <- data_control[, c(names_keep, measurments_keep)]
  data_hockey_players <- data_hockey_players[, c(names_keep, measurments_keep)]
  data_control$control <- "nesportovci"
  data_hockey_players$control <- "hokejista"
  data_out <- rbind(data_control,data_hockey_players)
  data_out$control <- as.factor(data_out$control)
  print("Data merged, control variable created")
  return(data_out)
}
 
###### Read, clean and merge #####
data_control_xlsx <- read_data_(path_data_control_xlsx)
names(data_control_xlsx) <- str_replace_all(names(data_control_xlsx), "[[:punct:]]", ".")
names(data_control_xlsx)<- str_replace_all(names(data_control_xlsx), " ", ".")
names(data_control_xlsx)[grepl("Sway.area.per.second...", names(data_control_xlsx))] <-"Sway.area.per.second...total..mm.2.s."
names(data_control_xlsx)

 
data_hockey_players<- read_data_(path_data_hockey_players)
names(data_hockey_players)
print(paste0("Number of observations before first cleaning ", dim(data_hockey_players)[1]))
data_hockey_players <- data_hockey_players %>% filter(Sway.V...total..mm.s.<8000)
print(paste0("Number of observations after first cleaning ", dim(data_hockey_players)[1]))
data <- do_bind_and_filter(data_control_xlsx, data_hockey_players)

data[is.na(as.numeric(data$Sway.path...total..mm.)),]
#####
do_save_plot <- function(variable,
                         title,
                         ylabel,
                         file_to){
  
  plot_1 <- ggplot(data = data, 
                   aes_string(x = "control",
                       y = variable,
                       colour = "control")) +
    facet_grid(rows = vars(Sensory.Manipulation), cols = vars(Stance.Position, Footwear ), scales="free")+
    geom_boxplot() +
    ggtitle(title)+
    ylab(ylabel)+
    xlab("")+
    guides(colour=guide_legend(title=""))
  
  ggsave(filename = paste0(dir_output, "/", file_to), plot = plot_1, width = 20, height =10)
}

##### sway_path_total.png #####
do_save_plot(variable = "Sway.path...total..mm.",
             title = "Sway path total",
             ylabel = "Sway path total [mm]",
             file_to="sway_path_total.png")

do_save_plot(variable = "Sway.V...total..mm.s.",
             title = "Sway velocity total [mm/s]",
             ylabel = "Sway velocity total [mm/s]",
             file_to= "sway_v_total.png")

do_save_plot(variable = "Sway.area.per.second...total..mm.2.s.",
             title = expression(paste("Sway area per second [", mm^{2}/s, "]")),
             ylabel = expression(paste("Sway area per second [", mm^{2}/s, "]")),
             file_to= "sway_area_per_second.png")



