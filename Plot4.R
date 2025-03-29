# Libraries Requirements

library(magrittr)
library(tidyverse)


# 1 Create a data directory
if(!base::file.exists("data")) {
  base::dir.create("data")
}


# 2.1 Download files and store it in data directory.
if(!base::file.exists("./data/FNEI_data.zip")){
  utils::download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                       destfile = "./data/FNEI_data.zip")
}

# 2.2 Unzipping the FNEI_data.zip file.
if(!base::file.exists("./data/unzipped/Source_Classification_Code.rds") | !base::file.exists("./data/unzipped/summarySCC_PM25.rds")){
  utils::unzip(zipfile = "./data/FNEI_data.zip",
               exdir = "./data/unzipped/",
               list = FALSE,
               overwrite = TRUE)
}


# 3 Loading the RDS files.
NEI <- base::readRDS("./data/unzipped/summarySCC_PM25.rds")
SCC <- base::readRDS("./data/unzipped/Source_Classification_Code.rds")


# 4.1. Filtering the SCC cods from Coal.
SCC_list <- SCC %>% 
  dplyr::filter(base::grepl(x = EI.Sector,
                            pattern = "Coal|coal")) %>%
  dplyr::select(SCC, EI.Sector)

# 4.2. Filtering NEI dataset of Coal from a specific SCC list.
NEI_q4 <- base::subset(x = NEI, SCC %in% SCC_list$SCC)

# 4.3. Merging SCC_list and NEI_q4 to insert a column of EI.Sector on NEI dataset.
NEI_q4_v2 <- base::merge(NEI_q4, SCC_list)

# 4.4. Calculating the total summation and removing some patterns from EI.Sector column.
NEI_q4_v3 <- NEI_q4_v2 %>%
  dplyr::group_by(year, EI.Sector) %>%                                    # Grouping to summarize it by year and EI.Sector.
  dplyr::summarise(Total = base::sum(Emissions)) %>%                      # Calculating the Total column.
  dplyr::mutate(EI.Sector = base::gsub(pattern = "Fuel Comb - | - Coal",  # Removing some patterns from EI.Sector
                                       replacement =  "",                 # Cleaning the info from EI.Sector column.
                                       x = EI.Sector))

# 4.5. Auxiliary dataset to calculate the total of each bar.
NEI_q4_v3_total <- NEI_q4_v3 %>%
  dplyr::summarise(Total = base::sum(Total)/1000)

#################################### 5. Plot 4  #######################################

# 5.1. Exporting a PNG file. 
grDevices::png(filename = "plot4.png", height = 480, width = 800)

# 5.1.1. Creating a ggplot2 graph.
ggplot2::ggplot(data = NEI_q4_v3,
                ggplot2::aes(x = year,
                             y = Total/1000,       # Re-scaling to thousands.
                             fill = EI.Sector)) +
  
  # Defining stacked bars.
  ggplot2::geom_bar(position = "stack", stat = "identity") + 
  
  # Adding labels with value over the bars.
  ggplot2::geom_text(data = NEI_q4_v3_total,
                     ggplot2::aes(x = year,
                                  label = base::format(x = Total,
                                                       nsmall = 1, digits = 1), # Rounding the number.
                                  y = Total,
                                  fill = NULL),
                     nudge_y = 10) + # Distance to the point.
  
  # Setting the years.
  ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
  
  # Adding title
  ggplot2::labs(title = base::expression('Coal Combustion PM'[2.5] ~ ' in the United States')) + 
  
  # Adding x-axis label.
  ggplot2::xlab("Year") +
  
  # Adding y-axis label.
  ggplot2::ylab(base::expression('PM'[2.5] ~ 'Emissions (10' ^3 ~ 'tons)')) +
  
  # Editing the legend position and tile position.
  ggplot2::theme(legend.position = "bottom",
                 legend.title.align = 0.5,
                 plot.title = ggplot2::element_text(hjust = 0.5)) +
  
  # Removing the legend title.
  ggplot2::guides(fill = ggplot2::guide_legend(title = ""))

# 5.2. Closing the device.
grDevices::dev.off()