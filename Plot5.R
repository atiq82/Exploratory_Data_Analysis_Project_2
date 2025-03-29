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

# 4.1. Filtering NEI dataset of ON-ROAD from all sources.
NEI_q5 <- base::subset(x = NEI, NEI$fips=="24510" & NEI$type=="ON-ROAD")

# 4.2. Summarizing the dataset to aggregate Emissions into Total divided by year.
plot_5_data <- NEI_q5 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(Total = base::sum(Emissions))

########################### 5. Plot 5 ##########################################

# 5.1. Exporting a PNG file. 
grDevices::png(filename = "plot5.png", height = 480, width = 800)

# 5.1.1. Creating a ggplot2 graph.
ggplot2::ggplot(data = plot_5_data,
                ggplot2::aes(x = year,
                             y = Total)) +
  
  # Defining stacked bars.
  ggplot2::geom_bar(position = "stack",
                    stat = "identity") + 
  
  # Adding labels with value over the bars.
  ggplot2::geom_text(data = plot_5_data,
                     ggplot2::aes(x = year,
                                  label = base::format(x = Total,
                                                       nsmall = 1,digits = 1), # Rouding the values.
                                  y = Total,
                                  fill = NULL),
                     nudge_y = 10) + 
  
  # Setting the years.
  ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
  
  # Adding title
  ggplot2::labs(title = base::expression('Vehicle Emissions PM'[2.5] ~ ' in Baltimore')) + 
  
  # Adding x-axis label.
  ggplot2::xlab("Year") + 
  
  # Adding y-axis label.
  ggplot2::ylab(base::expression('Total PM'[2.5] ~ 'Emissions (tons)')) +
  
  # Editing the legend position/align and title position.
  ggplot2::theme(legend.position = "bottom",
                 legend.title.align = 0.5,
                 plot.title = ggplot2::element_text(hjust = 0.5)) + 
  
  # Removing the legend title.
  ggplot2::guides(fill = ggplot2::guide_legend(title = "")) -> p5

# Printing the ggplot2 graph.
base::print(p5)

# 5.2. Closing the device.
grDevices::dev.off()