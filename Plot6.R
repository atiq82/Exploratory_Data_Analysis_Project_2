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


# 4.1. Filtering NEI dataset of ON-ROAD type to select only Baltimore City and Los Angeles County observations.
NEI_q6 <- base::subset(x = NEI,
                       NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")

# 4.2. Creating a auxiliary dataset to store the infos: fips and city.
city_data <- base::data.frame(fips = c("24510","06037"),                    # First column
                              city = c("Baltimore, MD","Los Angeles, CA"))  # Second column

# 4.3. Merging NEI_q6 and city_data to create a new dataframe with a column of city. The city info
# will be used in the graph.
NEI_q6_v2 <- base::merge(NEI_q6, city_data)

# 4.4. Summarizing the dataset to aggregate Emissions into Total divided into year and city.
plot_6_data <- NEI_q6_v2 %>%
  dplyr::group_by(year, city) %>%
  dplyr::summarise(Total = base::sum(Emissions))

########################### 5. Plot 6 ##########################################

# 5.1. Exporting a PNG file. 
grDevices::png(filename = "plot6.png", height = 480, width = 800)

# 5.1.2. Creating the first graph about Baltimore City Vehicle Emissions.
p61 <- ggplot2::ggplot(data = plot_6_data %>% dplyr::filter(city == "Baltimore, MD"), # Filtering only data from Baltimore.
                       ggplot2::aes(x = year, y = Total)) + 
  
  # Defining a line plot, color and line width.
  ggplot2::geom_line(lwd = 1, color = "deepskyblue") + 
  
  # Adding title.
  ggplot2::labs(title = base::expression('Vehicle Emissions PM'[2.5] ~ ' in Baltimore (MD)')) +
  
  # Defining the tick marks on the x-axis.
  ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) + 
  
  # Adding x-axis label.
  ggplot2::xlab("Year") + 
  
  # Adding y-axis label.
  ggplot2::ylab(base::expression('Total PM'[2.5] ~ 'Emissions (tons)'))

# 5.1.2. Creating the first graph about Los Angeles County Vehicle Emissions.
p62 <- ggplot2::ggplot(data = plot_6_data %>% dplyr::filter(city == "Los Angeles, CA"), # Filtering only data from Los Angeles County.
                       ggplot2::aes(x = year, y = Total)) +
  
  # Defining a line plot, color and line width.
  ggplot2::geom_line(lwd = 1, color = "coral") +
  
  # Adding title.
  ggplot2::labs(title = base::expression('Vehicle Emissions PM'[2.5] ~ ' in Los Angeles County (CA)')) +
  
  # Defining the tick marks on the x-axis.
  ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) + 
  
  # Adding x-axis label.
  ggplot2::xlab("Year") + 
  
  # Adding y-axis label.
  ggplot2::ylab(base::expression('Total PM'[2.5] ~ 'Emissions (tons)'))

# 5.1.3. Using the cowplot to bind graphs p1 and p2 into one.
p612 <- cowplot::plot_grid(p61, p62, labels = "")

# 5.1.4. Printing the cowplot.
base::print(p612)

# 5.2. Closing the device.
grDevices::dev.off()