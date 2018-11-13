library(dplyr)

download_data <- function(url) {
        filename <- url %>%
                URLdecode %>%
                strsplit("/") %>%
                unlist %>%
                tail(1)
        if (!file.exists(filename))
                download.file(url, dest = filename)
        filename
}

unzip_file <- function(filename) {
        if (filename %in% dir())
                unzip(filename)
}

load_data <- function(filename) {
        if (filename %in% dir())
                readRDS(filename)
        else
                NULL
}

fetch_data <- function() {
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        url %>%
                download_data %>%
                unzip_file
}

fetch_NEI <- function(filename = "summarySCC_PM25.rds") {
        if (filename %in% dir())
                readRDS(filename)
        else
                stop(paste(filename, "is not present", sep = " "))
}

fetch_SCC <- function(filename = "Source_Classification_Code.rds") {
        if (filename %in% dir())
                readRDS(filename)
        else
                stop(paste(filename, "is not present", sep = " "))
}

