library(dplyr)

make_plot1 <- function() {
	fetch_NEI() %>%
		total_emissions_by_year
}

fetch_NEI <- function(filename = "summarySCC_PM25.rds") {
        if (filename %in% dir())
                readRDS(filename)
        else
                stop(paste(filename, "is not present", sep = " "))
}

total_emissions_by_year <- function(NEI) {
	emissions_by_year <- aggregate(Emissions ~ year, data = NEI, FUN = sum)
	png("plot1.png")
	barpos <- with(emissions_by_year,
	     barplot(Emissions,
		     names.arg = year,
		     ylab = "Amount of PM2.5 emitted, in millions of tons",
		     yaxt = "n",
                     ylim = c(0, 8e6),
		     xlab = "Year"))
	title("Total Emissions of PM25 by Year in the USA")
	update_y_axis()
	add_labels(barpos, emissions_by_year)
        trend_line(emissions_by_year)
	dev.off()
}

update_y_axis <- function() {
	new_ticks <- 0:8 %>% sapply(function(val) val * 1e6)
	new_labels <- new_ticks %>% sapply(function(val) val / 1e6)
	axis(side = 2, at = new_ticks, labels = new_labels)
}

add_labels <- function(barpos, emissions_by_year) {
	emissions <- emissions_by_year %>%
		       select(year, Emissions) %>%
		       mutate(label = sprintf("%.2f", Emissions / 1e6))
	text(barpos, emissions$Emissions, emissions$label, pos = 1)
}

trend_line <- function(emissions_by_year) {
        fit <- lm(emissions_by_year$Emissions ~ I(emissions_by_year$year - 1999))
        abline(fit, col = "red") 
}
