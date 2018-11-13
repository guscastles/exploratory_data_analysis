library(dplyr)

make_plot2 <- function() {
	fetch_NEI() %>%
		total_emissions_by_year_in_baltimore
}

fetch_NEI <- function(filename = "summarySCC_PM25.rds") {
        if (filename %in% dir())
                readRDS(filename)
        else
                stop(paste(filename, "is not present", sep = " "))
}

total_emissions_by_year_in_baltimore <- function(NEI) {
	baltimore <- NEI["fips"] == "24510"
	emissions_by_year <- aggregate(Emissions ~ year, data = NEI[baltimore,], FUN = sum)
	png("plot2.png")
	barpos <- with(emissions_by_year,
	     barplot(Emissions,
		     names.arg = year,
		     ylab="Amount of PM2.5 emitted, in thousands of tons",
		     yaxt="n",
                     ylim = c(0, 4e3),
		     xlab="Year"))
	title("Total Emissions of PM25 by Year in Baltimore City, Maryland")
	update_y_axis()
	add_labels(barpos, emissions_by_year)
        trend_line(emissions_by_year)
	dev.off()
}

update_y_axis <- function() {
	new_ticks <- 0:6 %>% sapply(function(val) val * 1e3)
	new_labels <- new_ticks %>% sapply(function(val) val / 1e3)
	axis(side = 2, at = new_ticks, labels = new_labels)
}

add_labels <- function(barpos, emissions_by_year) {
	emissions <- emissions_by_year %>%
		       select(year, Emissions) %>%
		       mutate(label = sprintf("%.2f", Emissions / 1e3))
	text(barpos, emissions$Emissions, emissions$label, pos = 1)
}

trend_line <- function(emissions_by_year) {
        fit <- lm(emissions_by_year$Emissions ~ I(emissions_by_year$year - 1999))
        abline(fit, col = "red") 
}
