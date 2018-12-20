buoy_years <- avail_buoy_years()
# number of buoys online over time
plot(table(buoy_years$year))
# longest running buoys
sort(table(buoy_years$buoy), decreasing=T)[1:25]
