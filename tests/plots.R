.Libs()
library(orgR)
library(grid)
library(gridExtra)

qs.file = "~/git/org/Archive/qs.org"
clock.ds <- GetClockTable(qs.file)
clock.ds$duration = as.numeric(clock.ds$end - clock.ds$start , unit="secs")

## p = view.Clock(clock.ds)
## grid.draw(p)

## p = viewByFamily.Clock(clock.ds)
## grid.draw(p)


p = viewPieChart.Clock(clock.ds)
grid.draw(p)
