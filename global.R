if(!require(rpivotTable)) {install.packages('rpivotTable', dependencies = FALSE); require(rpivotTable)}
if(!require(shinydashboard)) {install.packages('shinydashboard', dependencies = FALSE); require(shinydashboard)}
if(!require(tdplyr)) {install.packages('tdplyr', dependencies = FALSE); require(tdplyr)}
if(!require(dplyr)) {install.packages('dplyr', dependencies = FALSE); require(dplyr)}
if(!require(bupaR)) {install.packages('bupaR', dependencies = FALSE); require(bupaR)}
if(!require(dbplyr)) {install.packages('dbplyr', dependencies = FALSE); require(dbplyr)}
if(!require(tidyr)) {install.packages('tidyr', dependencies = FALSE); require(tidyr)}
if(!require(hablar)) {install.packages('hablar', dependencies = FALSE); require(hablar)}
if(!require(shinybusy)) {install.packages('shinybusy', dependencies = FALSE); require(shinybusy)}
if(!require(shinyWidgets)) {install.packages('shinyWidgets', dependencies = FALSE); require(shinyWidgets)}
if(!require(tippy)) {install.packages('tippy', dependencies = FALSE); require(tippy)}
if(!require(Dict)) {install.packages('Dict', dependencies = FALSE); require(Dict)}
# if(!require(RColorBrewer)) {install.packages('RColorBrewer', dependencies = FALSE); require(RColorBrewer)}

if(!require(petrinetR)) {install.packages('petrinetR', dependencies = FALSE); require(petrinetR)}
if(!require(heuristicsmineR)) {install.packages('heuristicsmineR', dependencies = FALSE); require(heuristicsmineR)}

if(!require(DiagrammeR)) {install.packages('DiagrammeR', dependencies = FALSE); require(DiagrammeR)}
if(!require(plotly)) {install.packages('plotly', dependencies = FALSE); require(plotly)}
if(!require(ggplot2)) {install.packages('ggplot2', dependencies = FALSE); require(ggplot2)}
if(!require(eventInterval)) {install.packages('eventInterval', dependencies = FALSE); require(eventInterval)}
if(!require(TraMineR)) {install.packages('TraMineR', dependencies = FALSE); require(TraMineR)}
if(!require(splitstackshape)) {install.packages('splitstackshape', dependencies = FALSE); require(splitstackshape)}
if(!require(DT)) {install.packages('DT', dependencies = FALSE); require(DT)}

if(!require(scales)) {install.packages('scales', dependencies = FALSE); require(scales)}
if(!require(purrr)) {install.packages('purrr', dependencies = FALSE); require(purrr)}
if(!require(cluster)) {install.packages('cluster', dependencies = FALSE); require(cluster)}
if(!require(sortable)) {install.packages('sortable', dependencies = FALSE); require(sortable)}
if(!require(ggthemes)) {install.packages('ggthemes', dependencies = FALSE); require(ggthemes)}
if(!require(RColorBrewer)) {install.packages('RColorBrewer', dependencies = FALSE); require(RColorBrewer)}
#if(!require(tidyverse)) {install.packages('tidyverse', dependencies = FALSE); require(tidyverse)}
if(!require(shinyWidgets)) {install.packages('shinyWidgets', dependencies = FALSE); require(shinyWidgets)}
if(!require(esquisse)) {install.packages('esquisse', dependencies = FALSE); require(esquisse)}

if(!require(d3treeR)) {install.packages("remotes"); remotes::install_github("d3treeR/d3treeR"); require(d3treeR)}

if(!require(treemap)) {install.packages('treemap', dependencies = FALSE); require(treemap)}
if(!require(svgPanZoom)) {install.packages('svgPanZoom', dependencies = FALSE); require(svgPanZoom)}
if(!require(DiagrammeRsvg)) {install.packages('DiagrammeRsvg', dependencies = FALSE); require(DiagrammeRsvg)}
if(!require(flexdashboard)) {install.packages('flexdashboard', dependencies = FALSE); require(flexdashboard)}
if(!require(billboarder)) {install.packages('billboarder', dependencies = FALSE); require(billboarder)}
if(!require(DDoutlier)) {install.packages('DDoutlier', dependencies = FALSE); require(DDoutlier)}
if(!require(processcheckR)) {install.packages('processcheckR', dependencies = FALSE); require(processcheckR)}
if(!require(shinyjs)) {install.packages('shinyjs', dependencies = FALSE); require(shinyjs)}
if(!require(formattable)) {install.packages('formattable', dependencies = FALSE); require(formattable)}
if(!require(rhandsontable)) {install.packages('rhandsontable', dependencies = FALSE); require(rhandsontable)}
if(!require(googleVis)) {install.packages('googleVis', dependencies = FALSE); require(googleVis)}
if(!require(sankeywheel)) {install.packages('sankeywheel', dependencies = FALSE); require(sankeywheel)}
if(!require(getPass)) {install.packages('getPass', dependencies = FALSE); require(getPass)}
if(!require(lubridate)) {install.packages('lubridate', dependencies = FALSE); require(lubridate)}
if(!require(gapminder)) {install.packages('gapminder', dependencies = FALSE); require(gapminder)}
if(!require(hrbrthemes)) {install.packages('hrbrthemes', dependencies = FALSE); require(hrbrthemes)}
if(!require(viridis)) {install.packages('viridis', dependencies = FALSE); require(viridis)}
if(!require(ChainLadder)) {install.packages('ChainLadder', dependencies = FALSE); require(ChainLadder)}

tab_modules <- list.files("tabs", pattern = "tab_module.*\\.R", full.names = T,
                          recursive = T)

for(tab_module in tab_modules) source(tab_module)
