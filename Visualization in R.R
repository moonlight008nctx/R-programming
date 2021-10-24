# Visualization tips in Rstudio

## visualize correlation matrix
library(corrplot)
### first run the correlation matrix for data table.
cor(tb_house1981_2)
# the following code can help visualize correlation matrix.
corrplot(cor(tb_house1981_2))