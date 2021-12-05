# Class 05: Data Visualization

# Today we are going to use ggplot2 package

# First we need to load the package!
# install.packages("ggplot2")
library(ggplot2)

# we will use this inbuilt "cars" dataset first
head(cars)

# All ggplots have at least 3 layers,
# data + aes + geoms
ggplot(data=cars) + 
  aes(x=speed, y=dist) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Stopping Distance of Old Cars",
       x="Speed (MPH)",
       y="Stopping Distance (ft)")

# Side-note: ggplot is not the only graphics system 
# a very popular one is good old "base" R graphics

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# Q. How many genes are in the dataset?
nrow(genes)

# Q. How many columns are in the dataset?
colnames(genes)
ncol(genes)

# Q. How many genes are "up"?
table(genes$State)

# Q. What % are up?
round(table(genes$State)/nrow(genes)*100, 2)

# Lets make a figure
p <- ggplot(genes) + 
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()
p

# I like it but not the default colors, lets change them
p + scale_colour_manual(values=c("blue","gray","red"))

# I will give a title to the plot, 
# and change the names of x-axis and y-axis
p <- p + 
  labs(title="Gene Expression Changes Upon Drug Treatment",
       x="Control (no drug)",
       y="Drug Treatment")

# Lets explore the gapminder dataset
# install.packages("gapminder")
library(gapminder)
head(gapminder)

# Lets make a new plot of year vs lifeExp
ggplot(gapminder) +
  aes(x=year, y=lifeExp, col=continent) +
  geom_jitter(width=0.3, alpha=0.4) +
  geom_violin(aes(group=year), alpha=0.2, 
              draw_quantiles = 0.5)

# Install the plotly
# install.packages("plotly")
# library(plotly)
# ggplotly()