########################################################################
# Introduce the concept of factor analysis using psych package in R
#
# Measuring consumer Involvement: Factor Analysis in R
# The Devil is in the Data
# https://lucidmanager.org/measauring-consumer-involvement
# Peter Prevos
#
# Update to essentials style for mlhub.ai Graham Williams 20190120
#
# Text comes from Peter's blog post and used in accordance with the
# Creative Commons Attribution-ShareAlike 4.0 International License.

cat("==================================================
Measure Consumer Involvement using Factor Analysis
==================================================

Below we introduce the concept of factor analysis in the context of
measuring consumer involvement. Consumer involvement aims to measure the
attitude people have towards a product or service.

This example was created by Peter Prevos and packaged for mlhub.ai by
Graham Williams. See the README for details.
")

# Load required packages.

suppressMessages(
{
library(magrittr)     # Data pipelines: %>% %<>% %T>% equals().
library(tidyverse)    # Tools for tidy data.
library(psych)        # Factor analysis.
})

cat("\nPress Enter to continue on to read the data: ")
invisible(readChar("stdin", 1))

# Load the dataset.

cat("
===================
Loading the Dataset
===================

The dataset consists of consumer's answers to a series of questions. Analysing
such data invovles an exploration of hidden (latent) states of the mind that
influences of the consumer. Factor Analysis can discover latent variables
within customer survey data through patterns in the responses.

The data is from 832 tap water consumers from Australia and the United States
who were surveyed to seek their views on tap water. For example, *to me, tap
water is:* Important ... Unimportant, Boring ... Intersting, etc. Ten such
questions were asked with a range between 1 and 7 (from Important to
Unimportant).

Below we report on the size of the dataset (number of rows and columns) and
also list a random sample of 12 observations.

")

suppressMessages(read_csv("customers_quan.csv")) %>%
  select(starts_with("p"))  %T>%
  {dim(.) %>% print(); cat("\n")} %T>% 
  {sample_n(., 12) %>% print()} ->
consumers

cat("\nPress Enter to continue on to cleanse the data: ")
invisible(readChar("stdin", 1))

# Data cleansing.

cat("
==============
Data Cleansing
==============

Customers who provided the same answer to all items or did not respond to all
questions are removed as these are most likely invalid responses. This leaves
757 rows of data.

")

sdevs <- apply(consumers, 1, sd, na.rm = TRUE)
incomplete <- apply(consumers, 1, function(i) any(is.na(i)))
consumers <- consumers[sdevs != 0 & !incomplete, ]
dim(consumers)

## Exploratory Analysis

cat("\nPress Enter to continue on to exploratory analysis: ")
invisible(readChar("stdin", 1))

cat("
========
Box Plot
========

A boxplot is a convenient way to view the responses to multiple survey
items in one visualisation. This plot immediately shows an interesting
pattern in the answers. It seems that responses to the first five
items were generally higher than those for the last five items. This
result seems to indicate a demarcation between cognitive and affective
involvement.

")

fname <- "involvement_explore.png"
consumers %>%
    rownames_to_column(var = "Subject") %>%
    gather(Item, Response, -Subject) %>%    
    ggplot(aes(Item, Response)) + geom_boxplot(fill = "#f7941d") +
    ggtitle("Personal Involvement Index",
            subtitle = paste("Tap Water Consumers (n =",
                             nrow(consumers), ")"))
suppressMessages(ggsave(fname, dpi = 300))

cat("Press Enter to display the boxplot: ")
invisible(readChar("stdin", 1))

if (Sys.getenv("DISPLAY") != "")
{
  system(paste("eom", fname), ignore.stderr=TRUE, wait=FALSE)
}

cat("
Close the graphic window using Ctrl-w.
Press Enter to continue the analysis: ")
invisible(readChar("stdin", 1))

cat("
===========
Correlation
===========

The next step in the exploratory analysis is to investigate how these
factors correlate with each other. The correlation plot here shows
that all items strongly correlate with each other. In correspondence
with the boxplots above, the first five and the last five items
correlate more strongly with each other. This plot suggests that the
two dimensions of the involvement index correlate with each other.

")

fname <- "involvement_correlation.png"
png(fname, width=1024, height=1024)
corPlot(consumers)
invisible(dev.off())

cat("Press Enter to display the correlation: ")
invisible(readChar("stdin", 1))

if (Sys.getenv("DISPLAY") != "")
{
  system(paste("eom", fname), ignore.stderr=TRUE, wait=FALSE)
}

cat("
Close the graphic window using Ctrl-w.
Press Enter to continue the analysis: ")
invisible(readChar("stdin", 1))

cat("
===============
Factor Analysis
===============

We use Factor Analysis to uncover latent variables in our data. An important
decision in factor analysis is to decide how to rotate the factors. There are
two types of rotations: orthogonal (reduce correlation between dimenstions) and
oblique (allow for dimensions to relate to each other). Given the strong
correlations in the correlation plot and that both dimensions measure
involvement, this analysis uses oblique rotation. The visualisation below
shows how each of the items and the two dimensions relate to each other.

")


suppressMessages(piiFac <- fa(consumers, nfactors = 2, rotate = "oblimin"))
fname <- "involvement_factors.png"
consumers <- png(fname, width = 1024, height = 768)
fa.diagram(piiFac)
invisible(dev.off())

cat("Press Enter to display the relationships: ")
invisible(readChar("stdin", 1))

if (Sys.getenv("DISPLAY") != "")
{
  system(paste("eom", fname), ignore.stderr=TRUE, wait=FALSE)
}

cat("
Close the graphic window using Ctrl-w.
Press Enter to finish the demonstration: ")
invisible(readChar("stdin", 1))
