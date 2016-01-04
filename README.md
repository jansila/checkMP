# checkMP
This repository provides three files:

1) checkMP.R is a function that computes a test statistic to check if a series possesses the Markov property. Its construction follows lecture notes from Module MA7404 taught by Dr Bogdan Grechuk at the University of Leicester - Fall 2015. 

http://www2.le.ac.uk/departments/mathematics/extranet/staff-material/staff-profiles/bg83

2) markov test.R defines other functions and introduces core code for Shiny app I created and is available here: https://github.com/jansila/Models_mortgage.git

The transition matrix is computed using package markovchain https://cran.rstudio.com/web/packages/markovchain/index.html

3) Import.xlsx is the data set it was tested on, it contains the Bank of England base interest rate since 1911 till the end of 2015.