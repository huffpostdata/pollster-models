# HuffPost Pollster Models

This repository contains code used by HuffPost Pollster models and forecasts.

## Model Descriptions

### poll-average

The initial poll averaging model, written by Simon Jackman for the 2013 Virginia Governor's race, and generalized to any race by Jay Boice.

	> Rscript poll-average.R '2013-virginia-governor-cuccinelli-vs-mcauliffe'

### poll-average-senate-2014

Natalie Jackson's forecast for the 2014 Senate elections, based on `poll-average`, with improvements including…

	> Rscript runall.R
	
### poll-average-gov-2014

Natalie Jackson's forecast for the 2014 Gubenatorial elections, based on `poll-average`, with improvements including...

	> Rscript runall.R

### poll-average-new

A generalized version of the improvements made for the 2014 Senate/Gubenatorial elections, currently running on the 2014 National House Race chart.

	> Rscript poll-average-new.R '2014-national-house-race'

## Installation

### OSX

Using [Homebrew](http://brew.sh/):

	brew tap homebrew/science
	brew install r
	brew install jags
	
	R
	> install.packages("rjags")
	> install.packages("truncnorm")
	> install.packages("rjson")

### Windows

### CentOS

	sudo yum -y install libXt-devel.x86_64 libX11-devel.x86_64 libpng-devel.x86_64 xorg-x11-xtrans-devel.x86_64 libXtst-devel.x86_64 gcc44-c++.x86_64 gcc-gfortran.x86_64 gcc44-gfortran.x86_64 readline-devel.x86_64 gcc-c++.x86_64 g++ compat-gcc-34-g77.x86_64
	wget http://lib.stat.cmu.edu/R/CRAN/src/base/R-3/R-3.0.2.tar.gz
	tar -zxvf R-3.0.2.tar.gz && cd R-3.0.2 && ./configure && make && sudo make install
	sudo cp /usr/local/bin/R /usr/bin/
	sudo cp /usr/local/bin/Rscript /usr/bin/
	sudo yum -y install lapack.x86_64 lapack-devel.x86_64 blas.x86_64 blas-devel.x86_64
	wget http://downloads.sourceforge.net/project/mcmc-jags/JAGS/3.x/Source/JAGS-3.4.0.tar.gz
	tar -zxvf JAGS-3.4.0.tar.gz && cd JAGS-3.4.0 && ./configure --libdir=/usr/local/lib64 && make && sudo make install
	
	R
	> install.packages("rjags")
	> install.packages("truncnorm")
	> install.packages("rjson")

## Authors

* Natalie Jackson <natalie.jackson@huffingtonpost.com>
* Simon Jackman <jackman@stanford.edu>
* Jay Boice <jay.boice@huffingtonpost.com>
