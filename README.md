# HuffPost Pollster Models

This repository contains code used by HuffPost Pollster models and forecasts.

# Setup

## Mac OS X

Using [Homebrew](http://brew.sh/):

```sh
brew tap homebrew/science
brew install r
brew install jags
brew install curl
R --vanilla -e 'install.packages(c("rjags", "truncnorm", "coda", "httr"), repos=c("https://cloud.r-project.org/"))'
```

## Windows

Really? No.

## Linux: CentOS

```bash
wget http://download.fedoraproject.org/pub/epel/6/i386/epel-release-6-8.noarch.rpm
(cd /etc/yum.repos.d && sudo wget 'http://download.opensuse.org/repositories/home:/cornell_vrdc/CentOS_CentOS-6/home:cornell_vrdc.repo')
sudo rpm -ivh epel-release-6-8.noarch.rpm
sudo yum install R R-devel jags4-devel curl-devel
R --vanilla -e 'install.packages(c("rjags", "truncnorm", "coda", "httr"), repos=c("https://cloud.r-project.org/"))'
```

# Usage

```sh
cd poll-average
Rscript --vanilla ./poll-average.R http://production-elections-internal.use1.huffpo.net/pollster 2016-new-hampshire-ayotte-vs-hassan fast
```

This will:

1. GET http://production-elections-internal.use1.huffpo.net/pollster/2016-new-hampshire-ayotte-vs-hassan.csv
2. Calculate predictions for all days
3. POST predictions to `http://production-elections-internal.use1.huffpo.net/pollster/api/charts/2016-new-hampshire-ayotte-vs-hassan/model-output.csv`
4. POST house effects to `http://production-elections-internal.use1.huffpo.net/pollster/api/charts/2016-new-hampshire-ayotte-vs-hassan/model-output.csv`

This doesn't use auth. Obviously, it won't work over the Internet: it'll only
work on our VPN. (That's why we're not panicking about _not_ using HTTPS.)

The `fast` argument means we shouldn't do a full MVMC run: we'll use a far lower
M to save time. The alternative and default, `slow`, will be slower and more
stable.

# Code structure

Clearly, the important code is in `poll-average/`. The entrypoint is
`poll-average/poll-average.R`.

Other code is in `cruft/`:

* `cruft/poll-average-senate-2014/runall.R`: Natalie Jackson's forecast for the 2014 Senate elections.
* `cruft/poll-avarage-gov-2014/runall.R`: Natalie Jackson's forecast for the 2014 Gubenatorial elections.
* `cruft/poll-average-new/poll-average-new.R`: A generalized version of the improvements made for the 2014 Senate/Gubenatorial elections, currently running on the 2014 National House Race chart.

## Authors

* Adam Hooper <adam.hooper@huffingtonpost.com>
* Natalie Jackson <natalie.jackson@huffingtonpost.com>
* Simon Jackman <jackman@stanford.edu>
* Jay Boice <jay.boice@huffingtonpost.com>
