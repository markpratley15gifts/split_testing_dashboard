A shiny app for analysing split test.

requires R and Shiny server to be installed on the machine
listens on port 3838

These two files need to be placed in the directory (ensure that it is empty first):
/srv/shiny-server

requires the following R libraries:
shiny
ggplot2
reshape2
dplyr
stringr

on Ubuntu Server 14.04  done with (you'll need to find the correct source for other distributions):

sudo su -c "echo 'deb http://archive.linux.duke.edu/cran/bin/linux/ubuntu trusty/' >> /etc/apt/sources.list"
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get dist-upgrade -y
sudo apt-get install r-base -y
sudo su - \
-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
sudo su - \
-c "R -e \"install.packages('reshape2', repos='https://cran.rstudio.com/')\""
sudo su - \
-c "R -e \"install.packages('dplyr', repos='https://cran.rstudio.com/')\""
sudo su - \
-c "R -e \"install.packages('stringr', repos='https://cran.rstudio.com/')\""
sudo su - \
-c "R -e \"install.packages('ggplot2', repos='https://cran.rstudio.com/')\""
sudo apt-get install gdebi-core -y
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.1.759-amd64.deb
sudo gdebi shiny-server-1.4.1.759-amd64.deb -y
