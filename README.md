---
editor_options: 
  markdown: 
    wrap: 80
---

# BellviewR

Indirect reference intervals using R

This R shiny app has been designed as a tool to assist in the process for
developing reference intervals using and indirect approach based on data mining.
At the present time the application supports the use of Bhattaharya analysis. As
well as the refineR algorithm as implemented in the as named package and
described in Ammer T., Schuetzenmeister A., Prokosch H.-U., Rauh M., Rank C.M.,
Zierk J. "refineR: A Novel Algorithm for Reference Interval Estimation from
Real-World Data". Scientific Reports (2021) <doi:10.1038/s41598-021-95301-2>.

Bhattacharya analysis is based on the implementation described in Daniel T
Holmes, MD, Kevin A Buhr, PhD, Widespread Incorrect Implementation of the
Hoffmann Method, the Correct Approach, and Modern Alternatives, American Journal
of Clinical Pathology, Volume 151, Issue 3, March 2019, Pages 328--336,
<https://doi.org/10.1093/ajcp/aqy149>

## Install R and RStudio Desktop

While this application is available as
[bellviewr](https://grasstree.shinyapps.io/bellviewr/) on the shinyapps site,
for performance on larger data sets you may wish to run as a local installation.

Download and install a copy of R, which is available from the comprehensive R
archive network [CRAN](https://cran.r-project.org/mirrors.html).

Run R and install the following packages.

    install.packages(c("shiny","dplyr","ggplot2","refineR"))

If you are behind a firewall you may need to add your proxy details. Update the
.Renviron file with.

    file.edit("~/.Renviron")

Add the following entries

    http_proxy=http://proxy.domain.or.ip:port
    https_proxy=http://proxy.domain.or.ip:port

If authentication is required use

    http_proxy=http://proxy.domain.or.ip:port
    https_proxy=http://proxy.domain.or.ip:port
    http_proxy_user=username:password
    https_proxy_user=username:password

For reasons that elude me, this works from the R commandline or RGui app but not
from RStudio

Download RStudio Desktop from
[Posit](https://posit.co/download/rstudio-desktop/) and install.

It is possible to install both R and RStudio to a user directory without
administrator privileges.

### Installing and running the application

You have the option of downloading the project directory as a zip file or
cloning the Git repository. From [https://github.com/dche658/bellviewr](Bellviewr page on Git Hub) click on the Code button to display
the repository URL or a link to download all the files.

Select the so named link if downloading as a zip archive and save to a suitable
directory on your computer. Extract the zip archive. Run RStudio Desktop and
from the File menu select Open Project. Navigate to the folder where the files
are and select the bellviewr.Rproj file.

If cloning the Git repository, copy the URL
[https://github.com/dche658/bellviewr.git](https://github.com/dche658/bellviewr.git)
then open RStudio Desktop and select New Project from the File menu. Then choose
the Version Control option and then Git. Enter the copied URL and then Create 
Project. All going well, the project files will be downloaded from GitHub

To run the application in RStudio Desktop, go to the Files window for the project
and open app.R. Run App should start the application and display the web page.


