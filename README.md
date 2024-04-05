-   setting up Shiny Server:
    <https://www.phind.com/search?cache=xbbosn359065x7dn4tunghvu&source=sidebar>
-   install R packages in `sudo su` mode on `akramihpc1` for system-wide
    availability!
-   `library(renv)` for reproducibility across computers: not implemented for now
-   `.Rprofile`:
    -   loading utility functions
    -   building paths to data files computer dependently 
    -   loading package dependencies


-   running `update_shiny_app.sh` bash script:

    -   script path: `/etc/cron.daily` on our VM1 
    -	  `systemd` is used on Ubuntu to schedule tasks: `https://www.phind.com/search?cache=kehos5pzvo7v8411fimrs0nq`

```
#!/bin/bash

# Redirect all output to a log file
exec > >(tee -a ~/update_shiny_app.log) 2>&1

# Change directory
cd /srv/shiny-server/shiny-performance-tracking

# Checkout master branch
git checkout master

# Run R script
Rscript ExtractSaveData.R

# Add changes to git
git add .

# Commit changes
git commit -m "daily update of TRAINING.csv"

# Push changes to remote repository
git push

# Restart shiny-server
systemctl restart shiny-server

```
