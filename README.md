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
    -   on Ubuntu `systemd` is used to schedule tasks
    -   for more more details: <https://www.phind.com/search?cache=kehos5pzvo7v8411fimrs0nq>
    -   scheduled scripts run as `root` so an SSH key needs to be set up for 
    `root` user to be able perform git operations. 
    <https://docs.github.com/en/authentication/connecting-to-github-with-ssh>


```
#!/bin/bash

# for cron jobs the $HOME variable needs to be explicitly set
export HOME=/root/

# Generate a timestamp for the log file name
DATE=$(date +"%Y-%m-%d_%H-%M")

# Redirect all output to a log file with the timestamp in the filename
exec > >(tee -a /mnt/ceph/_logs/shiny_log_$DATE.txt) 2>&1


cd /srv/shiny-server/shiny-performance-tracking
git config --global --add safe.directory /srv/shiny-server/shiny-performance-tracking
git checkout master
git pull
Rscript ExtractSaveData.R
git add .
git commit -m 'daily update of TRAINING.csv'
git push

# Restart shiny-server
systemctl restart shiny-server.service
```

