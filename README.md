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
    -	add `root ALL=(vplattner) NOPASSWD: ALL` line to `/etc/sudoers`:  allows the `root` user to execute any command as `vplattner` without password (necessary to switch to `vplattner` in the script)
    -	on Ubuntu `systemd` is used to schedule tasks
    -   for more more details: <https://www.phind.com/search?cache=kehos5pzvo7v8411fimrs0nq>

```
#!/bin/bash

# Generate a timestamp for the log file name
DATE=$(date +"%Y-%m-%d_%H-%M")

# Redirect all output to a log file with the timestamp in the filename
exec > >(tee -a /mnt/ceph/_raw_data/rat_training_172/update_shiny_app_log_$DATE.txt) 2>&1

# Switch to vplattner user and execute Git commands
su - vplattner -c "
cd /srv/shiny-server/shiny-performance-tracking
git checkout master
Rscript ExtractSaveData.R
git config --global --add safe.directory /srv/shiny-server/shiny-performance-tracking
git add .
git commit -m 'daily update of TRAINING.csv'
git push
"

# Restart shiny-server
systemctl restart shiny-server.service
```

