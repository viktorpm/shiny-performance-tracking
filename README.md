### Setting Up Shiny Server

- For detailed instructions check our lab documentation on [how to set up a Shiny Server](https://github.com/viktorpm/limlab_documentation/blob/main/docs/Setting%20up%20Shiny%20server.md). Access to the GitHub repository is required for viewing.

### Installing R Packages System-Wide

- Use `sudo su` mode on `akramihpc1` to install R packages for system-wide availability.

### Reproducibility Across Computers

- Currently, `library(renv)` is not implemented for reproducibility across computers.

### .Rprofile

- Utilize `.Rprofile` for:
    - Loading utility functions.
    - Building paths to data files based on the computer.
    - Loading package dependencies.

### Scheduled data processing

- `update_shiny_app.sh` bash script, path: `/etc/cron.daily` on our VM1.
- On Ubuntu, `systemd` is used for scheduling tasks.
- Configuration files are stored at:
    - `/etc/systemd/system/update_shiny_app.service`
    - `/etc/systemd/system/update_shiny_app.timer`
- Scheduled scripts run as `root`, requiring an SSH key for the `root` user to perform git operations. Refer to [GitHub's SSH documentation](https://docs.github.com/en/authentication/connecting-to-github-with-ssh) for setup instructions.

### Useful Commands

- Restart timers after making changes in the configuration files: `sudo systemctl daemon-reload`
- List running timers: `systemctl list-timers`
- For more details check our lab documentation on [scheduling bash scripts](https://github.com/viktorpm/limlab_documentation/blob/main/docs/Scheduling%20and%20logging%20bash%20scripts.md). Access to the GitHub repository is required for viewing.

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

