# Base R image
FROM rocker/r-base
ENV TZ=America/New_York

# Install git and dependencies
RUN apt update -qq && apt install -y \
    cron \
    git \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev

# Install pak using the simpler method
RUN R -e "install.packages('pak')"

# Use pak to install R dependencies
RUN R -e "pak::pkg_install(c('dplyr','stringr','readr','jsonlite','janitor','lubridate','RPostgres','DBI','httr2','pool','blastula','purrr'))"

WORKDIR /home/r-environment

# Create environment export script
RUN echo '#!/bin/bash\n\
# Export Docker environment variables to file\n\
printenv | grep -E "^(DB_|EMAIL_|TZ)=" > /tmp/docker.env' > /docker-entrypoint.sh && \
    chmod +x /docker-entrypoint.sh

# Create a script that pulls latest code and runs scraper
RUN echo '#!/bin/bash\n\
# Load environment variables\n\
if [ -f /tmp/docker.env ]; then\n\
    set -a\n\
    . /tmp/docker.env\n\
    set +a\n\
fi\n\
cd /home/r-environment\n\
rm -rf temp_repo\n\
git clone https://github.com/ryanscharf/Scratchoff.git temp_repo\n\
cp temp_repo/scratchoff_scraper_api.R .\n\
rm -rf temp_repo\n\
Rscript /home/r-environment/scratchoff_scraper_api.R' > /home/r-environment/run_scraper.sh && \
    chmod +x /home/r-environment/run_scraper.sh

# Update cron to use the script (runs at 3 AM daily)
RUN echo "0 3 * * * /home/r-environment/run_scraper.sh >> /var/log/cron.log 2>&1" > /etc/cron.d/scraper-cron && \
    chmod 0644 /etc/cron.d/scraper-cron && \
    crontab /etc/cron.d/scraper-cron

# Create log file
RUN touch /var/log/cron.log

# Run cron in foreground
CMD ["sh", "-c", "/docker-entrypoint.sh && cron && tail -f /var/log/cron.log"]
