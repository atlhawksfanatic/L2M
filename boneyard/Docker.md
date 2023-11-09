# Install Docker for RSelenium

Loose notes for installing Docker on an Ubuntu system to run this repository. Notes come from:  

- https://docs.docker.com/get-started/
- https://docs.docker.com/engine/install/ubuntu/

## Install using the repository

If this is a fresh install of Docker, then the next step can be ignored. However, first uninstall any old versions of Docker that may still be on the instance with:

```shell
sudo apt-get remove docker docker-engine docker.io containerd runc
```

### Set up the repository

Once that is complete, then use sudo commands to make sure the instance is up to date for all software needed for Docker:

```shell
sudo apt-get update
sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg
```

Add Docker's official GPG key:

```shell
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```

Use the following command to set up the stable repository:

```shell
echo \
  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

## Install Docker Engine

Update the apt package index, and install the latest version of Docker Engine and container:


```shell
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Docker should be installed now. As always, it is good practice to then restart your computer.

## Tweaking Docker

With Docker installed, it won't properly interact with the L2M repository unless the current user has permissions installed. To do so, run these commands but replace the `$USER` with the actual username of the user who will be using this repository. For myself, this was `robert` but obviously this will differ for each person.

```shell
sudo groupadd docker
sudo usermod -a -G docker $USER
```

Change the permissions of docker socket to be able to connect to the docker daemon `/var/run/docker.sock`:

```shell
sudo chmod 666 /var/run/docker.sock
```

A helpful Stack Overflow post on these issues that are needed for tweaking: https://stackoverflow.com/questions/48957195/how-to-fix-docker-got-permission-denied-issue

# Install the RSelenium Docker

## Pulling the Standalone Browser

Now that Docker has been successfully installed on your computer, the actual Docker container needed for the [RSelenium package](https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html) to function. There are many different browsers that can be used with RSelenium but for our purposes we will select the Firefox browser as the Docker container to use with the L2M repository:

```shell
sudo docker pull selenium/standalone-firefox
```

RSelenium now should work as long as the Firefox Docker container is running.

## Running the Standalone Browser

To run the Firefox Docker container, there needs to be a port selected for both the Docker host and for the port on the container. These two numbers can differ based on any other Docker containers that might be already running on your computer, however I am choosing 4445 as the port for the Docker host and 4444 for the port on the container within the R code and will do that for the command to start the Firefox container. If you choose a different port number for the Docker host then make sure you change the R code in this repository. But to run the given ports, use the following terminal command:

```shell
docker run -d -p 4445:4444 selenium/standalone-firefox
```

`[port number on docker host]:[port number on container]`

Terminal commands can also be called from within R. The `system()` function is what can be called in R code to run terminal commands:

```r
system("docker run -d -p 4445:4444 selenium/standalone-firefox")
```

If this is called in an R script, as it currently is in the L2M repository, then it is good practice to follow up the command with a pause in running the rest of a script. This ensures enough time for the Docker container to get up and running. The L2M code currently has a 7 second pause after loading the Docker container (`Sys.sleep(7)`).

## Keeping the Container Running After Reboots

For some people, [having the container always running](https://www.digitalocean.com/community/questions/how-to-start-docker-containers-automatically-after-a-reboot) may be preferable to running the container after every reboot of your machine. If this is the case and you do not want to continuously start the container, then add in the `--restart unless-stopped` command when you start the container:

```shell
docker run -d --restart unless-stopped -p 4445:4444 selenium/standalone-firefox
```

Other options for how a container should be restarted are also available in the [official documentation](https://docs.docker.com/config/containers/start-containers-automatically/) if always restarting is not the right option for your set-up.

<!---
https://splash.readthedocs.io/en/stable/install.html

sudo docker pull scrapinghub/splash
sudo docker run -it -p 8050:8050 --rm scrapinghub/splash

docker build -t l2m
--->