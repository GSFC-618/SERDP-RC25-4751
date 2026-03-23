# PEcAn Installation Instructions
Evolving instructions for deploying PEcAn on AWS, local, and other servers to support the SERDP-RC25-4751 project

<br>

## Installing on the Airborne pcluster (SMCE)
Recommended installation: pixi environment
Based on this original thread: https://github.com/orgs/ccmmf/discussions/157#discussion-9276628 <br>

To build a pixi environment to run PEcAn+SIPNET on the Airborne Pcluster: <br>
1) ssh to the pcluster

2) launch an interactive slurm session to use for building the new pixi environment. It can take a couple of minutes to spin up the session <br>
   e.g. `srun --pty bash`
   
3) Install pixi (only need to do this the first time)
Pixi docs: https://pixi.prefix.dev/latest/installation/ <br>

`curl -fsSL https://pixi.sh/install.sh | sh` <br>

and to update

`pixi self-update`

4) Create a bashrc function to load easily pixi environments. Edit your .bashrc and add to the bottom

```bash
# Custom funcion to allow loading pixi envs by path
function pixi_activate() {
        # default to current directory if no path is given
        local manifest_path="${1:-.}"
        eval "$(pixi shell-hook --manifest-path "$manifest_path")"
}
#Usage:
# For the current directory: pixi_activate
# For a specific path: pixi_activate /path/to/your/project
export -f pixi_activate
```
   
5)    Clone the git repo to your $HOME or location you want to build the PEcAn pixi envrionment OR download the pixi.toml and pixi.lock  files, e.g. <br>
```bash
  mkdir -p $HOME/pecan/
  cd $HOME/pecan/
  wget https://raw.githubusercontent.com/GSFC-618/SERDP-RC25-4751/refs/heads/main/install/pecan/pixi/linux-64/pixi.toml
  wget https://raw.githubusercontent.com/GSFC-618/SERDP-RC25-4751/refs/heads/main/install/pecan/pixi/linux-64/pixi.lock
```

6) Install PEcAn using the pixi run syntax and calling the specific install task (and the subtasks), e.g.
```bash
pixi run install_pecan
```

If you are building from scratch with the .lock file, it should build reasonably quickly. If you are building from scratch and creating a new .lock file it can take several minutes to complete

At the end of the build you should see many notifications, like below, reporting that different packages were installed into the new environment <br>

<img width="913" height="631" alt="Screenshot 2026-03-23 at 4 55 27 PM" src="https://github.com/user-attachments/assets/4a6ea1a7-f876-4cda-bbb1-46099717ff8a" />

Note: once built, your $HOME/pecan/ directory will now have a .pixi hidden folder with the full pixi environment, including the dependencies like netCDF, gdal, and jags <br>

<img width="534" height="141" alt="Screenshot 2026-03-23 at 4 57 50 PM" src="https://github.com/user-attachments/assets/4c43014c-2d92-458c-90d3-eefb13bb285d" />

<img width="585" height="153" alt="Screenshot 2026-03-23 at 4 58 09 PM" src="https://github.com/user-attachments/assets/6984ddd5-b77a-4a63-af15-2582318f012b" />


7) Test out your new pixi environment, e.g. 

You can either change to your new install location e.g. $HOME/pecan/ OR you can use the new `pixi_activate` function, e.g. 

```bash
pixi_activate $HOME/pecan/
```
This will load the pixi env into your shell. You will know the env is loaded because you should see the environment name in your shell (this env is called *serdp-rc25-4751*). See example below

<img width="663" height="63" alt="Screenshot 2026-03-23 at 4 58 50 PM" src="https://github.com/user-attachments/assets/941d70b9-9e3b-4991-8d29-5a4a97f85bfa" />


8) Quick verification of the PEcAn library installation <br>

<img width="606" height="332" alt="Screenshot 2026-03-23 at 5 00 11 PM" src="https://github.com/user-attachments/assets/3b5e1947-4dd1-470f-b31e-25f1e512ea46" />

At the R command prompt, run: 
```r
PEcAn.all::pecan_version()
```

To review the install, e.g. 

<img width="601" height="857" alt="Screenshot 2026-03-23 at 5 01 46 PM" src="https://github.com/user-attachments/assets/2df99c69-4c94-4747-b856-9d0896f6627a" />


Test load PEcAn.all
```r
 library("PEcAn.all")
```

<img width="443" height="311" alt="Screenshot 2026-03-23 at 5 03 27 PM" src="https://github.com/user-attachments/assets/fd43fb29-ee4a-4cee-b01d-fcd01ef291ee" />


