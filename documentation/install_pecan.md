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
7) 
