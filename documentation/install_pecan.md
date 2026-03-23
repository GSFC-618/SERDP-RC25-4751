# PEcAn Installation Instructions
Evolving instructions for deploying PEcAn on AWS, local, and other servers to support the SERDP-RC25-4751 project

<br>

## Installing on the Airborne pcluster (SMCE)
Recommended installation: pixi environment

To build a pixi environment to run PEcAn+SIPNET on the Airborne Pcluster: <br>
1) ssh to the pcluster

2) launch an interactive slurm session to use for building the new pixi environment. It can take a couple of minutes to spin up the session <br>
   e.g. `srun --pty bash`
   
3) Clone the git repo to your $HOME or location you want to build the PEcAn pixi envrionment OR download the pixi.toml and pixi.lock  files, e.g. <br>
```bash
  mkdir -p $HOME/pecan/
  cd $HOME/pecan/
  wget https://raw.githubusercontent.com/GSFC-618/SERDP-RC25-4751/refs/heads/main/install/pecan/pixi/linux-64/pixi.toml
  wget https://raw.githubusercontent.com/GSFC-618/SERDP-RC25-4751/refs/heads/main/install/pecan/pixi/linux-64/pixi.lock
```

4) XXXX
4) 
