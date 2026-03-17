# Documentation for the SERDP RC25-4751 Project

## Setting up PEcAn
GitHub: https://github.com/PecanProject/pecan


Airborne SMCE install location (global location) <br>
Installed using conda-store <br>

```global-pecan  /home/conda/global/envs/global-pecan```

<br>

Alexey's Pixi build instructions <br>
https://github.com/orgs/ccmmf/discussions/157#discussion-9276628

<br>

### SIPNET INSTALL
Example 1. 
GitHub repo for PEcAn: https://github.com/pecanproject/sipnet <br>
Instructions for install: https://github.com/PecanProject/sipnet/blob/master/docs/user-guide/getting-started.md <br>

Airborne SMCE pcluster system-wide install location (for all users): `/shared-ebs/software/models/sipnet`

```
git clone https://github.com/pecanproject/sipnet
cd sipnet
make
```

Example X. Using Singularity

```
module load singularity
singularity pull docker://pecan/model-sipnet-git:develop
```

#### Test runs
CCMMF example: https://github.com/ccmmf/workflows/tree/main/1a_single_site

SERDP test run example:

SDA test run example:

#### Data storage
