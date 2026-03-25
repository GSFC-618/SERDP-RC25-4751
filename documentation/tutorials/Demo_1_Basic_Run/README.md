# Instructions for running a simple single model simulation with SIPNET to test PEcAn and SIPNET installs 

After completing the PEcAn and SIPNET installation and setup, you can test your install with a basic SIPNET simulation. 

1) First, if not already working in an interactive shell create a new one for running the tests and verifying the install, e.g.

`srun -N 1 -n 2 --mem=8G --time=02:00:00 --pty /bin/bash`

2) XXXXX
3) XXXX
4) XXXX
5) XXXX
6) Activate your pixi environment, e.g.
   ```bash
   pixi_activate /path/to/pixi/env
   ```
7) XXXX
8) Run the short test run
   ```r
   Rscript workflow.R
   ```
9) If successful, you should see an output similar to this
```r
2026-03-24 10:49:30.29854 INFO   [PEcAn.utils::read.output] : Result summary:
                                Mean    Median
posix                      1.58e+07  1.58e+07
GPP                        2.80e-08  0.00e+00
NPP                        9.36e-09 -6.67e-09
TotalResp                  5.45e-08  4.00e-08
AutoResp                   1.86e-08  1.00e-08
HeteroResp                 3.59e-08  2.56e-08
SoilResp                   3.82e-08  2.72e-08
NEE                        2.65e-08  2.94e-08
AbvGrndWood                1.78e+01  1.77e+01
leaf_carbon_content        4.60e-02  1.44e-03
TotLivBiom                 2.52e+01  2.44e+01
TotSoilCarb                1.44e+01  1.49e+01
Qle                        4.37e+01  2.34e+01
Transp                     7.04e-06  0.00e+00
SoilMoist                  9.65e+01  1.04e+02
SoilMoistFrac              8.04e+00  8.68e+00
SWE                        1.25e+01  1.20e+00
litter_carbon_content      0.00e+00  0.00e+00
LAI                        6.53e-01  2.04e-02
fine_root_carbon_content   1.49e+00  8.03e-01
coarse_root_carbon_content 5.87e+00  5.85e+00
AGB                        1.78e+01  1.78e+01
time_bounds                1.83e+02  1.83e+02
GWBI                       6.09e-11  0.00e+00
mineral_N                  0.00e+00  0.00e+00
soil_organic_N             0.00e+00  0.00e+00
litter_N                   0.00e+00  0.00e+00
N2O_flux                   0.00e+00  0.00e+00
N_leaching                 0.00e+00  0.00e+00
N_fixation                 0.00e+00  0.00e+00
N_uptake                   0.00e+00  0.00e+00
CH4_flux                   0.00e+00  0.00e+00
2026-03-24 10:49:30.300057 WARN   [PEcAn.utils::read.output] :
   Variable time_bounds has 2 dimensions, it cannot be loaded and will be
   omitted.


Table: Model Output Variables and Descriptions

|Variable                   |Description                          |
|:--------------------------|:------------------------------------|
|GPP                        |Gross Primary Productivity           |
|NEE                        |Net Ecosystem Exchange               |
|TotalResp                  |Total Respiration                    |
|AutoResp                   |Autotrophic Respiration              |
|HeteroResp                 |Heterotrophic Respiration            |
|SoilResp                   |Soil Respiration                     |
|CH4_flux                   |Methane Flux                         |
|N2O_flux                   |Nitrous Oxide Flux                   |
|NPP                        |Net Primary Productivity             |
|GWBI                       |Gross Woody Biomass Increment        |
|TotLivBiom                 |Total living biomass                 |
|AGB                        |Total aboveground biomass            |
|LAI                        |Leaf Area Index                      |
|leaf_carbon_content        |Leaf Carbon Content                  |
|fine_root_carbon_content   |Fine Root Carbon Content             |
|coarse_root_carbon_content |Coarse Root Carbon Content           |
|AbvGrndWood                |Above ground woody biomass           |
|TotSoilCarb                |Total Soil Carbon                    |
|litter_carbon_content      |Litter Carbon Content                |
|Qle                        |Latent heat                          |
|Transp                     |Total transpiration                  |
|SoilMoist                  |Average Layer Soil Moisture          |
|SoilMoistFrac              |Average Layer Fraction of Saturation |
|SWE                        |Snow Water Equivalent                |
|year                       |Year                                 |
```

If all goes well you will end up with example figures in the `demo_outdir/figs` directory displaying some of the SIPNET modeling results, e.g.
<br>

<img width="800" height="600" alt="sipnet_GPP_NPP_by_time" src="https://github.com/user-attachments/assets/369cfe66-ebb6-4c4f-82ac-b9e057cadd2f" />

