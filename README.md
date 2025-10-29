# DDcentutils

R package created by the SCSC Ecosystem Modeling and Data Consortium Team to facilitate the use of the DayCent model and visualization of results.
This package is under construction and we welcome feedback.
We created a Discussions forum on GitHub to facilitate Q&A about the package and suggest ideas for further development: <https://github.com/CSU-Soil-Carbon-Solution-Center/DDcentutils/discussions> You can also report bugs by creating an issue on the repository.
For more about creating an issue, please visit: <https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue>

Please note that the package does not include the DayCent model.
Access to the model is currently being managed by the SCSC Ecosystem Modeling and Data Consortium at Colorado State University.
For more information about model access, please visit <https://www.soilcarbonsolutionscenter.com/consortium>.

# How to Install

You can install this package using the install_github function from the devtools package:

```{r}
# devtools version 2.4.5 (2022)
devtools::install_github("CSU-Soil-Carbon-Solution-Center/DDcentutilsLite", dependencies = TRUE, upgrade = c("ask"))
```

Or alternatively you can download the compressed package (tar.gz file) and install it locally (requires Rtools):

```{r}
install.packages(here::here("DDcentutilsLite_0.1.0.tar.gz"), repos = NULL, source = TRUE)
```

# Running **DDcentutilsLite** from the Command Line


## 1. Directory Structure

Your working directory should look like this:

```
project_root/
├── sites/
│ ├── site01/
│ │ ├── site_site01.100 # 100 file for DayCent 
│ │ ├── site_site01.sch # schedule file (one for each period to be run)
│ │ ├── weather.in_Wth.csv # weather input defined in .sch
│ │ ├── soils.in # soil input
| | ├── outputs.in
│ │ └── outputs/ # created automatically
│ └── site02/
│ └── ...
└── config/
  └── {site_config}.R # defines paths to DayCent executable
```  
  
  

---

## 2. Example Config File

Save this file as:  
`config/<site_config>.R`

```
dc_exe <- "<C:/Users/YourName/DayCent>/DDcentEVI_rev491.exe"
dc_path100 <- "<C:/Users/YourName/DayCent>/100libraryfiles"
```
Both paths can be absolute or relative to your working directory.

---

## 3. Running from the Command Line
**Option 1** – Using the package interface directly

Rscript -e "DDcentutilsLite::daycent_main_cli()" --args <site> <run> <config_file> [run_eq]

Example:

Rscript -e "DDcentutilsLite::daycent_main_cli()" --args site01 base ./config/site_config.R TRUE

**Option 2** – Using the installed executable script (if available):

Rscript "$(Rscript -e 'cat(system.file("scripts","daycent-main",package="DDcentutilsLite"))')" site01 base ./config/site_config.R TRUE

---

## 4. Outputs and Logs

sites/site01/<site>_<run>.log (console and run log)
sites/site01/outputs/ (DayCent output files)

To check the log contents:

cat sites/site01/site01_base.log

---

## 5. Outputs and Logs

R version 4.2 or higher

Required R packages:

DDcentutilsLite

logger

DayCent executables:

DDcentEVI_rev491.exe

DDlist100_rev491.exe

DayCent library files:
  crop.100                 
  cult.100                 
  fert.100                 
  fire.100                
  fix.100
  graz.100
  harv.100
  irri.100
  omad.100
  tree.100  #only needed for simulating forest
  trem.100  #only needed for simulating forest

Make sure the executables and 100 library files are available in the locations defined in your config file.












