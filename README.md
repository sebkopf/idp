Isodat Data Processor (idp)
===

This package uses the access to raw isodat data formalized in the [isoread package](https://github.com/sebkopf/isoread) and provides a graphical user interface for easy and quick processing of compound specific D/H isotope data in R. Basic functionality includes quick overviews of file information, reference & data plots, as well as saving chromatograms at any zoom level as vector graphics (PDF). Export to Excel files is supported for complete datasets (peak tables and chromatograms) at the single file level and across many files at once for the peak tables. User preferences (e.g. references highlighted in chromatograms, time in minutes vs. seconds, etc.) can be saved across sessions. Key expansions of functionality include the possibility to graphically de/select peaks as standards and quickly re-evaluate the data table based on all selected standards, as well as choosing between bracketing and regression standards mode. Additional future expansions of functionality include deconvolution of overlapping peaks (as far as possible) and proper integration of peaks with unequal widths in mass 2 and 3 traces (e.g. for samples with partly D-enriched analytes). Basic functionality such as adding, editing and deleting peaks, are also already part of the GUI layout but not implemented yet (sorry). Updates will come as I find time to work on this (and/or others interested in the project contribute). The long-term vision is to incorporate this into [CSIDE](https://github.com/sebkopf/isoread) for full support of peak mapping and identification, and compound-based data aggregation.

![Screenshot of the Isodat Data Processor](/inst/doc/screenshot.png?raw=true)

![Screenshot of the Isodat Data Processor](/inst/doc/screenshot2.png?raw=true)

## Installation

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces. GTK needs to be installed prior to using this package. If you don't have R and/or GTK yet, please follow this [link](https://gist.github.com/sebkopf/9405675) for information on installing R with GTK+. 

### Install idp package

The **devtools** package provides a super convenient way of installing the **idp** package directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) 
```

This package compiles R libraries from source using the gcc compiler, which is usually already installed on Unix-based systems. If not, it is most easily acquired by installing [Apple's XCode command line tools](https://developer.apple.com/downloads/) (requires an Apple ID, make sure to install for your version of Mac OS X). On Windows, it requires installing the [RTools from CRAN](http://cran.r-project.org/bin/windows/Rtools/). You can confirm that you have it all up and running by checking that ```find_rtools()``` in the R command line returns ```TRUE```:

```coffee
library(devtools)
find_rtools()
```
Then simply install the latest version of the IDP directly from GitHub (make sure [GTK is installed](https://gist.github.com/sebkopf/9405675) first!) by running the following code (if it is the first time you install the **idp** package, all missing packages will be automatically installed as well as their respective dependencies, which might take a few minutes):

```coffee
library(devtools)
install_github("sebkopf/idp")
```

####Troubleshooting

##### RGtk2 not recognized on Windows 8 

It seems that on Windows 8, the **RGtk2** package is sometimes not recognized during package installations from source. If **idp** installation fails with an error that **RGtk2** is missing (although it can be loaded without any problems following the [installation instructions](https://gist.github.com/sebkopf/9405675)), try installing it without the automatic package loading (the step where it fails) instead: 

```coffee
install_github("sebkopf/idp", reload=F, quick=T)
```

##### Java
**idp** uses the **xlsx** package for exporting data to .xlsx files. **xlsx** uses **rJava** to access the Java API for Excel. This requires Java to be installed on your computer. If there is a problem, it might throw an error something like the one below and usually launch an installer program for Java. If this happens, please follow the link in the error message or go directly to http://java.com/en/download/index.jsp to install the newest version of the Java runtime environment (JRE) for your operating system and then restart RStudio and try again.

```coffee
No Java runtime present, requesting install.
Error: Command failed (97)
```

##### Dependencies
The dependency packages have many dependencies of their own and sometimes not all of them get installed properly all at once. If this is the case, try rerunning the dfv installation (now that fewer dependencies need to be installed) or install the failed packages manually by running ```install.packages("PACKAGE NAME", depen=T)``` from the R command line.


#### Updating to a newer version

To update an older installation of the **idp** package to the newest version, just restart R and rerun ```install_github("sebkopf/idp")```. 


##Run IDP

### In R
Once installed, you can now run the Isodat Data Processor in any R workspace (command line R, RStudio, iPython Rmagic, etc.):

```coffee
library(idp)
idp.start()
```

### From command line

Or directly from command line, a link or another script via Rscript (will start in the current directory by default but you can adjust it by changing the path in the ```setwd()``` call):

#### Unix-based systems (Linux, MacOSX)

In the terminal, type:
```coffee
Rscript -e "setwd('.'); library(idp); idp.start_from_script()"
```

#### Windows

In the command line (note that unless RScript.exe is in your PATH, you need to adjust it to point to the right directory):
```coffee
"C:\Program Files\R\R-3.1.1\bin\Rscript.exe" -e "setwd('.'); library(idp); idp.start_from_script()"
```

NOTE: when setting a starting directory in this command line/Desktop link call, such as for example ```C:\Files\```, make sure to escape the backslashes, i.e. use ```setwd('C:\\Files\\')```

