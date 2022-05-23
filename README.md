# PastureMonitoring
Project to utilise Sentinel-2 imagery and custom calibrations to predict paddock level pasture availability

Thanks for choosing to use the pasture monitoring processing repository written in R.  Please note there are a few pre-requisites required for this to work:  

1. R Programming Language installed
2. A username and password for the Copernicus Open Hub: https://scihub.copernicus.eu/dhus/#/home
3. Sen2Cor processor installed: https://step.esa.int/main/snap-supported-plugins/sen2cor/
4. An account with ShinyApps.io: https://www.shinyapps.io/
5. Point or polygon shapefiles with your locations of interest in projected coordinate system WGS84 UTM Zone XXY, where XXY matches the UTM zone of the image tile that intersects that shapefile e.g. 56S.

Take your time completing the optional parameters in the settings/settings.R script with a text editor initially.  This is where you choose your parent directories and location to store your large satellite image files. You will also need to define your usernames etc. Note that when the processing script identifies an unknown username it will interactively ask for passwords and store them in your machines keychain for subsequent runs. If you change passwords you will need to manually update your keychain using the functions available in the "keyring" R library. 

Once initialy setup is complete, the processing can be initiated interactively by opening the processpasturedata.R with Rscript.  It can also be "sourced" from within the R-console or R-Studio.  After the 1st successful complete run of the processpasturedata.R script the processpasturedata-auto.R script can be used automatically initiate the processing on a schedule.  This can be done using Task Scheduler on Windows or as a Cron job on Linux ensuring you select Rscript as the software to open it with.  If you really want to be tricky, why not install node-red and control the processing from your own dashboard! We will include the required flow to get you started soon. 

It is strongly advised that during first run you select the option to halt processing after all directories are created and shapefiles checked then manually go and download as many historic tiles as possible for your farms.  The script will inform you interactively when to select stop.  If you add new farm or paddock shapefile you will need to rerun the interactive script to set it up.   

Generating your own pasture calibration model is advised, otherwise you can simply use the default calibration supplied.  A calibration maker is supplied to generate calibrations from your custom calibration data.  You will also need to create a treemask for each sentinel image tile.  This can be done using the included treemask creation function from any downloaded image. If a treemask doesn't exist for a downloaded tile, it will be generated automatically. This mask can be edited manually using QGIS and the Surval add-on: https://github.com/lutraconsulting/serval/blob/master/Serval/docs/user_manual.md to ensure accuracy for each of your farms/paddocks. The best image to use to create an auto mask is a image collected when your farm was drought affected, largest difference between pasture and tree leaf greeness. The repository includes a demo shapefile and a demo calibration data set.  These can be copied and edited to include your data, or used as templates for your farms.   

Please note: We are building this repository still.  Working processor not yet available, please check again soon...
