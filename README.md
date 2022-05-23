# PastureMonitoring
Project to utilise Sentinel-2 imagery and custom calibrations to predict paddock level pasture availability

Thanks for choosing to use the pasture monitoring processing repository written in R.  Please note there are a few pre-requisites required for this to work:  

1. A username and password for the Copernicus Open Hub: https://scihub.copernicus.eu/dhus/#/home
2. Sen2Cor processor installed: https://step.esa.int/main/snap-supported-plugins/sen2cor/
3. An account with ShinyApps.io: https://www.shinyapps.io/
4. Point or polygon shapefiles with your locations of interest in projected coordinate system WGS84 UTM Zone XXY, where XXY matches the UTM zone of the image tile that intersects that shapefile e.g. 56S.

Take your time completing the optional parameters in the main execution script, processpasturedata.R, before you start.  This is where you choose your parent directories and location to store your large satellite image files.

It is strongly advised that during first run you select the option to halt processing after all directories are created and then manually go and download as many historic tiles as possible for your farms   

You will need your own pasture calibration models, or you can simply use the default supplied.  A calibration maker is supplied to generate calibrations from your custom calibration data.  You will also need to create a treemask for each sentinel image tile.  This can be done using the included treemask creation function from any downloaded image. If a treemask doesn't exist for a downloaded tile, it will be generated automatically. This mask can be edited manually using QGIS and the Surval add-on: https://github.com/lutraconsulting/serval/blob/master/Serval/docs/user_manual.md to ensure accuracy for each of your farms/paddocks. The best image to use to create an auto mask is a image collected when your farm was drought affected, largest difference between pasture and tree leaf greeness. The repository includes a demo shapefile and a demo calibration data set.  These can be copied and edited to include your data, or used as templates for your farms.   
