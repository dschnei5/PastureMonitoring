######################################################################################
############################control-post-processing.R#################################
######################################################################################

####Load functions####
{
print("Loading user defined functions...");

source(file = "functions/postprocess-images.R");

####Execute Processing####

if(length(property.nam)>0 & !post.fast){
  sapply(property.nam,post.processor)
};
if(length(property.nam)>0 & post.fast){
  postprocess.fast()
};
  
av.shiny.apps <- app.update(pm = propmeta)

MD5.check4 <- md5post(av.shiny.apps, MD5.check1)

if (length(MD5.check4$filename) >= 1) {
  Apps2GoUp <- app.deploy(MD5.check4, av.shiny.apps)
  } else {
  print("No Shiny Apps Required Updating")
  Sys.sleep(3)
  }
}

send.mail(Apps2GoUp);
if (exists("cld.dir")) {
  cld.disconnect.d()
  cld.disconnect.s()}

####END SCRIPT####