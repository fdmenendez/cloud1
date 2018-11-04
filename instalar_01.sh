#instalo para que se pueda instalar en R el paquete mlrMBO
cd
sudo dpkg --add-architecture i386
sudo apt-get update

sudo apt-get --yes install libssl-dev
sudo apt-get --yes install libcurl4-openssl-dev
sudo apt-get --yes install libxml2-dev
sudo apt-get --yes install git-core
sudo apt --yes install libgeos-dev libproj-dev libgdal-dev

sudo apt-get --yes install nano

#instalo la ultima version de R
sudo apt update
sudo apt --yes install r-base


#instalo paquetes de R
cd
sudo  Rscript --verbose  ./instalo_paquetes.r
sleep  10

#xgboost instalo la ultima version de desarrollo de XGBoost que no esta disponible en CRAN R (para histogramas)
cd
sudo rm -rf  xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
cd R-package
sudo R CMD INSTALL .
cd
sudo rm -rf  xgboost


#LightGBM instalo ya que no esta disponible en CRAN R
cd
sudo apt-get   --yes   install cmake
cd
sudo rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM
cd LightGBM/R-package
sudo  Rscript ./build_package.R
sudo  R CMD INSTALL lightgbm_*.tar.gz --no-multiarch
cd
sudo rm -rf  LightGBM



#instalo RStudio Server
cd
sudo apt-get --yes install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.1.456-amd64.deb
sudo gdebi --n rstudio-server-1.1.456-amd64.deb
cd
rm  rstudio-server-1.1.456-amd64.deb
#cambio el puerto del Rstudio Server al 80 para que se puede acceder en universidades
sudo  chmod a=rw  /etc/rstudio/rserver.conf
sudo echo "www-port=80" >> /etc/rstudio/rserver.conf
sudo rstudio-server restart
sudo rstudio-server start



#instalo Google Cloud Fuse  para poder ver el bucket
cd
export GCSFUSE_REPO=gcsfuse-`lsb_release -c -s`
echo "deb http://packages.cloud.google.com/apt $GCSFUSE_REPO main" | sudo tee /etc/apt/sources.list.d/gcsfuse.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
sudo apt-get --yes update
sudo apt-get --yes install gcsfuse
sleep  10
mkdir $HOME/cloud

#limpio el desorden de las instalaciones
sudo apt-get --yes  autoremove

