#!/bin/bash

if [ "$1" == "" ]; then
    echo "**** Password del usuario de R no especificada. llamar al script '$0 R_PASSWORD'"
	exit 1
fi

R_PASSWORD=$1

#ME ASEGURO DE ESTAR EN EL HOME
cd

echo 'CREANDO instance-instalacion ....'

#ASIGNO LA CONFIGURACION A US-CENTRAL1-C DONDE CREO LAS VM
gcloud config set compute/zone us-central1-c

#SI EXITE LA VM LA BORRO
gcloud compute instances delete instance-instalacion --quiet

#CREO LA VM DE INSTALACION
gcloud beta compute instances create instance-instalacion --zone=us-central1-c --machine-type=n1-standard-2 --subnet=default --network-tier=PREMIUM --maintenance-policy=MIGRATE --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --tags=http-server --image=ubuntu-minimal-1804-bionic-v20180814 --image-project=ubuntu-os-cloud --boot-disk-size=10GB --boot-disk-type=pd-ssd --boot-disk-device-name=instance-instalacion

#PERMITO EL TRAFICO EN HTTP
gcloud compute firewall-rules create default-allow-http --direction=INGRESS --priority=1000 --network=default --action=ALLOW --rules=tcp:80 --source-ranges=0.0.0.0/0 --target-tags=http-server

echo 'COPIANDO ARCHIVOS A instance-instalacion ...'

#MUEVO LOS ARCHIVOS A LA VM CREADA
gcloud compute scp ~/instalar_01.sh ~/autorun_script.sh ~/instalo_paquetes.r instance-instalacion:~/ --force-key-file-overwrite

#SETEO MI ZONA HORARIA PARA QUE NO PREGUNTE DESPUES EL SCRIPT DE INSTALACION
#gcloud compute ssh instance-instalacion --command 'export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true;echo -e "tzdata tzdata/Areas select America\ntzdata tzdata/Zones/America select Argentina/Buenos_Aires" > /tmp/tz ; sudo debconf-set-selections /tmp/tz; sudo rm /etc/localtime /etc/timezone; sudo dpkg-reconfigure -f non-interactive tzdata'
gcloud compute ssh instance-instalacion --command 'export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true; sudo ln -fs /usr/share/zoneinfo/America/Argentina/Buenos_Aires /etc/localtime; sudo dpkg-reconfigure -f non-interactive tzdata'

echo 'INSTALANDO PAQUETES EN instance-instalacion ...'

#CORRO EL SCRIPT instalar_01.sh
gcloud compute ssh instance-instalacion --command "chmod +x *.sh"
gcloud compute ssh instance-instalacion --command "DEBIAN_FRONTEND=noninteractive ./instalar_01.sh"

echo 'CAMBIANDO PASSWORD ... '

#SETEO LA PASSWORD DE ROOT
USUARIO="$(gcloud compute ssh instance-instalacion --command "whoami")"
echo -e "$R_PASSWORD\n$R_PASSWORD" | gcloud compute ssh instance-instalacion --command "sudo passwd $USUARIO"

echo 'APAGANDO instance-instalacion ... '
gcloud compute instances stop instance-instalacion

#SALIDA

echo 'SCRIPT FINALIZADO CON EXITO!!!'
echo "VM instance-instalacion APAGADA."
echo "++++++++++++++++++++++++++++++++++"
echo "+++++++++ DATOS RSTUDIO ++++++++++"
echo "++ Nombre de usuario: $USUARIO"
echo "++ Password: $R_PASSWORD"
echo "++++++++++++++++++++++++++++++++++"
