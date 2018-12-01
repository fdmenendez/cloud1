#!/bin/bash

if [ ! -f $HOME/autorun_script.sh ]; then
	echo "autorun_script.sh no encontrado en $HOME"
	exit 1
fi	

#ASIGNO LA CONFIGURACION A US-CENTRAL1-C DONDE CREO LAS VM
gcloud config set compute/zone us-central1-c

#LEVANTO LA VM DE INSTALACION
gcloud compute instances start instance-instalacion

#BORRO EL AUTORUN EXISTENTE
#gcloud compute ssh instance-instalacion --command "rm autorun_script.sh"

#MUEVO EL ARCHIVO A LA VM Y LE DOY PERMISOS DE EJECUCION
gcloud compute scp ~/autorun_script.sh instance-instalacion:~/ --force-key-file-overwrite
gcloud compute ssh instance-instalacion --command "chmod +x autorun_script.sh"

#APAGO LA VM
gcloud compute instances stop instance-instalacion

#RECREO LA IMAGEN
gcloud compute images delete image-dm --quiet
gcloud compute images create image-dm --source-disk=instance-instalacion --source-disk-zone=us-central1-c
