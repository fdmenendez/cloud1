#!/bin/bash

if [ "$1" == "" ]; then
    echo "**** Nombre del bucket no especificado. llamar al script '$0 NOMBRE_DEL_BUCKET'"
	exit 1
fi

NOMBRE_DEL_BUCKET=$1

echo "el nombre de tu bucket es: '${NOMBRE_DEL_BUCKET}'"

#ME ASEGURO DE ESTAR EN EL HOME
cd

echo '-- ABRIENDO EL ZIP DE cloud1 ...'

#ABRO EL ZIP DE CLOUD1
#rm -rf cloud1
#unzip cloud1.zip

echo '-- CREANDO BUCKET ...\n'
#CREO EL BUCKET
gcloud config set compute/zone us-central1-c
gsutil rm -r gs://"${NOMBRE_DEL_BUCKET}"
gsutil mb -c multi_regional gs://"${NOMBRE_DEL_BUCKET}"


echo '-- LIMPIANDO ENTORNO DE EJECUCIONES ANTERIORES ...'

#ASIGNO LA CONFIGURACION DONDE CREO LAS VM
CMD_ZONA="gcloud compute instances list --format='value(zone)' --filter='name=instance-instalacion'"
ZONA=`eval $CMD_ZONA`
gcloud config set compute/zone "${ZONA}"
#gcloud config set compute/zone us-central1-c

#LIMPIO TODO EL ENTORNO
gcloud compute images delete image-dm --quiet
#gcloud compute instances delete instance-ranger --quiet


echo '-- MOVIENDO ARCHIVOS AL BUCKET ...'

#COPIO DEL BUCKET PUBLICO
#gsutil -m cp -r cloud1 gs://"${NOMBRE_DEL_BUCKET}"
gsutil -m rsync -d -r gs://fdm1cloudanalysis gs://"${NOMBRE_DEL_BUCKET}"

#LE DOY ACCESO A LA SERVICE ACCOUNT
KEY="$(gcloud compute instances describe instance-instalacion --format='value(serviceAccounts[0].email)')"

echo "-- DANDO ACCESO AL BUCKET A LA SERVICE ACCOUNT $KEY ..."

#SE CREA EL JSON CON LOS PERMISOS PARA EL BUCKET
[ -f "perms.txt" ] && mv "perms.txt" "perms-$(date +%Y%m%d%H%M%S).txt"

gsutil iam get gs://"${NOMBRE_DEL_BUCKET}" > perms.txt

REP='{"members": ["serviceAccount:'
REP+=$KEY
REP+='"],"role": "roles/storage.objectAdmin"}'
echo "$REP"
jq ".bindings[.bindings| length] |= . + ${REP}" perms.txt > perms_new.txt

gsutil iam set perms_new.txt gs://"${NOMBRE_DEL_BUCKET}"

echo "-- CREANDO EL JSON CON LA SERVICE KEY ..."
#CREA EL JSON CON LA SERVICE KEY
rm privatekey_inicial.json
gcloud iam service-accounts keys create ~/privatekey_inicial.json --iam-account "${KEY}"

echo "-- LIMPIANDO CRONTAB ..."

#ELIMINO EL CRONTAB VIEJO SI EXISTIA
gcloud compute instances start instance-instalacion
sleep 5

EMPTY_CRON="# Edit this file to introduce tasks to be run by cron.
# 
# Each task to run has to be defined through a single line
# indicating with different fields when the task will be run
# and what command to run for the task
# 
# To define the time you can provide concrete values for
# minute (m), hour (h), day of month (dom), month (mon),
# and day of week (dow) or use '*' in these fields (for 'any').# 
# Notice that tasks will be started based on the cron's system
# daemon's notion of time and timezones.
# 
# Output of the crontab jobs (including errors) is sent through
# email to the user the crontab file belongs to (unless redirected).
# 
# For example, you can run a backup of all your user accounts
# at 5 a.m every week with:
# 0 5 * * 1 tar -zcf /var/backups/home.tgz /home/
# 
# For more information see the manual pages of crontab(5) and cron(8)
# 
# m h  dom mon dow   command"

echo "$EMPTY_CRON" | gcloud compute ssh instance-instalacion --command "crontab"
gcloud compute instances stop instance-instalacion


#LEVANTO LA VM DE INSTALACION
gcloud compute instances start instance-instalacion
sleep 5

#MUEVO EL JSON A LA VM
#gcloud compute ssh instance-instalacion --command "cd; rm privatekey_inicial.json"
gcloud compute scp ~/privatekey_inicial.json instance-instalacion:~/ --force-key-file-overwrite
if [[ $? = 1 ]]; then
	gcloud compute scp ~/privatekey_inicial.json instance-instalacion:~/ --force-key-file-overwrite
fi

echo 'MONTANDO BUCKET ...'
#MONTO EL BUCKET
gcloud compute ssh instance-instalacion --command "gcsfuse --implicit-dirs --file-mode 777 --dir-mode 777 --key-file $HOME/privatekey_inicial.json '${NOMBRE_DEL_BUCKET}' $HOME/cloud"
if [[ $? = 1 ]]; then
   echo "FALLO EL MONTADO DEL BUCKET. TERMINANDO SCRIPT Y APAGANDO VM..."
	gcloud compute instances stop instance-instalacion   
   exit 1
fi

echo "ACTUALIZANDO CRONTAB CON LOS NUEVOS VALORES ..."

#EDITO EL CRONTAB
CRON_CONTENT="# Edit this file to introduce tasks to be run by cron.
# 
# Each task to run has to be defined through a single line
# indicating with different fields when the task will be run
# and what command to run for the task
# 
# To define the time you can provide concrete values for
# minute (m), hour (h), day of month (dom), month (mon),
# and day of week (dow) or use '*' in these fields (for 'any').# 
# Notice that tasks will be started based on the cron's system
# daemon's notion of time and timezones.
# 
# Output of the crontab jobs (including errors) is sent through
# email to the user the crontab file belongs to (unless redirected).
# 
# For example, you can run a backup of all your user accounts
# at 5 a.m every week with:
# 0 5 * * 1 tar -zcf /var/backups/home.tgz /home/
# 
# For more information see the manual pages of crontab(5) and cron(8)
# 
# m h  dom mon dow   command
@reboot gcsfuse --implicit-dirs --file-mode 777 --dir-mode 777 --key-file $HOME/privatekey_inicial.json $NOMBRE_DEL_BUCKET $HOME/cloud
@reboot /bin/bash $HOME/autorun_script.sh"

echo "$CRON_CONTENT" | gcloud compute ssh instance-instalacion --command "crontab"

#APAGO LA VM DE INSTALACION
gcloud compute instances stop instance-instalacion

echo "CREANDO IMAGEN ..."

#CREO LA IMAGEN
gcloud compute images create image-dm --source-disk=instance-instalacion --source-disk-zone=us-central1-c

#echo "CREANDO LA VM instance-ranger ..."

#YA NO CREO LA VM GRANDE YA QUE SE VAN A CREAR POR EXPERIMIENTO
#gcloud beta compute instances create instance-ranger --zone=us-central1-c --machine-type=n1-highmem-8 --subnet=default --network-tier=PREMIUM --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --service-account="${KEY}" --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --tags=http-server --image=image-dm --boot-disk-size=10GB --boot-disk-type=pd-standard --boot-disk-device-name=instance-ranger


#SALIDA
echo 'SCRIPT FINALIZADO CON EXITO!!!'


