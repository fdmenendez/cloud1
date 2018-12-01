#!/bin/bash

shopt -s xpg_echo
if [ "$1" == "" ]; then
	echo "**** Timer no especificado. seteando revision cada 600 segundos..."
	WAIT=600
else
	WAIT=$1
	echo "**** Timer especificado. seteando revision cada $WAIT segundos..."
fi

#LE DOY AL USUARIO LA LISTA DE VMs
echo "Selecciona la VM a monitorear de la lista"
select vm in $(gcloud compute instances list --format='value(NAME)'); do 
	echo "Elegiste la VM: $vm"
	NOMBRE_VM=$vm
	break ;
done

#ASIGNO LA CONFIGURACION DONDE CREO LAS VM
CMD_ZONA="gcloud compute instances list --format='value(zone)' --filter='name=($NOMBRE_VM)'"
ZONA=`eval $CMD_ZONA`
gcloud config set compute/zone "${ZONA}"

#LE DOY ACCESO A LA SERVICE ACCOUNT
KEY="$(gcloud iam service-accounts list --format='value(email)')"

#AVERIGUO EL TIPO DE LA MAQUINA ORIGINAL
TYPE="$(gcloud compute instances describe $NOMBRE_VM --format='value(machineType)' | sed 's/.*\///')"

if [ "$(gsutil ls | wc -l)" -eq 1 ]; then
	BUCKET="$(gsutil ls | head -1)"
else

	#LE DOY LA OPCION AL USUARIO SI HAY MAS DE UN BUCKET EN EL PROYECTO
	echo "Selecciona el numero de bucket de la lista"
	select buck in $(gsutil ls); do 
		echo "Elegiste el bucket: $buck"
		BUCKET=$buck
		break ;
	done
fi

#SETEO EL NOMBRE DEL FICHERO TESTIGO
FINAL="${BUCKET}cloud1/work/$NOMBRE_VM/finished"

while true
do
	STATUS="$(gcloud compute instances describe "${NOMBRE_VM}" --format='value(status)')"
	if [[ "$STATUS" == "RUNNING" || "$STATUS" == "PROVISIONING" || "$STATUS" == "STAGING" ]]; then
		FECHA="$(date)"
		echo "Status = $STATUS... Esperando $WAIT segundos... ultima revision $FECHA\n"
		sleep $WAIT
	else
		echo "Status = $STATUS... Recreando la VM..."
		gsutil -q stat "${FINAL}"
		if [ $? -ne 0 ]; then
			ZONA_ANT=`eval $CMD_ZONA`
			#BORRO LA VM
			gcloud compute instances delete "${NOMBRE_VM}" --quiet
					
			HR="$(date +'%H')"
			if ((HR >= 0 && HR < 6)) 
			then
				ZONA="us-east1-b"
			fi
			if ((HR >= 6 && HR < 12)) 
			then
				ZONA="us-west1-b"
			fi
			if ((HR >= 12 && HR < 18)) 
			then
				ZONA="asia-northeast1-b"
			fi
			if ((HR >= 18 && HR < 24)) 
			then
				ZONA="europe-west1-b"
			fi
			echo "**** Usando $ZONA"
			
			#LE DOY ACCESO A LA SERVICE ACCOUNT
			KEY="$(gcloud iam service-accounts list --format='value(email)')"

			#ASIGNO LA CONFIGURACION DONDE CREO LAS VM
			gcloud config set compute/zone "${ZONA}"

			gcloud beta compute instances create "${NOMBRE_VM}" --zone="${ZONA}" --machine-type="${TYPE}" --subnet=default --network-tier=PREMIUM --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --service-account="${KEY}" --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --tags=http-server --image=image-dm --boot-disk-size=10GB --boot-disk-type=pd-standard
		else
			echo "*** el script finalizo con exito -  la VM esta apagada"
			exit 0
		fi
	fi
		
done
