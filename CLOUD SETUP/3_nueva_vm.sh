#!/bin/bash

if [ "$#" -lt 3 ]; then
    echo "**** Error en parametros. llamar al script '$0 NOMBRE_VM NUCLEOS RAM [ZONA]'"
	exit 1
fi

if [ "$4" == "" ]; then
    echo "**** Zona no especificada. Calculando...'"
	HR="$(date +"%H" | sed -e 's/^0//g')"
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
else
	echo "**** Zona especificada = $4"
	ZONA=$4
fi
NOMBRE_VM=$1
NUCLEOS=$2
RAM=$3
RAMB=$((RAM*1024))

#SI EXISTE LA VM QUE QUIERO CREAR LA BORRO
CMD_ZONA="gcloud compute instances list --format='value(zone)' --filter='name=($NOMBRE_VM)'"
ZONA_OLD=`eval $CMD_ZONA`
gcloud compute instances delete "${NOMBRE_VM}" --quiet --zone="${ZONA_OLD}"

#LE DOY ACCESO A LA SERVICE ACCOUNT
KEY="$(gcloud iam service-accounts list --format='value(email)')"

#ASIGNO LA CONFIGURACION DONDE CREO LAS VM
gcloud config set compute/zone "${ZONA}"

gcloud beta compute instances create "${NOMBRE_VM}" --zone="${ZONA}" --machine-type=custom-"${NUCLEOS}"-"${RAMB}" --subnet=default --network-tier=PREMIUM --no-restart-on-failure --maintenance-policy=TERMINATE --preemptible --service-account="${KEY}" --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --tags=http-server --image=image-dm --boot-disk-size=10GB --boot-disk-type=pd-standard
