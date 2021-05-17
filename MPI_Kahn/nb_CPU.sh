# Script retournant le nombre de slots disponibleen local

echo $(cat /proc/cpuinfo | grep processor | wc -l)