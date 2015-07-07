#!/bin/bash -x
MYPATH=$(dirname $(readlink -f $0))

if [ "$#" -lt 2 ]; then
    echo "usage : $0 <a tix file name> <folder1> <folder2> ..."
    exit 1;
fi

tixname=$1
shift
random=`echo $RANDOM`
tmpfile="/tmp/$(basename $0).$$.list"
for d in $@; do
    find $d -name ${tixname} > ${tmpfile}
done

list=""
for i in $(cat ${tmpfile}); do
    list="${list} $i"
done

if [ "${list}" == "" ]; then
    echo "cannot find ${tixname}"
    rm ${tmpfile}
    exit 0;
else
    rm -f ${MYPATH}/../sum.${tixname}
    hpc sum ${list} --output=${MYPATH}/../sum.${tixname}
    if [ "$?" -eq 0 ]; then
	for i in $(cat ${tmpfile}); do
	    echo "rm $i"
	    rm $i
	done
	rm ${tmpfile}

	echo "generate coverage report .."
	hpc markup --srcdir=${MYPATH}/.. --destdir=${MYPATH}/../report  ${MYPATH}/../sum.${tixname} 
    fi
fi

