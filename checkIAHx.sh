#!/bin/bash

#cd /home/javaapps/sbt-projects/BVS-InfoButton
cd /home/heitor/sbt-projects/BVS-InfoButton

CheckFileName=`date '+%Y%m%d'.chk`
sbt 'testOnly org.bireme.infob.IAHxTest' &> ./$CheckFileName

Errors=`grep -c "*** FAILED ***" ./$CheckFileName`
if [ "$Errors" != "0" ]; then
  #sendemail -f appofi@bireme.org -u "IAHx - Check ERROR - `date '+%Y%m%d'`" -m "IAHx - Check ERROR" -a $CheckFileName -t barbieri@paho.org -cc antoniov@paho.org -s esmeralda.bireme.br -xu serverofi -xp bir@2012#
  echo "ERRO !!!"
fi

rm ./$CheckFileName

cd -
