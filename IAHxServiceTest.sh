#!/bin/bash

cd /home/javaapps/sbt-projects/BVS-InfoButton

CheckFileName=`date '+%Y%m%d'.chk`
sbt 'testOnly org.bireme.infob.IAHxServiceTest' &> /tmp/$CheckFileName

Errors=`grep -c "*** FAILED ***" /tmp/$CheckFileName`
if [ "$Errors" != "0" ]; then
  sendemail -f appofi@bireme.org -u "IAHx - Check ERROR - `date '+%Y%m%d'`" -m "IAHx - Check ERROR" -a /tmp/$CheckFileName -t barbieri@paho.org -cc antoniov@paho.org -s esmeralda.bireme.br -xu serverofi -xp bir@2012#
  rm /tmp/$CheckFileName
  exit 1
fi

rm /tmp/$CheckFileName

cd -
