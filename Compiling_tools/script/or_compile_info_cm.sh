#!/bin/sh

cd ..

echo "       CHARACTER VERS*32, BDATE*32, BTIME*32,BTAG*80, MSGO*68,BNAME*32"
echo "       CHARACTER YEARSTRING*10"
echo "       INTEGER LEN_VERS, LEN_BDATE, LEN_BTIME, PMSG,LEN_MSG,LENYS"
echo "       INTEGER LENBNAM"
echo " "
echo "       DATA VERS/\"OpenRadioss\"/"
echo "       PARAMETER (LEN_VERS=11)"

echo " "
echo "       DATA BDATE/__DATE__/"

echo " "
echo "       DATA BTIME/__TIME__/"
echo " "
echo "       DATA BNAME/\"$5\"/"
echo " "

#Build Tag
if [ -z "$GITHUB_SHA" ]
then
  echo "       DATA BTAG/\"  \"/ "
else
  echo "       DATA BTAG/'CommitID: $GITHUB_SHA'/ "
fi

if [ $2 = 1 ]; then
echo "       PARAMETER (PMSG=1)"
echo "       DATA  MSGO/'$6'/"
echo "       PARAMETER (LEN_MSG=$LEN_MSG)"
else
echo "       PARAMETER (PMSG=0)"
echo "       DATA  MSGO/' '/"
echo "       PARAMETER (LEN_MSG=1)"
fi
             

