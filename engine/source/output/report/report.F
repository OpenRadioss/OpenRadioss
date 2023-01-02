Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  REPORT                        source/output/report/report.F 
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE REPORT(IH3DREADER,N,IMVW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "commandline.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
#include      "drape_c.inc"
#include      "submodel.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .         IH3DREADER,N,IMVW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .         IOUTREP,I,LEN_CWD, OUT_TMP, IS_MESSAGE, IS_WARNING,IS_FILE_FOUND, IEND
      INTEGER*4 GETCWD, STATUS

      CHARACTER FILNAM*100,MVWNAM*100
      CHARACTER LINE*200
      CHARACTER CWD*ncharline
      CHARACTER*100  FMW
      CHARACTER*8  DATE
      CHARACTER*10  TIME
      CHARACTER ELTYP(0:105)*5
      CHARACTER*256 OUTNAME
      CHARACTER*120 CART  
      my_real
     .   ENTOT, ENTOT1, ERR, ERR1, X99, EMASS,MAS,
     .   VX,VY,VZ,DT05,ENTMP(20)  ,RTMP(10),
     .   MVX, MVY, MVZ, TS, MAS2, WEWE2, ENTOT1B
      DOUBLE PRECISION ETIME, RETIME, TT0,
     .                 ENCIND, XMOMTD, YMOMTD, ZMOMTD,
     .                 XMASSD, ENROTD, ENINTD, ENCIND2,
     .                 ENROTD2, ENTOTB, EAMSD 
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
      DATA ELTYP/'FIXED',
     1           'SOLID','QUAD ','SHELL','TRUSS','BEAM ',
     2           'SPRIN','SH_3N','     ','AIRBA','INTER',
     3           'NODE ','     ','     ','     ','     ',
     4           '     ','     ','     ','     ','     ',
     5           '     ','     ','     ','     ','     ',
     6           '     ','     ','     ','     ','     ',
     7           '     ','     ','     ','     ','     ',
     8           '     ','     ','     ','     ','     ',
     9           '     ','     ','     ','     ','     ',
     A           '     ','     ','     ','     ','RENAU',
     B           'SPCEL','FVBAG','     ','     ','     ',
     C           '     ','     ','     ','     ','     ',
     D           '     ','     ','     ','     ','     ',
     E           '     ','     ','     ','     ','     ',
     F           '     ','     ','     ','     ','     ',
     G           '     ','     ','     ','     ','     ',
     H           '     ','     ','     ','     ','     ',
     I           '     ','     ','     ','     ','     ',
     J           '     ','     ','     ','     ','     ',
     K           '     ','     ','     ','     ','XELEM',
     K           'IGE3D','     ','     ','     ','     '/
c
      DATA X99/99.9/
C=======================================================================
      OUT_TMP = 777777
      IF(ISPMD == 0) THEN 
c
       ENTOT = ENCIN + ENINT + ENROT
       ENTOTB = ENTOT + ENCIN2 + ENROT2 
       ENTOT1=ENTOT0 + TFEXT
       ENTOT1B=ENTOT0 + TFEXT + DELTAE + TFEXT_MD       
       IF(ABS(ENTOT1B)>EM20)THEN
         ERR = ENTOTB/ENTOT1B - ONE
         ERR1 = MAX(-X99, MIN(X99,ERR*HUNDRED))     
       ELSE
        ERR = ZERO
        ERR1 =ZERO
       ENDIF
       EMASS = (XMASS - MASS0) / MASS0
        
       CALL DATE_AND_TIME(DATE,TIME)
       STATUS = GETCWD(CWD)
       LEN_CWD = LEN_TRIM(CWD)
       FILNAM=ROOTNAM(1:ROOTLEN)//'_report.html'
       MVWNAM=ROOTNAM(1:ROOTLEN)//'.mvw'
       IOUTREP = 123124

       LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM)) 

       OPEN(UNIT=IOUTREP,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',
     .        FORM='FORMATTED',STATUS='UNKNOWN')
 
       WRITE(IOUTREP,'(A)')'<html>'
       WRITE(IOUTREP,'(A)')'<head>'
       IF(GOT_INSPIRE_ALM == 1)THEN
         WRITE(IOUTREP,'(A,I4,A)')'<title>Altair Solver ',2022,' Report</title>'
       ELSE
         WRITE(IOUTREP,'(A,I4,A)')'<title>Altair Radioss(TM) ',2022,' Report</title>'
       ENDIF
       WRITE(IOUTREP,'(A)')'<script language="JavaScript">'
       WRITE(IOUTREP,'(A)')'function h3d()'
       WRITE(IOUTREP,'(A)')'{'
       WRITE(IOUTREP,'(A)')'winProps = "height=600,width=520,location=no,scrollbars=no,menubars=no,toolbars=no,
     .                      status=no,resizable=yes";'
       WRITE(IOUTREP,'(A,A,A)')'window.open("',ROOTNAM(1:ROOTLEN),'_frames.html","H3D",winProps);'
       WRITE(IOUTREP,'(A)')'}'
       WRITE(IOUTREP,'(A)')'</script>'
       WRITE(IOUTREP,'(A)')'</head>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<body bgcolor="#FFFFFF" text="#000000">'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<table>'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td>'
       IF(GOT_INSPIRE_ALM == 1)THEN
       WRITE(IOUTREP,'(A,I10,A)')'<font face="Arial" size="5"><b><i>Altair Solver ',2022,' Report</i></b></font><br>'
       ELSE
       WRITE(IOUTREP,'(A,I10,A)')'<font face="Arial" size="5"><b><i>Altair Radioss(TM) ',2022,' Report</i></b></font><br>'
       ENDIF
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="4"><b><i> </i></b></font>'
       WRITE(IOUTREP,'(A)')'</td></tr>'
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<hr size="3" color="#000000" noshade>'
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="2">'
       WRITE(IOUTREP,'(11A)')'Run submitted ',DATE(1:4),'/',DATE(5:6),'/',DATE(7:8),' ,  ',TIME(1:2),'h',TIME(3:4),'mn'
       WRITE(IOUTREP,'(A)')'<p>'
       IF(GOT_INSPIRE_ALM == 1)THEN
         WRITE(IOUTREP,'(4A)')'Solver Input file : ',CWD(1:LEN_CWD),'/',ROOTNAM(1:ROOTLEN),'_0000.rad'
         WRITE(IOUTREP,'(A)')'<p>'
         WRITE(IOUTREP,'(4A)')'Solver Input file : ',CWD(1:LEN_CWD),'/',ROOTNAM(1:ROOTLEN),'_0001.rad'
         WRITE(IOUTREP,'(A)')'<p>'
       ELSE
         WRITE(IOUTREP,'(4A)')'Starter Input file : ',CWD(1:LEN_CWD),'/',ROOTNAM(1:ROOTLEN),'_0000.rad'
         WRITE(IOUTREP,'(A)')'<p>'
         WRITE(IOUTREP,'(4A)')'Engine Input file : ',CWD(1:LEN_CWD),'/',ROOTNAM(1:ROOTLEN),'_0001.rad'
         WRITE(IOUTREP,'(A)')'<p>'
       ENDIF
       WRITE(IOUTREP,'(A)')'<br>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="4"><b><i>Run summary</i></b></font>'
       WRITE(IOUTREP,'(A)')'<hr size="1" color="#000000" noshade>'
       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<li type="square">'
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Finite element model information</b></font>'
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'<ul>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'


       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<li><b>Model Information</b>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of parts: &nbsp;&nbsp;</td>'
       WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NPART,'</td>'
       WRITE(IOUTREP,'(A)')'</tr>'
       IF(NUMMAT-1 >= 1)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Material laws: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMMAT-1,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMGEO /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of properties: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMGEO,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NFUNCT /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of functions: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NFUNCT,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '


       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<li><b>Mesh information</b>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of nodes: &nbsp;&nbsp;</td>'
       WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMNODG,'</td>'
       WRITE(IOUTREP,'(A)')'</tr>'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of elements: &nbsp;&nbsp;</td>'
       WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELQG+NUMELSG+NUMELCG+NUMELTG+NUMELPG+NUMELRG,'</td>'
       WRITE(IOUTREP,'(A)')'</tr>'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of parts: &nbsp;&nbsp;</td>'
       WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NPART,'</td>'
       WRITE(IOUTREP,'(A)')'</tr>'
       IF(NRBODY /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Rigid bodies: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NRBODY,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NRBE2G /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Rbe2s: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NRBE2G,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NRBE3G /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Rbe3s: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NRBE3G,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<li><b>Elements</b>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       IF(NUMELQG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 2D solid elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELQG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELSG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D solid elements (TETRA4 & BRICK): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELSG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELS10G /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D solid elements (TETRA10): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELS10G,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELS16G /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D solid elements (SOLID16): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELS16G,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELS20G /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D solid elements (SOLID20): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELS20G,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELCG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D shell elements (4-NODES): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELCG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELTGG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D shell elements (3-NODES): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELTGG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELTRG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D truss elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELTRG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELPG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D beam  elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELPG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELRG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3D spring  elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELRG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELXG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of multipurpose elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELXG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMELIG3D /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of iso-geometrics elements: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMELIG3D,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMSPHG /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of smooth particles (SPH CELLS): &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMSPHG,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '

       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<li><b>Loads and boundaries</b>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       IF(NUMBCS /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Boundary conditions : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMBCS,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NCONLD /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of cloads : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NCONLD,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NINTER /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of interfaces : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NINTER,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NFXBODY /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of flexible bodies : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NFXBODY,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRAV /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of gravities : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRAV,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NFXVEL /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of imposed disp/vel/acc : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NFXVEL,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NINVEL /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Initial velocities : &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NINVEL,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NVOLU /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of monitored volumes: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NVOLU,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NRWALL /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Rigid walls: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NRWALL,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<p>'

       WRITE(IOUTREP,'(A)')'<p>'
       IF(GOT_INSPIRE_ALM == 1)THEN
         WRITE(IOUTREP,'(A)')'<li><b>Solver tools</b>'
       ELSE
         WRITE(IOUTREP,'(A)')'<li><b>Radioss tools</b>'
       ENDIF
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       IF(NCLUSTER /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of Clusters: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NCLUSTER,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NDRAPE /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of drapes: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NDRAPE,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMPLY /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of plies: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMPLY,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMSTACK /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of stacks: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMSTACK,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRNOD /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of node groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRNOD,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRBRIC /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of brick groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRBRIC,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRQUAD /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of quad groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRQUAD,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRSHEL /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of shell groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRSHEL,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRSH3N /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of 3 node shell groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRSH3N,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRTRUS /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of truss groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRTRUS,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRBEAM /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of beam groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRBEAM,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRSPRI /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of spring groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRSPRI,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NGRPART /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of part groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NGRPART,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NSURF /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of surface groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NSURF,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NSLIN /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of line groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NSLIN,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NSENSOR /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of sensors: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NSENSOR,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMSKW /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of skews: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMSKW,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NUMFRAM /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of frames: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NUMFRAM,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NSUBMOD /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of submodels: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NSUBMOD,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '


       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<li><b>Data history</b>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       IF(NTHGRP /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of th groups: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NTHGRP,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NSECT /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of sections: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NSECT,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NACCELM /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of accelerometers: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NACCELM,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       IF(NBGAUGE /= 0)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of gauges: &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NBGAUGE,'</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')' '


c       WRITE(IOUTREP,'(A)')'<p>'
c       WRITE(IOUTREP,'(A)')'<li><b>Materials</b>'
c       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
c       WRITE(IOUTREP,'(A)')'<tr>'

c       IF(NUMMAT /= 0)THEN
c         DO I=1,NUMMAT
c           MATERIAL(IPM(2,I)) = 1
c
c         ENDDO
c         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of /MAT/LAW1 : &nbsp;&nbsp;</td>'
c         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">               1</td>'
c         WRITE(IOUTREP,'(A)')'</tr>'
c         WRITE(IOUTREP,'(A)')'<tr>'
c         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of /MAT/LAW2 : &nbsp;&nbsp;</td>'
c         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">               1</td>'
c         WRITE(IOUTREP,'(A)')'</tr>'
c       ENDIF

c       WRITE(IOUTREP,'(A)')'</table>'
c       WRITE(IOUTREP,'(A)')' '
c       WRITE(IOUTREP,'(A)')'<p>'

c       WRITE(IOUTREP,'(A)')'<p>'
c       WRITE(IOUTREP,'(A)')'<li><b>Properties</b>'
c       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
c       WRITE(IOUTREP,'(A)')'<tr>'
c       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of /PROP/SHELL : &nbsp;&nbsp;</td>'
c       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">               1</td>'
c       WRITE(IOUTREP,'(A)')'</tr>'
c       WRITE(IOUTREP,'(A)')'<tr>'
c       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Number of /PROP/SOLID : &nbsp;&nbsp;</td>'
c       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">               1</td>'
c       WRITE(IOUTREP,'(A)')'</tr>'
c       WRITE(IOUTREP,'(A)')'</table>'
c       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<p>'


       OUTNAME = ROOTNAM(1:ROOTLEN)//'_0000.out'

       IS_FILE_FOUND = 0

       OPEN (UNIT=OUT_TMP,STATUS='OLD',FORM='FORMATTED',FILE=OUTNAME,ERR=100)
       IS_FILE_FOUND = 1
       IS_WARNING = 0
       IS_MESSAGE = 0
       IEND = 0
       DO WHILE (IS_MESSAGE == 0)
         READ(OUT_TMP,'(A)',END=100)CART
         IF (CART(1:29) == '     TERMINATION WITH WARNING') IS_WARNING = 1
         IF (CART(1:29) == '|            ERROR(S) SUMMARY') IS_MESSAGE = 1
       END DO

       IF(IS_WARNING == 1)THEN
         WRITE(IOUTREP,'(A)')' '
         WRITE(IOUTREP,'(A)')'<li type="square">'
         IF(GOT_INSPIRE_ALM == 1)THEN
           WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Solver : Warning(s) :</b></font>'
         ELSE
           WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Starter : Warning(s) :</b></font>'
         ENDIF
         WRITE(IOUTREP,'(A)')'</li>'
         WRITE(IOUTREP,'(A)')'<p>'
         WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       ENDIF

       READ(OUT_TMP,'(A)',END=100)CART
       DO WHILE (IEND /= 1)
         WRITE(IOUTREP,'(A)')'<tr>'
         IF (CART /= ' ' . AND. 
     .       CART(61:118) /= '---------------------------------------------------------')
     .     WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="1">&nbsp;&nbsp;&nbsp;&nbsp;',CART(63:119),' </td>'
         WRITE(IOUTREP,'(A)')'</tr>'
         READ(OUT_TMP,'(A)',END=100)CART
         IF (CART(1:29) == '-----------------------------')IEND = 1
       END DO
  100  CONTINUE 
       IF ( IS_FILE_FOUND == 0 )THEN
         WRITE(IOUTREP,'(A)')' '
         WRITE(IOUTREP,'(A)')'<li type="square">'
         IF(GOT_INSPIRE_ALM == 1)THEN
           WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Solver : Warning(s) :</b></font>'
         ELSE
           WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Starter : Warning(s) :</b></font>'
         ENDIF
         WRITE(IOUTREP,'(A)')'</li>'
         WRITE(IOUTREP,'(A)')'<p>'
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">No 0.out file found !!! &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">Not able to write information !!! &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A)')'</tr>'

       ENDIF


       IF(NTHGRP /= 0)THEN
         WRITE(IOUTREP,'(A)')'</table>'
       ENDIF

       CLOSE (OUT_TMP)

c       WRITE(IOUTREP,'(A)')' '
c       WRITE(IOUTREP,'(A)')'<li type="square">'
c       WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Engine : Warning(s)/Error(s) :</b></font>'
c       WRITE(IOUTREP,'(A)')'</li>'
c       WRITE(IOUTREP,'(A)')'<p>'

       WRITE(IOUTREP,'(A)')'<li type="square">'
       IF(GOT_INSPIRE_ALM == 1)THEN
         WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Solver : Last cycle information :</b></font>'
       ELSE
         WRITE(IOUTREP,'(A)')'<font face="Arial" size="3"><b>Engine : Last cycle information :</b></font>'
       ENDIF
       WRITE(IOUTREP,'(A)')'</li>'
       WRITE(IOUTREP,'(A)')'<p>'

       IF(N == 2 .OR. N == 4 .OR. N == 5 .OR. N == 20)THEN
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">ERROR TERMINATION &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ELSEIF(N == 3)THEN 
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">USER BREAK &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ELSE
         WRITE(IOUTREP,'(A)')'<tr>'
         WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">NORMAL TERMINATION &nbsp;&nbsp;</td>'
         WRITE(IOUTREP,'(A)')'</tr>'
       ENDIF

       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')'<table cellspacing="0" cellpadding="0">'
       WRITE(IOUTREP,'(A)')'<tr>'
       WRITE(IOUTREP,'(A)')'<td><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'CYCLE &nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'FINAL TIME&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'TIME-STEP&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'ELEMENT&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'%ERROR&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'I-ENERGY&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'K-ENERGY T&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'K-ENERGY R&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'EXT-WORK&nbsp;&nbsp;&nbsp;'
       WRITE(IOUTREP,'(A)')'</td><td align="right"><font face="Arial" size="2">'
       WRITE(IOUTREP,'(A)')'MAS.ERR'
       WRITE(IOUTREP,'(A)')'</td></tr>'
       WRITE(IOUTREP,'(A)')'<tr><td colspan=10>'
       WRITE(IOUTREP,'(A)')'<hr size="1" color="#000000" noshade>'
       WRITE(IOUTREP,'(A)')'</td></tr>'
       WRITE(IOUTREP,'(A)')'<tr align="center">'

       WRITE(IOUTREP,'(A,I10,A)')'<td><font face="Arial" size="2">',NCYCLE,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',TT,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',DT2,'</td>'
       WRITE(IOUTREP,'(2A,I10,A)')'<td><font face="Arial" size="2">',ELTYP(ITYPTS),NELTS,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',ERR1,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',ENINT,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',ENCIN,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',ENROT,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td><font face="Arial" size="2">',TFEXT,'</td>'
       WRITE(IOUTREP,'(A,G11.4,A)')'<td align="right"><font face="Arial" size="2">',EMASS,'</td>'
       WRITE(IOUTREP,'(A)')'</tr>'
       WRITE(IOUTREP,'(A)')'</table>'
       WRITE(IOUTREP,'(A)')'</li>'
       WRITE(IOUTREP,'(A)')'</ul>'
       WRITE(IOUTREP,'(A)')'<p>'

       IF (IH3DREADER /= 0 .OR. IMVW /= 0) THEN

         WRITE(IOUTREP,'(A)')' '
         WRITE(IOUTREP,'(A)')'<font face="Arial" size="4"><b><i>Results summary</i></b></font>'
         WRITE(IOUTREP,'(A)')'<hr size="1" color="#000000" noshade>'
         WRITE(IOUTREP,'(A)')'<p>'
       ENDIF

       IF ( IMVW /= 0) THEN
         WRITE(IOUTREP,'(A)')'<a href="',MVWNAM(1:ROOTLEN+4),'">Open ',MVWNAM(1:ROOTLEN+4),' with Hyperview</a>'
         WRITE(IOUTREP,'(A)')'<p>'
       ENDIF

       IF (IH3DREADER /= 0) THEN
         WRITE(IOUTREP,'(A)')'<OBJECT ID="HVPControl" WIDTH=800 HEIGHT=600'
         WRITE(IOUTREP,'(A)')'classid="clsid:D28D4D39-57F5-4DFB-8832-C8CB1B1DC410">'
         WRITE(IOUTREP,'(3A)')'<PARAM name="src" value="',ROOTNAM(1:ROOTLEN),'.h3d" />'
         IF (IH3DREADER == 1) WRITE(IOUTREP,'(A)')'<PARAM name="inplace" value="1" />'
         IF (IH3DREADER == 2) WRITE(IOUTREP,'(A)')'<PARAM name="inplace" value="0" />'
         WRITE(IOUTREP,'(A)')'<PARAM name="launch_size" value="800x600" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_name0" value="contour dataset" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_val0" value="Displacement" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_name1" value="contour component" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_val1" value="Load Case 1" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_name2" value="simulation" />'
c         WRITE(IOUTREP,'(A)')'<PARAM name="rr_val2" value="1" />'
         WRITE(IOUTREP,'(A)')'<EMBED type="application/x-h3d"'
         WRITE(IOUTREP,'(A)')'pluginspage="http://www.altair.com"'
         WRITE(IOUTREP,'(A)')'width=800 height=600'
         WRITE(IOUTREP,'(A)')'src="',ROOTNAM(1:ROOTLEN),'h3d"'
         IF (IH3DREADER == 1) WRITE(IOUTREP,'(A)')'inplace=1'
         IF (IH3DREADER == 2) WRITE(IOUTREP,'(A)')'inplace=0'
         WRITE(IOUTREP,'(A)')'launch_size="800x600"'
         WRITE(IOUTREP,'(A)')'>'
         WRITE(IOUTREP,'(A)')'</OBJECT>'
         WRITE(IOUTREP,'(A)')'<p>'


c       WRITE(IOUTREP,'(A)')'<font face="Arial" size="2">'
c       WRITE(IOUTREP,'(A)')'<a href="javascript:h3d();">Click here</a> to view the results with HyperViewPlayer
c     . . This feature requires a modern browser with Javascript enabled, as well as the
c     .HyperViewPlayer plugin installed.'
       ENDIF

       IF (IH3DREADER /= 0 .OR. IMVW /= 0) THEN
         WRITE(IOUTREP,'(A)')'</font>'
       ENDIF

       WRITE(IOUTREP,'(A)')'<p>'
       WRITE(IOUTREP,'(A)')' '
       WRITE(IOUTREP,'(A)')'<br>'
       WRITE(IOUTREP,'(A)')'<hr size="1" color="#000000" width="50%" noshade>'
       WRITE(IOUTREP,'(A)')'<center>'
       WRITE(IOUTREP,'(A)')'<font face="Arial" size="1">'
       WRITE(IOUTREP,'(A)')'<b>&#169; 1996-2022 Altair Engineering, Inc.</b>'
       WRITE(IOUTREP,'(A)')'</font> '
       WRITE(IOUTREP,'(A)')'</center> '
       WRITE(IOUTREP,'(A)')'</body>'
       WRITE(IOUTREP,'(A)')'</html>'



       CLOSE(UNIT=IOUTREP)

        
      ENDIF
c-----------
      RETURN
      END
