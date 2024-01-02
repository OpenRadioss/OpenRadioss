Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREDEC0                       source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECIG3D(ITAB    ,IPART   ,IPARTIG3D  ,IPM     ,IGEO    ,
     .                   KXIG3D  ,IXIG3D  ,ITABM1  ,NCTRLMAX, TABCONPATCH)
      USE MESSAGE_MOD
      USE MESHSURFIG3D_MOD
C----------------------------------------------------------
C     LECTURE ELEMENT ISO GEOMETRIQUE
C-----------------------------------------------
C     KXIG3D(1,*):IMID :   ID DU MATERIAU
C     KXIG3D(2,*):IPID :   ID DE LA PROPRIETE
C     KXIG3D(3,*):NNOD :   NOMBRE DE POINTS DE CONTROLE DE L ELEMENT
C     KXIG3D(4,*):IAD  :   ADRESSE DE LA ZONE DES NOS DE NOEUDS DANS IXIG3D
C                                     IXIG3D(IAD) A IXIG3D(IAD+NNOD-1)
C     KXIG3D(5,*):ID   :   ID DE L'ELEMENT.
C     KXIG3D(6,*):ID   :   index of 1st knot in the Xknot vector corresponding to the element
C     KXIG3D(7,*):ID   :   index of 1st knot in the Yknot vector corresponding to the element
C     KXIG3D(8,*):ID   :   index of 1st knot in the Zknot vector corresponding to the element
C     KXIG3D(9,*):ID   :   index of 2nd knot in the Xknot vector corresponding to the element
C     KXIG3D(10,*):ID  :   index of 2nd knot in the Yknot vector corresponding to the element
C     KXIG3D(11,*):ID  :   index of 2nd knot in the Zknot vector corresponding to the element
C     KXIG3D(12,*):    :
C     KXIG3D(13,*):    :   
C     KXIG3D(14,*):    :   
C     KXIG3D(15,*):ID  :   ID OF THE FIRST NODE FOR ANIMATION FILE (27 BRICKS)   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),IXIG3D(*),ITAB(*), 
     .        IPART(LIPART1,*),IPARTIG3D(*),
     .        IPM(NPROPMI,*),IGEO(NPROPGI,*),ITABM1(*),
     .        NCTRLMAX
      TYPE(TABCONPATCH_IG3D_), DIMENSION(*) :: TABCONPATCH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,ID,IDS,IDEX,IDGU,IGS,NNOD,IAD,
     .        I1, I2,MID,PID,IDX1,IDY1,IDZ1,NCTRL,NBLINE,
     .        NRAFX,NRAFY,NRAFZ,NBIG3D_PATCH
      INTEGER TABIDS(NUMELIG3D0),J10(10)
      CHARACTER MESS*40
      my_real
     .   BID 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5,USR2SYS
C-----------------------------------------------
      DATA MESS /'ISO-GEOMETRIC ELEMENTS DEFINITION       '/
C=======================================================================
c
        NBIG3D_PATCH = 0
        NBPART_IG3D = 0
c
        KCUR = KIGE3D
        NBPART_IG3D = NBPART_IG3D+1
        TABCONPATCH(NBPART_IG3D)%ID_TABCON=NBPART_IG3D
        IREC = KOPTAD(KCUR)
        IREC=IREC+1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        DO WHILE( LINE(1:1) /= '/' .OR. LINE(1:6) == '/IGE3D')
        
         IF (LINE(1:1) == '/')THEN  ! CHANGEMENT DE PART
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         ENDIF
       
         READ(LINE,ERR=999,FMT=FMT_8I)
     .   ID,IDX1,IDY1,IDZ1,NCTRL,NRAFX,NRAFY,NRAFZ
         NBIG3D_PATCH=NBIG3D_PATCH+1
         IREC = IREC + ((NCTRL-1)/10)+2
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         
         IF (LINE(1:6) == '/IGE3D')THEN  !  ON A ONE CHANGEMENT DE PART
           TABCONPATCH(NBPART_IG3D)%L_TAB_IG3D=NBIG3D_PATCH
           ALLOCATE(TABCONPATCH(NBPART_IG3D)%TAB_IG3D(NBIG3D_PATCH))
           NBPART_IG3D = NBPART_IG3D+1
           NBIG3D_PATCH=0
           IREC=IREC+1
           READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         ENDIF

        ENDDO
c
        TABCONPATCH(NBPART_IG3D)%L_TAB_IG3D=NBIG3D_PATCH
        ALLOCATE(TABCONPATCH(NBPART_IG3D)%TAB_IG3D(NBIG3D_PATCH))
c
        NBPART_IG3D = 0
        NBIG3D_PATCH = 0
c
        BID =0
        IAD =1
        KCUR=KIGE3D
        IREC=KOPTAD(KCUR)-1 
        I = 0
        INOD_IGE = FIRSTNOD_ISOGEO
        DO WHILE( I < NUMELIG3D0 )              
         IREC=IREC+1                        
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         IF (LINE(1:1) == '/')THEN
          NBPART_IG3D = NBPART_IG3D+1
          NBIG3D_PATCH = 0
          KLINE=LINE                                     
          CALL FREDEC0(ID)
          IDS=0
          DO J=1,NPART
            IF(IPART(4,J) == ID)IDS=J
          ENDDO
          IF(IDS == 0) THEN
            CALL ANCMSG(MSGID=402,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="IGE3D",
     .                  I1=ID,
     .                  I2=ID,
     .                  PRMOD=MSG_CUMU)
          ENDIF
          TABCONPATCH(NBPART_IG3D)%PID=IDS
         ELSE
          I = I + 1
          KXIG3D(1,I) =IPART(1,IDS)
          KXIG3D(2,I) =IPART(2,IDS)
          KXIG3D(4,I) =IAD
          IPARTIG3D(I)=IDS
C
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          READ(LINE,ERR=999,FMT=FMT_8I) ID,IDX1,IDY1,IDZ1,NCTRL,NRAFX,NRAFY,NRAFZ
          NBIG3D_PATCH = NBIG3D_PATCH + 1
          TABCONPATCH(NBPART_IG3D)%TAB_IG3D(NBIG3D_PATCH)=I ! ID
          NCTRLMAX = MAX(NCTRLMAX,NCTRL)
          KXIG3D(3,I)=NCTRL
          KXIG3D(5,I)=ID
          KXIG3D(6,I)=IDX1
          KXIG3D(7,I)=IDY1
          KXIG3D(8,I)=IDZ1
c          KXIG3D(9,I)=IDX1+1
c          KXIG3D(10,I)=IDY1+1
c          KXIG3D(11,I)=IDZ1+1
          KXIG3D(12,I)=MAX(NRAFX,1)
          KXIG3D(13,I)=MAX(NRAFY,1)
          KXIG3D(14,I)=MAX(NRAFZ,1)
          KXIG3D(15,I)=INOD_IGE
          INOD_IGE = INOD_IGE + 64
C                  
          NBLINE= ((NCTRL-1)/10)+1

          DO N=1,NBLINE
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_10I) J10
            DO J=1,10
             IF(J10(J) /= 0)THEN
               IXIG3D(IAD)=USR2SYS(J10(J),ITABM1,MESS,ID)
               IAD=IAD+1
             ENDIF
            ENDDO
          ENDDO
         ENDIF
        ENDDO
C-----------
      CALL ANCMSG(MSGID=402,                 
     .            MSGTYPE=MSGERROR,         
     .            ANMODE=ANINFO_BLIND_1,    
     .            PRMOD=MSG_PRINT) 
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      DO I=1,NUMELIG3D0
         TABIDS(I)= KXIG3D(5,I)
      ENDDO
      CALL UDOUBLE(TABIDS,1,NUMELIG3D0,MESS,0,BID)
C-------------------------------------
C Print
C-------------------------------------
      I1=1
      I2=MIN0(50,NUMELIG3D0)
C
   90 WRITE (IOUT,300)
      DO 100 I=I1,I2
       MID=IPM(1,KXIG3D(1,I))
       PID=IGEO(1,KXIG3D(2,I))
       WRITE (IOUT,'(4(I10,1X))') I,KXIG3D(5,I),MID,PID
       WRITE (IOUT,'(10(I10,1X))')
     .    (ITAB(IXIG3D(IAD)),IAD=KXIG3D(4,I),KXIG3D(4,I)+KXIG3D(3,I)-1)
c      call flush(IOUT)
  100 CONTINUE
      IF(I2==NUMELIG3D0)GOTO 200
      I1=I1+50
      I2=MIN0(I2+50,NUMELIG3D0)
      GOTO 90
C
 200  CONTINUE
C
 300  FORMAT(/' ISO-GEOMETRIC ELEMENTS'/
     +        ' ----------------------'/
     + '    LOC-EL     GLO-EL      MATER       GEOM'/
     + '    NODES LIST')
      RETURN
C-------------------------------------
  999 CALL FREERR(3)
      RETURN
      END
