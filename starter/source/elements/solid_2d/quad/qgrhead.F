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
Chd|  QGRHEAD                       source/elements/solid_2d/quad/qgrhead.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE QGRHEAD(
     1       IXQ     ,PM      ,GEO     ,INUM    ,ISEL    ,
     2       ITR1    ,EADD    ,INDEX   ,ITRI    ,IPARTQ  ,     
     3       ND      ,IGRSURF ,IGRQUAD ,CEP     ,
     5       XEP     ,IGEO    ,IPM     ,IQUAOFF )     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXQ(7,NUMELQ)     TABLEAU CONECS+PID+MID+NOS SOLIDES 4N      E
C     PM(NPROPM,NUMMAT) TABLEAU DES CARACS DES MATERIAUX           E
C     GEO(NPROPG,NUMGEO)TABLEAU DES CARACS DES PID                 E
C     INUM(8,NUMELQ)    TABLEAU DE TRAVAIL                         E/S
C     ISEL(NSELQ)       TABLEAU DES SOLIDES 4N CHOISIS POUR TH     E/S
C     ITR1(NSELQ)       TABLEAU DE TRAVAIL                         E/S
C     EADD(NUMELQ)      TABLEAU DES ADRESEES DANS IDAM CHGT DAMIER   S
C     INDEX(NUMELQ)     TABLEAU DE TRAVAIL                         E/S
C     ITRI(5,NUMELQ)    TABLEAU DE TRAVAIL                         E/S
C     IPARTQ(NUMELQ)    TABLEAU PART                               E/S
C     CEP(NUMELQ)       TABLEAU DE TRAVAIL                         E/S
C     XEP(NUMELQ)       TABLEAU DE TRAVAIL                         E/S
C     IQUAOFF(NUMELQ)   FLAG ELEM RBY ON/OFF                       E/S
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr21_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXQ(7,*),ISEL(*),INUM(9,*),IPARTQ(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(5,*),ND, CEP(*),XEP(*),
     .        IGEO(NPROPGI,NUMGEO), IPM(NPROPMI,NUMMAT),
     .        IQUAOFF(*)
      my_real ::  PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRQUAD)  :: IGRQUAD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .        I,J,K,L, NG, NN, N, MLN, MID, PID ,
     .        II,JJ, II1,JJ1,II2,JJ2,II3,JJ3,II4,JJ4,
     .        MODE, ML1, ML2, MT1, MT2,IGT, 
     .        MSKMLN,MSKJAL,MSKMID,MSKPID,IEOS,
     .        MSKJEU,MSKJTU,MSKJTH,MSKJPO,
     .        IPLAST, IALEL, MT,IREP,IINT,ISSN,NGTVX,IFAIL,IRB,
     .        JALE_FROM_MAT,JALE_FROM_PROP
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER WORK(70000)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
C
      DATA MSKMLN /O'07770000000'/
      DATA MSKJAL /O'00000070000'/
      DATA MSKJEU /O'00000007000'/
      DATA MSKJTU /O'00000000700'/
      DATA MSKJTH /O'00000000070'/
      DATA MSKJPO /O'00000000007'/
      DATA MSKMID /O'07777777777'/
      DATA MSKPID /O'07777777777'/
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 
C----------------------------------------------------------
C   TRI GLOBAL SUR TOUS LES CRITERES POUR TOUS LES ELEMENTS
C----------------------------------------------------------
C
      DO I=1,NUMELQ
        EADD(I)=1
        ITRI(4,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTQ(I)
        INUM(2,I)=IXQ(1,I)
        INUM(3,I)=IXQ(2,I)
        INUM(4,I)=IXQ(3,I)
        INUM(5,I)=IXQ(4,I)
        INUM(6,I)=IXQ(5,I)
        INUM(7,I)=IXQ(6,I)
        INUM(8,I)=IXQ(7,I)
        INUM(9,I)=IQUAOFF(I)
      ENDDO
C
      DO I=1,NUMELQ
        XEP(I)=CEP(I)
      ENDDO
C
C
      DO I = 1, NUMELQ
        II = I
        NPT=1
        JPOR=0
        MID= IXQ(1,II)
        PID= IXQ(6,II)
        IPLAST=IPLA_DS-1
        IREP  = 0
        JCVT  = 0
        IFAIL = 0
        IEOS  = 0
        IF (PID/=0) THEN
          IGT  = IGEO(11,PID)
          IF (IGT /= 15) IPLAST = IGEO(9,PID)
          IF (IGT==15)JPOR=2*NINT(GEO(28,PID))
          JCVT = IGEO(16,PID)
        ENDIF
        MLN = NINT(PM(19,ABS(MID)))
        IF(MID<0)THEN
          IF(MLN==6.AND.JPOR/=2)MLN=17
          IF(MLN==46)MLN=47
          MID=IABS(MID)
        ENDIF
        IFAIL = IPM(220,MID)       
        JALE_FROM_MAT = NINT(PM(72,MID))
        JALE_FROM_PROP = IGEO(62,PID)
        JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
        JLAG=0
        IF(JALE==0.AND.MLN/=18)JLAG=1
        JEUL=0
        IF(JALE==2)THEN
          JALE=0
          JEUL=1
        ENDIF
        JTUR=NINT(PM(70,MID))
        JTHE=NINT(PM(71,MID))
        JMULT=0
        IF(MLN==20)THEN
          JMULT=NINT(PM(20,MID))
          MT1=NINT(PM(21,MID))
          MT2=NINT(PM(22,MID))
          ML1=NINT(PM(19,MT1))
          ML2=NINT(PM(19,MT2))
        ELSE
          JMULT=0
          ML1=0
          ML2=0 
        ENDIF
C
        IF(JCVT/=0.AND.(JLAG==0.OR.MLN==20))THEN
          ID=IGEO(1,PID)
          CALL FRETITL2(TITR,
     .                  IGEO(NPROPGI-LTITR+1,PID),LTITR)
          CALL ANCMSG(MSGID=608,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IXQ(7,I))
          JCVT=0
        END IF
        IEOS = IPM(4,MID)
C tri sur elem delete des rigidbody
C IRB = 0 : elem actif
C IRB = 1 : elem inactif et optimise pour en SPMD
C IRB = 2 : elem inactif mais optimise pour etre actif en SPMD
        IRB = IQUAOFF(I)       
C
C NPT = 1 ; JHBE = 0 ; JIVF = 0 et JLAG non utilise
Clef 1---------------------------------
        JPOR=MY_SHIFTL(JPOR,0)
        JTHE=MY_SHIFTL(JTHE,3)
        JTUR=MY_SHIFTL(JTUR,6)
        JEUL=MY_SHIFTL(JEUL,9)
        JALE=MY_SHIFTL(JALE,12)
        MLN=MY_SHIFTL(MLN,21)
c        IRB=MY_SHIFTL(IRB,0)
        ITRI(1,I)=MLN+JALE+JEUL+JTUR+JTHE+JPOR+IRB 
Clef 2---------------------------------
        IPLAST=MY_SHIFTL(IPLAST,0)
        IFAIL = MY_SHIFTL(IFAIL,3)
        ML1=MY_SHIFTL(ML1,5)
        ML2=MY_SHIFTL(ML2,13)
        IGT=MY_SHIFTL(IGT,21)
        JCVT=MY_SHIFTL(JCVT,28)
        ITRI(2,I)=IPLAST+ML1+ML2+IGT+JCVT + IFAIL
Clef 3---------------------------------
        ITRI(3,I)=MID
Clef 4---------------------------------
        ITRI(4,I)=PID
Clef 5---------------------------------
        IEOS = MY_SHIFTL(IEOS,0) 
C       next = MY_SHIFTL(next,4) 
        ITRI(5,I)=IEOS

      ENDDO
C
      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELQ , 5)
C
      DO I=1,NUMELQ
        IPARTQ(I) =INUM(1,INDEX(I))
        IQUAOFF(I) = INUM(9,INDEX(I))
      ENDDO
      IF (IMACH==3) THEN
        DO I=1,NUMELQ
          CEP(I)=XEP(INDEX(I))
        ENDDO
      ENDIF
      DO  K=1,7
        DO  I=1,NUMELQ
          IXQ(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO

C
C INVERSION DE INDEX (DANS ITR1)
C
      DO I=1,NUMELQ
        ITR1(INDEX(I))=I
      ENDDO


C
C RENUMEROTATION POUR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF(IGRSURF(I)%ELTYP(J) == 2)
     .       IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMEROTATION POUR GROUPES DE SOLIDES
C
      DO I=1,NGRQUAD
        NN=IGRQUAD(I)%NENTITY
        DO J=1,NN
          IGRQUAD(I)%ENTITY(J) = ITR1(IGRQUAD(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELQ
        II=ITRI(1,INDEX(I))
        JJ=ITRI(1,INDEX(I-1))
        II1=ITRI(2,INDEX(I))
        JJ1=ITRI(2,INDEX(I-1))
        II2=ITRI(3,INDEX(I))
        JJ2=ITRI(3,INDEX(I-1))
        II3=ITRI(4,INDEX(I))
        JJ3=ITRI(4,INDEX(I-1))
        II4=ITRI(5,INDEX(I))
        JJ4=ITRI(5,INDEX(I-1))
        IF(II/=JJ.OR.
     .     II1/=JJ1.OR.
     .     II4/=JJ4.OR.
     .     II2/=JJ2.OR.
     .     II3/=JJ3) THEN
              ND=ND+1
              EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELQ+1
C
      RETURN
      END
