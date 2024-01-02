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
Chd|  Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        ATURI2                        source/ale/ale2d/aturi2.F     
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        EDLEN2                        source/ale/ale2d/edlen2.F     
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|        Q4RCOOR2                      source/elements/solid_2d/quad4/q4coor2.F
Chd|        Q4VOLI2                       source/elements/solid_2d/quad4/q4voli2.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QMASI2                        source/elements/solid_2d/quad/qmasi2.F
Chd|        QMORTH2                       source/elements/solid_2d/quad/qmorth2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE Q4INIT2(ELBUF_STR  ,MS       ,IXQ          ,PM         ,X     ,
     2                  DETONATORS  ,GEO      ,VEUL         ,ALE_CONNECTIVITY  ,IPARG ,
     3                  DTELEM      ,SIGI       ,IGEO  ,
     4                  NEL         ,SKEW     ,MSQ          ,IPART      ,IPARTQ,
     5                  IPM         ,NSIGS    ,WMA          ,PTQUAD     ,BUFMAT,
     6                  NPF         ,TF       ,IPARGG       ,ILOADP     ,FACLOAD,
     7                  PARTSAV     ,V        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "scry_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), IPARG(*),IGEO(NPROPGI,*),
     .        NEL,IPART(LIPART1,*),IPARTQ(*),IPM(NPROPMI,*), PTQUAD(*),
     .        NSIGS, NPF(*),IPARGG(*)
      my_real
     .   MS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(10,*),  DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),
     .   MSQ(*), BUFMAT(*), TF(*),WMA(*),PARTSAV(20,*),V(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C FUNCTION:
C ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
C TYPE NAME                FUNCTION
C  I   IXQ(NIXQ,*)        - ELEMENT "MID", CONNECTIVITY, "PID", "SN"
C  I   PM(NPROPM,*)       - MATERIAL DATA (REAL)
C  I   X(3,*)             - NODAL COORDINATES
C  I   GEO(NPROPG,*)      - GEOMETRICAL PROPERTY DATA (REAL)
C  I   IPARG(*)           - PART PROPERTY DATA OF ELEMENT GROUP
C  O   DTELEM(*)          - ELEMENT TIME STEP
C  I   SIGI(NSIGS,*)      - (1~6,*): INITIAL STRESS
C                           (7~10,*): NUMBER, DENSITY, PLASTIC STRAIN, INTERNAL ENERGY
C  I   IGEO(NPROPGI,*)    - GEOMETRICAL PROPERTY DATA (INTEGER)
C  I   NEL                - ELEMENT NUMBER IN THIS GROUP
C  I   SKEW(LSKEW,*)          - ELEMENT SKEW
C  O   MSQ(*)             - ONE FOURTH OF ELEMENT MASS
C  I   IPART(LIPART1,*)        - PART PROPERTY DATA (USED FOR SPH CASE)
C  I   IPARTQ(*)          - ID OF PART THAT ELEMENT BELONGS TO (USED FOR SPH CASE)
C  I   IPM(NPROPMI,*)     - MATERIAL DATA (INTEGER)
C  I   NSIGS              - NUMBER OF DATA IN "SIGI"
C  I   PTQUAD(*)          - POINTER OF ELEMENT ADRESS IN "SIGI"
C  I   NPF(*),TF(*)       - Radioss function (x=Time) data
C  I   IPARGG(*)          - PART PROPERTY DATA OF ELEMENT GROUP (USED FOR ALE CASE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ)
      INTEGER NF1, IELVS, IX, I, IGTYP, IHBE, IP
      INTEGER IR,IS,NPTR,NPTS,IBID, IPID1
      my_real
     +   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     +   Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     +   Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ),
     +   Y12(MVSIZ),Y34(MVSIZ),Y13(MVSIZ),Y24(MVSIZ),
     +   Y14(MVSIZ),Y23(MVSIZ),
     +   Z12(MVSIZ),Z34(MVSIZ),Z13(MVSIZ),Z24(MVSIZ),
     +   Z14(MVSIZ),Z23(MVSIZ),YAVG(MVSIZ),AREA(MVSIZ),
     +   VOLU(MVSIZ),DET(MVSIZ),BID(1),AA(MVSIZ), DTX(MVSIZ),
     +   SY(MVSIZ) ,SZ(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ),
     .   E1Y(MVSIZ),E1Z(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ)
      my_real
     +   QN1,QN2,QN3,QN4,YAVG1,WI,KSI,ETA,FV
      my_real
     .     DELTAX(MVSIZ),Y234(MVSIZ),Y124(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)

C-----------------------------------------------
      CHARACTER*nchartitle,
     .   TITR1
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
c
      IGTYP = IPARG(38)
      IHBE = IPARG(23)
      JCVT  = IPARG(37)
!
      ISORTH = 0
      IBID = 0
      BID = ZERO
C
      NF1 = NFT+1
      IF(JCVT==0)THEN
        CALL QCOOR2(X,IXQ(1,NF1),NGL,MAT, 
     .        PID, IX1, IX2, IX3, IX4, 
     .        Y1, Y2, Y3, Y4,
     .        Z1, Z2, Z3, Z4,
     .        SY, SZ, TY, TZ)
          DO I=LFT,LLT
            YAVG(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
          ENDDO
      ELSE
        CALL Q4RCOOR2(X,IXQ(1,NF1),NGL,MAT,
     .        PID, IX1, IX2, IX3, IX4,
     .        Y1, Y2, Y3, Y4,
     .        Z1, Z2, Z3, Z4,YAVG,Y234,Y124,
     .        SY,SZ,TY,TZ, 
     .        E1Y, E1Z, E2Y, E2Z)
      ENDIF
C
      IF (IGTYP == 6) CALL QMORTH2(PID  ,GEO  ,IGEO ,GBUF%GAMA, NEL,
     .                   SY   ,SZ   ,TY   ,TZ  ,
     .                   E1Y  ,E1Z  , E2Y, E2Z) 
C
      CALL Q4VOLI2(GBUF%VOL,IXQ(1,NF1),
     .     NGL, AREA, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4,Y234,Y124)
      CALL QDLEN2(IPARG(63), 
     .     AREA, DELTAX, 
     .     Y1, Y2, Y3, Y4,
     .     Z1, Z2, Z3, Z4)
      IF(JEUL/=0) CALL EDLEN2(VEUL(1,NF1), AREA, DELTAX)
      DO I=LFT,LLT
        Y12(I) = Y1(I) - Y2(I)
        Y34(I) = Y3(I) - Y4(I)
        Y13(I) = Y1(I) - Y3(I)
        Y24(I) = Y2(I) - Y4(I)
        Y14(I) = Y1(I) - Y4(I)
        Y23(I) = Y2(I) - Y3(I)
        Z12(I) = Z1(I) - Z2(I)
        Z34(I) = Z3(I) - Z4(I)
        Z13(I) = Z1(I) - Z3(I)
        Z24(I) = Z2(I) - Z4(I)
        Z14(I) = Z1(I) - Z4(I)
        Z23(I) = Z2(I) - Z3(I)
      ENDDO
C
C
      IP=0
      CALL MATINI(PM        ,IXQ      ,NIXQ       ,X            ,
     .            GEO       ,ALE_CONNECTIVITY    ,DETONATORS ,IPARG        ,
     .            SIGI      ,NEL      ,SKEW       ,IGEO         ,
     .            IPART     ,IPARTQ   ,
     .            MAT       ,IPM      ,NSIGS      ,NUMQUAD      ,PTQUAD  ,
     .            IP        ,NGL      ,NPF        ,TF           ,BUFMAT  ,
     .            GBUF      ,LBUF     ,MBUF       ,ELBUF_STR    ,ILOADP  ,
     .            FACLOAD   ,DELTAX)

C     ENTER THE INTEGRATION POINTS LOOP -->
      NPTR = 2
      NPTS = 2
      DO IR=1,NPTR
        DO IS=1,NPTS
c
          LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
C
      IP = IR + (IS-1)*NPTR
      KSI = A_GAUSS(IR,NPTR)
      ETA = A_GAUSS(IS,NPTS)
      WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)
C
           CALL Q4DERI2(LBUF%VOL,KSI,ETA,WI,
     2             Y12,Y34,Y13,Y24,Y14,Y23,
     3             Z12,Z34,Z13,Z24,Z14,Z23,
     4             Y1,Y2,Y3,Y4,YAVG,IHBE,NGL)
C
Ccw<<<
c      DO I=LFT,LLT
c        AA(I) = AA(I) + EV(NB6+I-1)
c      ENDDO
Ccw>>>
C
          CALL MATINI(
     .            PM      ,IXQ     ,NIXQ       ,X            ,
     .            GEO     ,ALE_CONNECTIVITY   ,DETONATORS ,IPARG        ,
     .            SIGI    ,NEL     ,SKEW       ,IGEO         ,
     .            IPART   ,IPARTQ  ,
     .            MAT     ,IPM     ,NSIGS      ,NUMQUAD      ,PTQUAD  ,
     .            IP      ,NGL     ,NPF        ,TF           ,BUFMAT  ,
     .            GBUF    ,LBUF    ,MBUF       ,ELBUF_STR    ,ILOADP  ,
     .            FACLOAD, DELTAX)  
C
        ENDDO
      ENDDO
C     EXIT THE INTEGRATION POINTS LOOP <--
C
Ccw<<<
c      DO I=LFT,LLT
c        WRITE(*, *) EV(NB06+I-1),AA(I)
c      ENDDO
Ccw>>>
C
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE ET TURBULENCE
C----------------------------------------
      IF(JTHE/=0)CALL ATHERI(MAT ,PM  ,LBUF%TEMP)
      IF(JTUR/=0)CALL ATURI2(IPARGG   ,LBUF%RHO,PM,IXQ,X,
     .                         LBUF%RK  ,LBUF%RE, AREA)
C------------------------------------------
C     INITIALISATION DE LA MATRICE DE MASSE
C------------------------------------------
      IF(JLAG+JALE+JEUL/=0)
     .  CALL QMASI2(PM,MAT,MS,GBUF%VOL,MSQ(NF1),WMA,IPARTQ(NFT+1),PARTSAV,
     .     IX1, IX2, IX3, IX4,X ,V)
C-------------------------------------------
C      CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
       CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,GBUF%RE ,BUFMAT, DELTAX, AREA, 
     .     GBUF%VOL, DTX, IGEO,IGTYP)
       DO 10 I=LFT,LLT
        IF(IXQ(6,I+NFT)/=0) THEN
          IF(IGTYP/=0 .AND. IGTYP/=6 .AND.
     .       IGTYP/=14.AND.IGTYP/=15)THEN
             IPID1=IXQ(NIXQ-1,I+NFT)
             CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             CALL ANCMSG(MSGID=226,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,IPID1),
     .                   C1=TITR1,
     .                   I2=IGTYP)
          ENDIF
        ENDIF
       DTELEM(NFT+I)=DTX(I)
  10   CONTINUE
C
      RETURN
      END
