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
Chd|  QINIT2                        source/elements/solid_2d/quad/qinit2.F
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
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QMASI2                        source/elements/solid_2d/quad/qmasi2.F
Chd|        QMORTH2                       source/elements/solid_2d/quad/qmorth2.F
Chd|        QRCOOR2                       source/elements/solid_2d/quad/qrcoor2.F
Chd|        QVOLI2                        source/elements/solid_2d/quad/qvoli2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE QINIT2(ELBUF_STR   ,MS     ,IXQ       ,PM      ,X      ,
     .                  DETONATORS  ,GEO    ,VEUL      ,ALE_CONNECTIVITY   ,IPARG  ,
     .                  DTELEM      ,SIGI    ,IGEO   ,
     .                  NEL         ,SKEW   ,MSQ       ,IPART   ,IPARTQ ,
     .                  IPM         ,NSIGS  ,
     .                  WMA         ,PTQUAD ,BUFMAT    ,NPF     ,TF     ,
     .                  IPARGG      ,ILOADP ,FACLOAD   ,PARTSAV ,V      )
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), IPARG(*),IGEO(NPROPGI,*),
     .        NEL,IPART(LIPART1,*),IPARTQ(*),IPM(NPROPMI,*), PTQUAD(*),
     .        NSIGS, NPF(*),IPARGG(*)
      my_real
     .   MS(*), PM(NPROPM,*), X(*),  GEO(NPROPG,*),
     .   VEUL(10,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),
     .   MSQ(*), BUFMAT(*), TF(*),WMA(*),PARTSAV(20,*),V(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ)
      INTEGER NF1, IELVS, IX, I, IGTYP, IP, IBID, IPID1
      my_real
     .   E1Y(MVSIZ),E1Z(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   BID(1), DTX(MVSIZ),
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ)
      my_real
     .   FV
      CHARACTER TITR*nchartitle
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_), POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     AIRE(MVSIZ), DELTAX(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF  => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
c
      IGTYP = IPARG(38)
      JCVT  = IPARG(37)
      IBID = 0
      BID = ZERO
      NF1 = NFT+1
c
      IF(JCVT == 0)THEN
        CALL QCOOR2(X,IXQ(1,NF1),NGL,MAT, 
     .        PID, IX1, IX2, IX3, IX4, 
     .        Y1, Y2, Y3, Y4, 
     .        Z1, Z2, Z3, Z4,
     .        SY, SZ, TY, TZ)
      ELSE
        CALL QRCOOR2(X   ,IXQ(1,NF1),NGL  ,MAT    ,
     .         PID, IX1, IX2, IX3, IX4, 
     .         Y1, Y2, Y3, Y4, 
     .         Z1, Z2, Z3, Z4, 
     .         SY, SZ, TY, TZ,
     .         E1Y, E1Z, E2Y, E2Z)
      END IF
      IF (IGTYP == 6) CALL QMORTH2(PID  ,GEO  ,IGEO ,GBUF%GAMA, NEL,
     .                             SY   ,SZ   ,TY   ,TZ ,
     .                             E1Y  ,E1Z  , E2Y, E2Z)
      CALL QVOLI2(GBUF%VOL,IXQ(1,NF1), 
     .     NGL, AIRE, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3 ,Z4)
      CALL QDLEN2(IPARG(63),
     .     AIRE, DELTAX, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4)
      IF(JEUL/=0) CALL EDLEN2(VEUL(1,NF1), AIRE, DELTAX)
C
      IP=0
      CALL MATINI(PM      ,IXQ     ,NIXQ      ,X         ,
     .            GEO     ,ALE_CONNECTIVITY   ,DETONATORS,IPARG     ,
     .            SIGI    ,NEL     ,SKEW      ,IGEO      ,
     .            IPART   ,IPARTQ  ,
     .            MAT     ,IPM     ,NSIGS     ,NUMQUAD   ,PTQUAD  ,
     .            IP      ,NGL     ,NPF       ,TF        ,BUFMAT  ,
     .            GBUF    ,LBUF    ,MBUF      ,ELBUF_STR ,ILOADP  ,
     .            FACLOAD, DELTAX)
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE ET TURBULENCE
C----------------------------------------
      IF (JTHE/=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
      IF (JTUR/=0) CALL ATURI2(IPARGG ,GBUF%RHO,PM,IXQ,X,
     .                        GBUF%RK,GBUF%RE, AIRE)
C------------------------------------------
C     INITIALISATION DE LA MATRICE DE MASSE
C------------------------------------------
      IF (JLAG+JALE+JEUL/=0)
     .  CALL QMASI2(PM,MAT,MS,GBUF%VOL,MSQ(NF1),WMA,IPARTQ(NFT+1),PARTSAV,
     .     IX1, IX2, IX3, IX4 ,X ,V)
C-------------------------------------------
C      CALCUL DES DT ELEMENTAIRES
C-------------------------------------------
      CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,GBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     GBUF%VOL, DTX, IGEO)
c
       DO I=LFT,LLT
        IF(IXQ(6,I+NFT)/=0) THEN
          IF (IGTYP/=0 .AND. IGTYP/=6 .AND.
     .        IGTYP/=14.AND.IGTYP/=15)THEN
             IPID1=IXQ(NIXQ-1,I+NFT)
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             CALL ANCMSG(MSGID=226,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,IPID1),
     .                   C1=TITR,
     .                   I2=IGTYP)
          ENDIF
        ENDIF
        DTELEM(NFT+I)=DTX(I)
      ENDDO
C-----------
      RETURN
      END
