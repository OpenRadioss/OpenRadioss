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
Chd|  FVVENT0                       source/airbag/fvvent0.F
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag1.F
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F
Chd|        PORFORM4                      source/airbag/porfor4.F
Chd|        PORFORM5                      source/airbag/porfor5.F
Chd|        PORFORM6                      source/airbag/porfor6.F
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVVENT0(
     1                 ELSOUT  ,AOUTOT  ,NVENT    ,NELT    ,ITTF    ,
     2                 ELAREA  ,ELSINI  ,ELEM     ,ITAGEL  ,SVENT   ,
     3                 IBAGHOL ,RVOLU   ,RBAGHOL  ,PORO    ,P       ,
     4                 ELTG    ,IPARG   ,MATTG    ,NEL     ,POROSITY,
     5                 IPM     ,PM      ,ELBUF_TAB,IGROUPC ,IGROUPTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE ELBUFDEF_MOD
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NVENT, NELT, NEL, ITTF, ELEM(3,*), IBAGHOL(NIBHOL,*), ITAGEL(*),
     .          ELTG(*), IPARG(NPARG,*), IPM(NPROPMI,*),
     .          MATTG(*), IGROUPC(*),  IGROUPTG(*)

C     REAL
        my_real
     .     AOUTOT,
     .     ELSOUT(*), ELAREA(*), ELSINI(*), RVOLU(*), SVENT(NVENT),
     .     RBAGHOL(NRBHOL,*), PORO(*), P(*), PM(NPROPM,*), POROSITY(*)
        TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IEL, K, N1, N2, N3,
     .     IDEF,    IVENT,   IVENTYP,
     .     IPORT,   IPORP,   IPORA,  IPORT1, IPORP1, IPORA1,
     .     NG,      IM,      IFVENT,  NFUNC,    MTN,
     .     ILEAKAGE,IBLOCKAGE
        INTEGER JEL, NFT, NELG
C
C     REAL
        my_real
     .     PEXT,  AVENT, BVENT, AOUT,   AOUT1,
     .     FPORT, FPORP, FPORA, FPORT1, FPORP1, FPORA1, DERI,
     .     SCALT, SCALP, SCALS, AREA,   AINI,   EXTEN,
     .     TTF,   FLC,   FAC,   FAC1,   SVTFAC, PMEAN,  TT1,
     .     TVENT
        my_real GET_U_FUNC
        EXTERNAL GET_U_FUNC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
        PEXT =RVOLU(3)
        SCALT=RVOLU(26)
        SCALP=RVOLU(27)
        SCALS=RVOLU(28)
        TTF  =RVOLU(60)
C
        DO IVENT=1,NVENT
          SVENT(IVENT)=ZERO
        ENDDO
C
        DO IEL=1,NELT
          IF (ITAGEL(IEL)<0) THEN
            IVENT=-ITAGEL(IEL)
            SVENT(IVENT)=SVENT(IVENT)+ELAREA(IEL)
          ENDIF
        ENDDO
C
        DO IVENT=1,NVENT
          RBAGHOL(16,IVENT)=ZERO
          RBAGHOL(17,IVENT)=ZERO
          RBAGHOL(18,IVENT)=ZERO
          RBAGHOL(19,IVENT)=ZERO
          RBAGHOL(20,IVENT)=ZERO
          RBAGHOL(21,IVENT)=ZERO
          RBAGHOL(22,IVENT)=ZERO
        ENDDO
C------------------------------------------
C  COMPUTE EFFECTIVE VENTING SURFACE ELSOUT
C------------------------------------------
        AOUTOT=ZERO
        DO IEL=1,NELT
          ELSOUT(IEL)=ZERO
          IF (ITAGEL(IEL)<0) THEN
            AREA=ELAREA(IEL)
            IVENT=-ITAGEL(IEL)
            IVENTYP=IBAGHOL(13,IVENT)
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            PMEAN=THIRD*(P(N1)+P(N2)+P(N3))
C-------------
C  VENT HOLES
C-------------
            IF(IVENTYP==0)THEN
C
              AINI=ELSINI(IEL)
              EXTEN=AREA/AINI
C
              IDEF=IBAGHOL(1,IVENT)
              IF (IDEF==0.OR.IDEF==2) CYCLE
              AVENT =RBAGHOL(2,IVENT)
              BVENT =RBAGHOL(6,IVENT)
              TVENT =RBAGHOL(3,IVENT)
C
              IPORT =IBAGHOL(3,IVENT)
              IPORP =IBAGHOL(4,IVENT)
              IPORA =IBAGHOL(5,IVENT)
              IPORT1=IBAGHOL(6,IVENT)
              IPORP1=IBAGHOL(7,IVENT)
              IPORA1=IBAGHOL(8,IVENT)
C
              FPORT =RBAGHOL(7,IVENT)
              FPORP =RBAGHOL(8,IVENT)
              FPORA =RBAGHOL(9,IVENT)
              FPORT1=RBAGHOL(10,IVENT)
              FPORP1=RBAGHOL(11,IVENT)
              FPORA1=RBAGHOL(12,IVENT)
C
              AOUT=AVENT*AREA*(ONE-PORO(IEL))
              AOUT1=BVENT*AREA*PORO(IEL)
              TT1=TT-TTF
              IF (ITTF==13) TT1=TT-TTF-TVENT
              IF (IPORA/=0)
     .           AOUT=AOUT*FPORA
     .                    *GET_U_FUNC(IPORA,EXTEN,DERI)
              IF (IPORT/=0)
     .           AOUT=AOUT*FPORT
     .                    *GET_U_FUNC(IPORT,TT1*SCALT,DERI)
              IF (IPORP/=0)
     .           AOUT=AOUT*FPORP
     .                    *GET_U_FUNC(IPORP,(PMEAN-PEXT)*SCALP,DERI)
C
              IF (IPORA1/=0)
     .           AOUT1=AOUT1*FPORA1
     .                      *GET_U_FUNC(IPORA1,EXTEN,DERI)
              IF (IPORT1/=0)
     .           AOUT1=AOUT1*FPORT1
     .                      *GET_U_FUNC(IPORT1,TT1*SCALT,DERI)
              IF (IPORP1/=0)
     .           AOUT1=AOUT1*FPORP1
     .                      *GET_U_FUNC(IPORP1,(PMEAN-PEXT)*SCALP,DERI)
C------------------
C POROSITY
C------------------
            ELSE
              IBLOCKAGE=IBAGHOL(14,IVENT)
              TT1=TT-TTF
              IF (ITTF==13) THEN
                TVENT=RBAGHOL(3,IVENT)
                TT1=TT-TTF-TVENT
              ENDIF
              SVTFAC=ZERO
C
              IM  = MATTG(IEL)
              MTN = IPM(2,IM)
              IF (MTN/=19.AND.MTN/=58) CYCLE
C
              ILEAKAGE = IPM(4,IM)
              NFUNC    = IPM(10,IM)+IPM(6,IM)
              IF(ILEAKAGE==0) THEN
                SVTFAC=ZERO
              ELSEIF(ILEAKAGE==1) THEN
                FLC=PM(164,IM)
                FAC=PM(165,IM)
                SVTFAC=FLC*FAC
              ELSEIF(ILEAKAGE==2.OR.ILEAKAGE==3) THEN
                FLC=ZERO
                IPORT=IPM(10+NFUNC-1,IM)
                IF(IPORT > 0) THEN
                  SCALT=PM(160,IM)
                  FPORT=PM(164,IM)
                  FLC=FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
                ENDIF
                FAC=ZERO
                IPORP=IPM(10+NFUNC-2,IM)
                IF(IPORP > 0) THEN
                  SCALP=PM(161,IM)
                  FPORP=PM(165,IM)
                  IF(ILEAKAGE==2) THEN
                    FAC=FPORP*GET_U_FUNC(IPORP,PMEAN*SCALP,DERI)
                  ELSE
                    FAC=FPORP*GET_U_FUNC(IPORP,(PMEAN-PEXT)*SCALP,DERI)
                  ENDIF
                ENDIF
                SVTFAC=FLC*FAC
              ELSEIF(ILEAKAGE==4) THEN
                AINI=ELSINI(IEL)
                CALL PORFORM4(SVTFAC,IM,IPM,PM,AREA,AINI,P,PEXT)
              ELSEIF(ILEAKAGE==5) THEN
                IF(NSPMD > 1) THEN
                  CALL ANCMSG(MSGID=258,ANMODE=ANINFO,I1=IPM(1,IM))
                  CALL ARRET(2)
                ELSE
                  K = ELTG(IEL)
                  IF(K <= NUMELCG)THEN
                    NG=IGROUPC(K)
                  ELSE
                    K=K-NUMELCG
                    NG=IGROUPTG(K)
                  ENDIF
                  NELG = IPARG(2,NG)
                  NFT  = IPARG(3,NG)
                  JEL  = K-NFT
                  CALL PORFORM5(SVTFAC,IM,IPM,PM,ELBUF_TAB(NG),P,PEXT,
     .                          JEL,NELG)
                  AREA=ELSINI(IEL)
                ENDIF
              ELSEIF(ILEAKAGE==6) THEN
                AINI=ELSINI(IEL)
                CALL PORFORM6(SVTFAC,IM,PM,AREA,AINI,P,PEXT)
              ENDIF
C
              IF(INTBAG==0) THEN
                AOUT = AREA*SVTFAC
                AOUT1= ZERO
              ELSE
                IF(IBLOCKAGE==1) THEN
                  AOUT = (ONE - PORO(IEL))*AREA*SVTFAC
                  AOUT1= ZERO
                ELSE
                  AOUT =(ONE - PORO(IEL))*AREA*SVTFAC
                  FAC1=PM(162,IM)
                  IF(FAC1 == ZERO) THEN
                    IPORT=IPM(10+NFUNC,IM)
                    IF(IPORT > 0) THEN
                      SCALT=PM(160,IM)
                      FPORT=PM(163,IM)
                      FAC1=FPORT*GET_U_FUNC(IPORT,TT1*SCALT,DERI)
                    ENDIF
                  ENDIF
                  AOUT1=  FAC1*PORO(IEL)*AREA*SVTFAC
                ENDIF
              ENDIF
            ENDIF  ! vent hole or porous surface
C
            ELSOUT(IEL)=AOUT+AOUT1
            AOUTOT=AOUTOT+ELSOUT(IEL)
            RBAGHOL(16,IVENT)=RBAGHOL(16,IVENT)+AOUT
            RBAGHOL(17,IVENT)=RBAGHOL(17,IVENT)+AOUT1
C Triangle interne
            IF(IEL > NEL) THEN
              POROSITY(IEL-NEL)=MIN(ONE,ELSOUT(IEL)/AREA)
            ENDIF
          ENDIF  ! test if triangle is venting
        ENDDO     ! loop on triangles
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        RETURN
      END
