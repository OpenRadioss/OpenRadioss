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
Chd|  LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        GJNT_DIFF                     source/tools/lagmul/gjnt_diff.F
Chd|        GJNT_GEAR                     source/tools/lagmul/gjnt_gear.F
Chd|        GJNT_RACK                     source/tools/lagmul/gjnt_rack.F
Chd|        LAG_DIRECT                    source/tools/lagmul/lag_direct.F
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|====================================================================
      SUBROUTINE LAG_GJNT(GJBUFI ,GJBUFR ,X      ,VR     ,AR     ,
     2                    IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                    COMNTAG,LTSM   ,ICFTAG ,JCFTAG ,MS     ,
     4                    IN     ,V      ,A      ,ISKIP  ,NCF_S  ,
     5                    NC     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,ISKIP,NCF_S,
     .        LLL(*),JLL(*),SLL(*),IADLL(*),GJBUFI(LKJNI,*),
     .        COMNTAG(*),ICFTAG(*),JCFTAG(*)
      my_real 
     .        GJBUFR(LKJNR,*),LTSM(6,*),XLL(*),MS(*),IN(*),
     .        X(3,*),V(3,*),A(3,*),VR(3,*),AR(3,*)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,JTYP,N0,N1,N2,N3,IC,NC_INI,NCL,COMFLAG
      my_real 
     .        VRN(3),DVR(3),VL(3),P(9),L1(3),L2(3),L3(3),ALPHA, DTD
C======================================================================|
      DO I=1,NGJOINT
        JTYP= GJBUFI(2,I)        
        N0  = GJBUFI(3,I)        
        N1  = GJBUFI(4,I)        
        N2  = GJBUFI(5,I)
        N3  = GJBUFI(6,I)
C
C---    Frame actualisation (main skew) at the beginning of the step        
        VRN(1) = VR(1,N0) + DT12*AR(1,N0)
        VRN(2) = VR(2,N0) + DT12*AR(2,N0)
        VRN(3) = VR(3,N0) + DT12*AR(3,N0)
        VL(1)  = GJBUFR( 2,I)*VR(1,N0)
     .          +GJBUFR( 3,I)*VR(2,N0)
     .          +GJBUFR( 4,I)*VR(3,N0)
        VL(2)  = GJBUFR( 5,I)*VR(1,N0)
     .          +GJBUFR( 6,I)*VR(2,N0)
     .          +GJBUFR( 7,I)*VR(3,N0)
        VL(3)  = GJBUFR( 8,I)*VR(1,N0)
     .          +GJBUFR( 9,I)*VR(2,N0)
     .          +GJBUFR(10,I)*VR(3,N0)
        CALL ROTBMR (VL ,GJBUFR(2,I) ,DT1)
C---    Frame estimation at the middle of the step    
        DVR(1) = DT12*AR(1,N0)
        DVR(2) = DT12*AR(2,N0)
        DVR(3) = DT12*AR(3,N0)
        P(1) = GJBUFR( 2,I)
        P(2) = GJBUFR( 3,I)
        P(3) = GJBUFR( 4,I)
        P(4) = GJBUFR( 5,I)
        P(5) = GJBUFR( 6,I)
        P(6) = GJBUFR( 7,I)
        P(7) = GJBUFR( 8,I)
        P(8) = GJBUFR( 9,I)
        P(9) = GJBUFR(10,I)
        VL(1)=GJBUFR(2,I)*VRN(1)+GJBUFR(3,I)*VRN(2)+GJBUFR(4,I)*VRN(3)
        VL(2)=GJBUFR(5,I)*VRN(1)+GJBUFR(6,I)*VRN(2)+GJBUFR(7,I)*VRN(3)
        VL(3)=GJBUFR(8,I)*VRN(1)+GJBUFR(9,I)*VRN(2)+GJBUFR(10,I)*VRN(3)
        VL(1)=P(1)*DVR(1)+P(2)*DVR(2)+P(3)*DVR(3)
        VL(2)=P(4)*DVR(1)+P(5)*DVR(2)+P(6)*DVR(3)
        VL(3)=P(7)*DVR(1)+P(8)*DVR(2)+P(9)*DVR(3)
        DTD = HALF*DT2
        CALL ROTBMR (VL ,P ,DTD)
C
        ALPHA = GJBUFR( 1,I)
        L1(1) = GJBUFR(11,I)
        L1(2) = GJBUFR(12,I)
        L1(3) = GJBUFR(13,I)
        L2(1) = GJBUFR(14,I)
        L2(2) = GJBUFR(15,I)
        L2(3) = GJBUFR(16,I)
        NC_INI  = NC
        COMFLAG = 0
        IF (COMNTAG(N0)>1) COMFLAG = 1
        IF (COMNTAG(N1)>1) COMFLAG = 1
        IF (COMNTAG(N2)>1) COMFLAG = 1
C---
        IF (JTYP==1) THEN
          NCL = 11
          CALL GJNT_GEAR(P      ,L1     ,L2     ,ALPHA  ,X      ,
     2                   IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                   N0     ,N1     ,N2     ,NC     )  
        ELSEIF (JTYP==2) THEN
          IF (COMNTAG(N3)>1) COMFLAG = 1
          NCL = 13
          L3(1) = GJBUFR(17,I)
          L3(2) = GJBUFR(18,I)
          L3(3) = GJBUFR(19,I)
          CALL GJNT_DIFF(P      ,L1     ,L2     ,L3     ,ALPHA  ,
     2                   IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                   X      ,N0     ,N1     ,N2     ,N3     ,
     4                   NC     )  
        ELSEIF (JTYP==3) THEN
          NCL = 9
          CALL GJNT_RACK(P      ,L1     ,L2     ,ALPHA  ,X      ,
     2                   IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                   N0     ,N1     ,N2     ,NC     )  
        ENDIF
C
C---    Solving local Lagrange multipliers
        CALL LAG_DIRECT(
     1           IADLL    ,LLL      ,JLL      ,XLL      ,LTSM     ,
     2           V        ,VR       ,A        ,AR       ,MS       ,
     3           IN       ,NC_INI   ,NCL      )
        IF (COMFLAG==0) THEN
          ISKIP  = ISKIP + NCL
          NC = NC_INI
        ELSE
          IC = NC_INI - NCF_S
          DO K=1,NCL
            IC = IC + 1
            ICFTAG(IC) = IC + ISKIP
            JCFTAG(IC+ISKIP) = NC_INI + K
          ENDDO
        ENDIF
      ENDDO
C---
      RETURN
      END
