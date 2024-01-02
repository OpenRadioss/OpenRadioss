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
Chd|  HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_GJOINT
     1              (GJBUFI  ,GJBUFR  ,ITAB    ,ITABM   ,X       ,
     2               MASS    ,INER    ,LAG_NCF ,LAG_NKF ,LAG_NHF ,
     3               IKINE   ,UNITAB  ,IKINE1LAG,NOM_OPT,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER LAG_NCF ,LAG_NKF, LAG_NHF, 
     .        GJBUFI(LKJNI,*), ITAB(*), ITABM(*), IKINE(*),
     .        IKINE1LAG(*)
      my_real 
     .        GJBUFR(LKJNR,*), X(3,*), MASS(*), INER(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,KK,ID,UID,JTYP,N1,N2,N3,N0,NG,SUB_ID
      my_real R1(3),R2(3),R3(3),
     .        ALPHA,MS0,MS1,MS2,MS3,IN0,IN1,IN2,IN3,L1,L2,L3
      CHARACTER TITR*nchartitle,KEY*ncharkey,MESS*40
      DATA MESS/'GEAR JOINTS DEFINITION          '/
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C     GJBUFI(1,I) = ID
C     GJBUFI(2,I) = TYPE
C     GJBUFI(3,I) = N0
C     GJBUFI(4,I) = N1
C     GJBUFI(5,I) = N2
C     GJBUFI(6,I) = N3
C-----------------------------------------------
C     GJBUFR(    1,I) = ALPHA
C     GJBUFR(2 -10,I) = Local Skew
C     GJBUFR(11-13,I) = N1 Axis
C     GJBUFR(14-16,I) = N2 Axis
C     GJBUFR(17-19,I) = N3 Axis
C======================================================================|
C
      IS_AVAILABLE = .FALSE.
C
      WRITE(IOUT,1000)
      NG = 0
C      
      CALL HM_OPTION_START('/GJOINT')
C      
      DO I=1,NGJOINT
        NG=NG+1
C----------Multidomaines --> on ignore les gjoint non tages--------
        IF(NSUBDOM>0)THEN
            IF(TAGJOIN(NG)==0)CALL HM_SZ_R2R(TAGJOIN,NG,LSUBMODEL)
        END IF
C-----------------------------------------------------------------      
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
C
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
C
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('node_ID0',N0,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('node_ID1',N1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('node_ID2',N2,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('node_ID3',N3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FscaleV',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Mass',MS0,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Inertia',IN0,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        CALL HM_GET_FLOATV('Mass1',MS1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Inertia1',IN1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r1x',R1(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r1y',R1(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r1z',R1(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        CALL HM_GET_FLOATV('Mass2',MS2,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('Inertia2',IN2,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r2x',R2(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r2y',R2(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('r2z',R2(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        IF(KEY(1:4)=='DIFF') THEN
          CALL HM_GET_FLOATV('Mass3',MS3,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Inertia3',IN3,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('r3x',R3(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('r3y',R3(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('r3z',R3(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDIF
C------   
        N0 = USR2SYS(N0,ITABM,MESS,ID)          
        N1 = USR2SYS(N1,ITABM,MESS,ID)          
        N2 = USR2SYS(N2,ITABM,MESS,ID) 
        MASS(N0) =  MASS(N0) + MS0
        MASS(N1) =  MASS(N1) + MS1
        MASS(N2) =  MASS(N2) + MS2
        INER(N0) =  INER(N0) + IN0   
        INER(N1) =  INER(N1) + IN1   
        INER(N2) =  INER(N2) + IN2  
        IF (ALPHA==0) ALPHA = 1.0
        IF(R1(1)==ZERO.AND.R1(2)==ZERO.AND.R1(3)==ZERO) R1(1)=1.
        IF(R2(1)==ZERO.AND.R2(2)==ZERO.AND.R2(3)==ZERO) R2(1)=1.
        CALL KINSET(512,ITAB(N0),IKINE(N0),7,0,IKINE1LAG(N0))
        CALL KINSET(512,ITAB(N1),IKINE(N1),7,0,IKINE1LAG(N1))
        CALL KINSET(512,ITAB(N2),IKINE(N2),7,0,IKINE1LAG(N2))
C---       
        IF(KEY(1:4)=='GEAR') THEN
          JTYP = 1
          N3 = 0
          R3(1) = ONE
          R3(2) = ZERO
          R3(3) = ZERO
        ELSEIF(KEY(1:4)=='DIFF') THEN
          JTYP = 2
          N3 = USR2SYS(N3,ITABM,MESS,ID) 
          CALL KINSET(512,ITAB(N3),IKINE(N3),7,0,IKINE1LAG(N3))
          IF(R3(1)==ZERO.AND.R3(2)==ZERO.AND.R3(3)==ZERO) R3(1)=1.
          MASS(N3) =  MASS(N3) + MS3
          INER(N3) =  INER(N3) + IN3   
        ELSEIF(KEY(1:4)=='RACK') THEN
          JTYP = 3
          N3 = 0
          R3(1) = ONE
          R3(2) = ZERO
          R3(3) = ZERO

        ELSE
c         unknown type
        ENDIF  
C------         
        L1 = ONE/SQRT(R1(1)*R1(1)+R1(2)*R1(2)+R1(3)*R1(3))
        L2 = ONE/SQRT(R2(1)*R2(1)+R2(2)*R2(2)+R2(3)*R2(3))
        L3 = ONE/SQRT(R3(1)*R3(1)+R3(2)*R3(2)+R3(3)*R3(3))

        DO J = 1,3
          R1(J) = R1(J)*L1
          R2(J) = R2(J)*L2
          R3(J) = R3(J)*L3
        ENDDO
        GJBUFI(1,I) = ID           
        GJBUFI(2,I) = JTYP           
        GJBUFI(3,I) = N0         
        GJBUFI(4,I) = N1          
        GJBUFI(5,I) = N2
        GJBUFI(6,I) = N3
C
        GJBUFR( 1,I) = ONE/ALPHA 
        GJBUFR( 2,I) = ONE
        GJBUFR( 3,I) = ZERO
        GJBUFR( 4,I) = ZERO
        GJBUFR( 5,I) = ZERO
        GJBUFR( 6,I) = ONE
        GJBUFR( 7,I) = ZERO
        GJBUFR( 8,I) = ZERO
        GJBUFR( 9,I) = ZERO
        GJBUFR(10,I) = ONE
        GJBUFR(11,I) = R1(1)
        GJBUFR(12,I) = R1(2)
        GJBUFR(13,I) = R1(3)
        GJBUFR(14,I) = R2(1)
        GJBUFR(15,I) = R2(2)
        GJBUFR(16,I) = R2(3)
        GJBUFR(17,I) = R3(1)
        GJBUFR(18,I) = R3(2)
        GJBUFR(19,I) = R3(3)
C
C---
        IF (JTYP==1) THEN
          LAG_NHF = LAG_NHF + 55
          LAG_NCF = LAG_NCF + 11
          LAG_NKF = LAG_NKF + 60
          WRITE(IOUT,1101)ID,JTYP,ITAB(N1),ITAB(N2),ITAB(N0),
     .          ALPHA,MS1,MS2,MS0,IN1,IN2,IN0,
     .          R1(1),R1(2),R1(3),R2(1),R2(2),R2(3)
        ELSEIF (JTYP==2) THEN
          LAG_NHF = LAG_NHF + 78
          LAG_NCF = LAG_NCF + 13
          LAG_NKF = LAG_NKF + 108
          WRITE(IOUT,1102)ID,JTYP,ITAB(N1),ITAB(N2),ITAB(N3),ITAB(N0),
     .          ALPHA,MS1,MS2,MS3,MS0,IN1,IN2,IN3,IN0,
     .          R1(1),R1(2),R1(3),R2(1),R2(2),R2(3),R3(1),R3(2),R3(3)
        ELSEIF (JTYP==3) THEN
          LAG_NHF = LAG_NHF + 36
          LAG_NCF = LAG_NCF + 9
          LAG_NKF = LAG_NKF + 48
          WRITE(IOUT,1101)ID,JTYP,ITAB(N1),ITAB(N2),ITAB(N0),
     .          ALPHA,MS1,MS2,MS0,IN1,IN2,IN0,
     .          R1(1),R1(2),R1(3),R2(1),R2(2),R2(3)
        ENDIF
C---        
      ENDDO
C---        
      RETURN
 1000 FORMAT(//
     .'       COMPLEX JOINTS  (GEAR TYPE) '/
     . '      --------------------------- ')
 1101 FORMAT( 5X,' JOINT ID . . . . . . . . . . . .',I10
     .       /10X,'JOINT TYPE . . . . . . . . . . .',I10
     .       /10X,'N1 . . . . . . . . . . . . . . .',I10
     .       /10X,'N2 . . . . . . . . . . . . . . .',I10
     .       /10X,'MAIN NODE. . . . . . . . . . .',I10
     .       /10X,'ALPHA. . . . . . . . . . . . . .',1PG20.13
     .       /10X,'ADDED N1 MASS. .   . . . . . . .',1PG20.13
     .       /10X,'ADDED N2 MASS. .   . . . . . . .',1PG20.13
     .       /10X,'ADDED MAIN MASS  . . . . . . .',1PG20.13
     .       /10X,'ADDED N1 INERTIA . . . . . . . .',1PG20.13
     .       /10X,'ADDED N2 INERTIA . . . . . . . .',1PG20.13
     .       /10X,'ADDED MAIN INERTIA . . . . . .',1PG20.13
     .       /10X,'VECTOR T1: '
     .       /10X,'        ',1PG20.13,1PG20.13,1PG20.13
     .       /10X,'VECTOR T2: '
     .       /10X,'        ',1PG20.13,1PG20.13,1PG20.13/)
 1102 FORMAT( 5X,' JOINT ID . . . . . . . . . . . .',I10
     .       /10X,'JOINT TYPE . . . . . . . . . . .',I10
     .       /10X,'N1 . . . . . . . . . . . . . . .',I10
     .       /10X,'N2 . . . . . . . . . . . . . . .',I10
     .       /10X,'N3 . . . . . . . . . . . . . . .',I10
     .       /10X,'MAIN NODE. . . . . . . . . . .',I10
     .       /10X,'ALPHA. . . . . . . . . . . . . .',1PG20.13
     .       /10X,'ADDED N1 MASS. .   . . . . . . .',1PG20.13
     .       /10X,'ADDED N2 MASS. .   . . . . . . .',1PG20.13
     .       /10X,'ADDED N3 MASS. .   . . . . . . .',1PG20.13
     .       /10X,'ADDED MAIN MASS  . . . . . . .',1PG20.13
     .       /10X,'ADDED N1 INERTIA . . . . . . . .',1PG20.13
     .       /10X,'ADDED N2 INERTIA . . . . . . . .',1PG20.13
     .       /10X,'ADDED N3 INERTIA . . . . . . . .',1PG20.13
     .       /10X,'ADDED MAIN INERTIA . . . . . .',1PG20.13
     .       /10X,'VECTOR T1: '
     .       /10X,'        ',1PG20.13,G20.13,G20.13
     .       /10X,'VECTOR T2: '
     .       /10X,'        ',1PG20.13,G20.13,G20.13
     .       /10X,'VECTOR T3: '
     .       /10X,'        ',1PG20.13,G20.13,G20.13/)
C---
      RETURN
      END
      
