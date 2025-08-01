Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    law90_upd              ../starter/source/materials/mat/mat090/law90_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    func_slope             ../starter/source/tools/curve/func_slope.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW90_UPD(IOUT,TITR,MAT_ID,UPARAM,IPM, FUNC_ID,NPC,PLD,PM,
     .                     NFUNCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE)  :: TITR
      INTEGER ,INTENT(IN) :: NFUNCT
      INTEGER MAT_ID,IOUT,ID_F1,ID_F2
      INTEGER NPC(*), FUNC_ID(NFUNCT), IPM(NPROPMI)
      my_real 
     .         UPARAM(*),PLD(*),PM(NPROPM)
!      TYPE(TTABLE) TABLE(*)
      TARGET IPM
      INTEGER, DIMENSION(:), POINTER :: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFUNC,I,J
      INTEGER IPRINT, LP,IndexDesign0

      my_real 
     .      STIFF,STIFFC,STIFFT, EMAX_CURVE, EMIN_CURVE,EINI_CURVE,
     .     STIFFMIN,STIFFMAX,STIFFINI,STFAVG,SCALEFAC,E0,
     .     EPSMAX,EC_MAX,EMAX,G,C1,NU

      COMMON /MINCFPRT/IPRINT, LP
      COMMON /INDEX_SQP/IndexDesign0
C=======================================================================
      NFUNC =  IPM(10)
      IFUNC => IPM(10+1:10+NFUNC)
C=======================================================================
c
           EMAX_CURVE = ZERO
           EMIN_CURVE = EP20
           EINI_CURVE = ZERO
           EPSMAX = ONE
          DO J = 1, NFUNC 
              SCALEFAC= UPARAM(10 + NFUNC + J )  
              CALL FUNC_SLOPE(IFUNC(J),SCALEFAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STFAVG)  
              EMAX_CURVE = MAX(EMAX_CURVE,  STIFFMAX )
              EMIN_CURVE = MIN(EMIN_CURVE,  STIFFMIN)
              EINI_CURVE = MAX(EINI_CURVE, STIFFINI)
          ENDDO ! NFUNC 
          E0 = UPARAM(1) 
          IF(E0 < EINI_CURVE) THEN
             E0 = EINI_CURVE
             UPARAM(1) = E0   
             CALL ANCMSG(MSGID=865, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                 I1=MAT_ID,
     .                 C1=TITR,
     .                 R1=E0)
          ENDIF   
          IF(EMAX_CURVE <= E0) THEN
            EMAX = E0
          ELSE  
           EMAX = MIN(EMAX_CURVE,HUNDRED*E0 ) !
          ENDIF 
          EC_MAX = MAX(E0,EMAX)  
          UPARAM(11 + 2*NFUNC) = EMAX
          UPARAM(12 + 2*NFUNC) = EPSMAX
          NU = UPARAM(5)
          C1 = E0/THREE/(ONE - TWO*NU)
          G  = HALF*E0/(ONE + NU) 
          PM(20) = E0
          PM(22) = G
          PM(24) = EC_MAX
          PM(32) = C1     
!
         WRITE(IOUT,1000)
         WRITE(IOUT,1001) TRIM(TITR),MAT_ID,90   
         WRITE(IOUT,1002) EMAX       
         WRITE(IOUT,1003) EC_MAX       
c----------------
      RETURN
1000  FORMAT(
     & 5X,'  TABULATED NON LINEAR VISCO ELASTIC LAW',/,
     & 5X,' ----------------------------- --------',//)
1001  FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
1002   FORMAT(
     & 5X,'MAXIMUM YOUNG''S MODULUS. . . . . . . .=',1PG20.13/)
1003   FORMAT(
     & 5X,'YOUNG''S MODULUS FOR HG COMPUTE . . . .=',1PG20.13/)

      END
