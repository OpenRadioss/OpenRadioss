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
Chd|  LAW90_UPD                     source/materials/mat/mat090/law90_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        FUNC_SLOPE                    source/tools/curve/func_slope.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW90_UPD(IOUT,TITR,MAT_ID,UPARAM,IPM, FUNC_ID,NPC,PLD,PM,
     .                     NFUNCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
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
     .      STIFF,STIFFC,STIFFT, EMAX, EMIN,EINI,
     .     STIFFMIN,STIFFMAX,STIFFINI,STFAVG,SCALEFAC,E0,
     .     EPSMAX,EC_MAX

      COMMON /MINCFPRT/IPRINT, LP
      COMMON /INDEX_SQP/IndexDesign0
C=======================================================================
      NFUNC =  IPM(10)
      IFUNC => IPM(10+1:10+NFUNC)
C=======================================================================
c
           EMAX = ZERO
           EMIN = EP20
           EINI = ZERO
           EPSMAX = ONE
          DO J = 1, NFUNC 
              SCALEFAC= UPARAM(10 + NFUNC + J )  
              CALL FUNC_SLOPE(IFUNC(J),SCALEFAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STFAVG)  
              EMAX = MAX(EMAX,  STIFFMAX )
              EMIN = MIN(EMIN,  STIFFMIN)
              EINI = MAX(EINI, STIFFINI)
          ENDDO ! NFUNC 
          E0 = UPARAM(1)
          EC_MAX = MAX(E0,EMAX)
          IF(EMAX <= E0) THEN
            EMAX = E0
          ELSE  
!!          EMAX= MIN(EMAX, HUNDRED*E0) 
           EMAX = HUNDRED*E0 ! by default value 
          ENDIF   
          UPARAM(11 + 2*NFUNC) = EMAX
          UPARAM(12 + 2*NFUNC) = EPSMAX
          PM(24) = EC_MAX
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
