Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  READ_ALE_MAT                  source/materials/ale/read_ale_mat.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALE_SET_CONVECTION_CODES(IPM,PM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "alefvm.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NPROPMI,NUMMAT), INTENT(IN) :: IPM
      my_real, DIMENSION(NPROPM,NUMMAT), INTENT(INOUT) :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,ILAW,JTUR,JTHE,JALE,CODREZO,CODCONV
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO II=1,LCONV
        CODV(II)=0
      ENDDO
      IF(IALE+IEULER/=0.AND.ICONVCP == 1)THEN
        WRITE(IOUT,2008)
      END IF
      ! mass and energy convection for ALE/EULER laws
      DO II=1,NUMMAT-1
        ILAW = IPM(2,II)
        JTUR = NINT(PM(70,II))
        JTHE = NINT(PM(71,II))
        JALE = NINT(PM(72,II))
        CODCONV=0
        CODREZO=0
        IF(JALE>0)THEN
          IF(ILAW==1 .OR.
     .       ILAW==2 .OR.
     .       ILAW==3 .OR.
     .       ILAW==4 .OR.
     .       ILAW==5 .OR.
     .       ILAW==6 .OR.
     .       ILAW==7 .OR.
     .       ILAW==8 .OR.
     .       ILAW==9 .OR.
     .       ILAW==10 .OR.
     .       ILAW==11 .OR.
     .       ILAW==13 .OR.
     .       ILAW==16 .OR.
     .       ILAW==17 .OR.
     .       ILAW==18 .OR.
     .       ILAW==20 .OR.
     .       ILAW==21 .OR.
     .       ILAW==22 .OR.
     .       ILAW==23 .OR.
     .       ILAW==26 .OR.
     .       ILAW==29 .OR.
     .       ILAW==30 .OR.
     .       ILAW==31 .OR.
     .       ILAW==36 .OR.
     .       ILAW==37 .OR.
     .       ILAW==41 .OR.
     .       ILAW==44 .OR.
     .       ILAW==46 .OR.
     .       ILAW==47 .OR.
     .       ILAW==49 .OR.
     .       ILAW>=50     )THEN
                CODCONV = 11
                CODV(1) = 01
                CODV(2) = 01
          ENDIF
        ENDIF
        IF(JTUR>0) THEN
          CODCONV=CODCONV + 1100
          CODV(3)=1
          CODV(4)=1
        ENDIF
        IF(ILAW == 37 .OR. ILAW == 41)THEN
          CODCONV=CODCONV+10000
          CODV(5)=1
        ENDIF
        IF(JALE>0 .AND.(
     .     ILAW == 2  .OR.
     .     ILAW == 3  .OR.
     .     ILAW == 4  .OR.
     .     ILAW == 7  .OR.
     .     ILAW == 8  .OR.
     .     ILAW == 9  .OR.
     .     ILAW == 10 .OR.
     .     ILAW == 16 .OR.
     .     ILAW == 21 .OR.
     .     ILAW == 22 .OR.
     .     ILAW == 23 .OR.
     .     ILAW == 26 .OR.
     .     ILAW == 28 .OR.
     .     ILAW == 29 .OR.
     .     ILAW == 30 .OR.
     .     ILAW == 31 .OR.
     .     ILAW == 36 .OR.
     .     ILAW == 44 .OR.
     .     ILAW == 49 .OR.
     .     ILAW == 97       )  )THEN
             CODREZO=11
           ENDIF
         IF(JALE>0 .AND. ILAW == 1)THEN
            CODREZO=1
         ENDIF
         
        !---ALEFVM (obsolete)---! 
        IF(JALE > 0 .AND. IALEFVM == 1)THEN
          !Momentum Convection : %MOM(1:3,:)
          CODCONV=CODCONV+011100000 
          CODV(6) = 1
          CODV(7) = 1
          CODV(8) = 1                    
        ENDIF
        
        PM(10,II)=CODCONV+EM01
        PM(11,II)=CODREZO+EM01

      ENDDO

      !ALE CONVECTION INDEXES
      NVCONV=0
      DO II=1,LCONV
         IF(CODV(II) == 1)THEN
            NVCONV=NVCONV+1
            CODV(II)=NVCONV
         ENDIF
      ENDDO
 
 2008  FORMAT(
     & 5X,'    TRANSPORT of SPECIFIC HEAT for LAW67 is ON ',/,
     & 5X,'    ------------------------------------------- ',/)       
             
      RETURN
      END SUBROUTINE ALE_SET_CONVECTION_CODES 
