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
C     ..... FOR DEMO  ......
Chd|====================================================================
Chd|  HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT151(MTAG, PM, IPM, ID, TITR, MULTI_FVM,UNITAB,LSUBMODEL)
C     ..... FOR DEMO  ......
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW151 WITH HM READER ( TO BE COMPLETED )
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     ..... A FAIRE  ......
C     ............   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD       
      USE SUBMODEL_MOD 
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real
     .   PM(*)
      INTEGER :: IPM(*)
      TYPE(MLAW_TAG_) :: MTAG
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: II
      my_real :: FRAC_VOL, SUM_FRAC_VOL
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      NBMAT = 0
      CALL HM_GET_INTV('NIP',NBMAT,IS_AVAILABLE,LSUBMODEL)
      MULTI_FVM%NBMAT = MAX(MULTI_FVM%NBMAT, NBMAT)

      IF (NBMAT .GT. 21) THEN
         !! Limitation to 20 material
         CALL ANCMSG(MSGID = 87, MSGTYPE = MSGERROR, ANMODE = ANINFO)
      ENDIF

C     Store number of materials
      IPM(20) = NBMAT
      PM(20) = NBMAT + EM01

C     Reading submaterial IDS and corresponding volumic fractions
      SUM_FRAC_VOL = ZERO
      DO II = 1, NBMAT
         CALL HM_GET_INT_ARRAY_INDEX('MAT_ID_ARRAY',MAT_ID,II,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('VOL_FRAC_ARRAY',FRAC_VOL,II,IS_AVAILABLE,LSUBMODEL,UNITAB)
         IPM(20 + II) = MAT_ID
         PM(20 + II) = FRAC_VOL
         SUM_FRAC_VOL = SUM_FRAC_VOL + FRAC_VOL
         IF (FRAC_VOL .LT. ZERO .OR. FRAC_VOL .GT. ONE) THEN
            IF (NINIVOL .EQ. 0) THEN
               CALL ANCMSG(MSGID = 1511, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .              C1 = "ERROR", I1 = ID, I2 = MAT_ID, R1 = FRAC_VOL) 
            ELSE
               CALL ANCMSG(MSGID = 1511, MSGTYPE = MSGWARNING, ANMODE = ANINFO, 
     .              C1 = "WARNING", I1 = ID, I2 = MAT_ID, R1 = FRAC_VOL) 
            ENDIF
         ENDIF
      ENDDO
      IF (SUM_FRAC_VOL .NE. ONE) THEN
         IF (NINIVOL .EQ. 0) THEN
            CALL ANCMSG(MSGID = 1512, MSGTYPE = MSGERROR, ANMODE = ANINFO, 
     .           C1 = "ERROR", I1 = ID, R1 = SUM_FRAC_VOL) 
         ELSE
            CALL ANCMSG(MSGID = 1512, MSGTYPE = MSGWARNING, ANMODE = ANINFO, 
     .           C1 = "WARNING", I1 = ID, R1 = SUM_FRAC_VOL) 
         ENDIF
      ENDIF

      MTAG%L_FRAC = 1
      MTAG%G_DELTAX = 1
      MTAG%L_DELTAX = 1
      MTAG%G_RHO = 1
      MTAG%L_RHO = 1

      WRITE(IOUT,1000) NBMAT
      DO II = 1, NBMAT
         WRITE(IOUT, 1010) IPM(20 + II), PM(20 + II)
      ENDDO
      
 1000 FORMAT(
     & 5X,'  MULTIFLUID LAW   ',/,
     & 5X,'  --------------  ',/,
     & 5X,'  NUMBER OF MATERIALS. . . . . . .=',I8//)
 1010 FORMAT(
     & 5X,'  MATERIAL ID', I8, ' ; VOLUME FRACTION',1PG20.13/)
      RETURN
 999  CALL FREERR(3)
 
C-----------------------------------------------
      END SUBROUTINE HM_READ_MAT151
