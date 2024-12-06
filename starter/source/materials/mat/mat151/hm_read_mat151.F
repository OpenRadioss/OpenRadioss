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
C     ..... FOR DEMO  ......
      !||====================================================================
      !||    hm_read_mat151             ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat                ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    init_mat_keyword           ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
      !||    inivol_def_mod             ../starter/share/modules1/inivol_mod.F
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_MAT151( MTAG, PM, IPM, ID, TITR, MULTI_FVM, UNITAB, LSUBMODEL, MATPARAM )
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
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      INTEGER,INTENT(INOUT) :: IPM(NPROPMI)
      INTEGER,INTENT(IN) :: ID
      my_real,INTENT(INOUT) :: PM(NPROPM)      
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB       
      TYPE(MLAW_TAG_),INTENT(INOUT) :: MTAG      
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: II
      my_real :: FRAC_VOL, SUM_FRAC_VOL
      my_real :: VFRAC(21)
      INTEGER :: IMID(21)
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      NBMAT = 0
      CALL HM_GET_INTV('NIP',NBMAT,IS_AVAILABLE,LSUBMODEL)
      MULTI_FVM%NBMAT = MAX(MULTI_FVM%NBMAT, NBMAT)

      IF (NBMAT > 21) THEN
         !! Limitation to 20 material
         CALL ANCMSG(MSGID = 87, MSGTYPE = MSGERROR, ANMODE = ANINFO)
      ENDIF

C     Store number of materials
      IPM(20) = NBMAT
      PM(20) = NBMAT + EM01
      MATPARAM%MULTIMAT%NB = NBMAT
      ! Multimaterial data
      IF(.NOT.ALLOCATED(MATPARAM%multimat%vfrac))ALLOCATE(MATPARAM%multimat%vfrac(NBMAT))
      IF(.NOT.ALLOCATED(MATPARAM%multimat%mid))  ALLOCATE(MATPARAM%multimat%mid(NBMAT))
      

C     Reading submaterial IDS and corresponding volumic fractions
      SUM_FRAC_VOL = ZERO
      DO II = 1, NBMAT
         CALL HM_GET_INT_ARRAY_INDEX('MAT_ID_ARRAY',MAT_ID,II,IS_AVAILABLE,LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('VOL_FRAC_ARRAY',FRAC_VOL,II,IS_AVAILABLE,LSUBMODEL,UNITAB)
         IPM(20 + II) = MAT_ID
         MATPARAM%MULTIMAT%MID(II) = MAT_ID
         PM(20 + II) = FRAC_VOL
         MATPARAM%MULTIMAT%VFRAC(II) = FRAC_VOL
         IMID(II) = MAT_ID
         VFRAC(II) = FRAC_VOL
         SUM_FRAC_VOL = SUM_FRAC_VOL + FRAC_VOL
         IF (FRAC_VOL < ZERO .OR. FRAC_VOL > ONE) THEN
            IF (NUM_INIVOL == 0) THEN
               CALL ANCMSG(MSGID = 1511, MSGTYPE = MSGERROR, ANMODE = ANINFO, C1 = "ERROR", I1 = ID, I2 = MAT_ID, R1 = FRAC_VOL)
            ELSE
               CALL ANCMSG(MSGID = 1511, MSGTYPE = MSGWARNING, ANMODE = ANINFO, C1 = "WARNING", I1 = ID, I2 = MAT_ID, R1 = FRAC_VOL)
            ENDIF
         ENDIF
      ENDDO
      IF (SUM_FRAC_VOL /= ONE) THEN
         IF (NUM_INIVOL == 0) THEN
            CALL ANCMSG(MSGID = 1512, MSGTYPE = MSGERROR, ANMODE = ANINFO, C1 = "ERROR", I1 = ID, R1 = SUM_FRAC_VOL)
         ELSE
            CALL ANCMSG(MSGID = 1512, MSGTYPE = MSGWARNING, ANMODE = ANINFO, C1 = "WARNING", I1 = ID, R1 = SUM_FRAC_VOL)
            IF(SUM_FRAC_VOL == ZERO)THEN
              PM(20 + 1) = ONE  !default submaterial is submat #1
              MATPARAM%MULTIMAT%VFRAC(1) = ONE
            ENDIF
         ENDIF
      ENDIF

      MTAG%L_FRAC = 1
      MTAG%G_DELTAX = 1
      MTAG%L_DELTAX = 1
      MTAG%G_RHO = 1
      MTAG%L_RHO = 1

      ! MATPARAM keywords

      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")

      ! Multimaterial data
      MATPARAM%multimat%vfrac(1:NBMAT) = VFRAC(1:NBMAT)
      MATPARAM%multimat%mid(1:NBMAT) = IMID(1:NBMAT)

      WRITE(IOUT,1000) NBMAT
      DO II = 1, NBMAT
         WRITE(IOUT, 1010) MATPARAM%multimat%mid(II), MATPARAM%multimat%vfrac(II)
      ENDDO
      
 1000 FORMAT(
     & 5X,'  MULTIFLUID LAW   ',/,
     & 5X,'  --------------  ',/,
     & 5X,'  NUMBER OF MATERIALS. . . . . . .=',I8//)
 1010 FORMAT(
     & 5X,'  MATERIAL ID', I8, ' ; VOLUME FRACTION',1PG20.13/)
      RETURN
 
C-----------------------------------------------
      END SUBROUTINE HM_READ_MAT151
