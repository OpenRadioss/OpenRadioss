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
Chd|  HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        LEC_INIMAP1D_FILE             source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INIMAP1D(INIMAP1D, NPC     ,ITABM1, X, IGRBRIC,
     .                        IGRQUAD , IGRSH3N, MULTI_FVM, UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------   
      USE INIMAP1D_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
!NFUNCT
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPC(*), ITABM1(*)
      my_real, INTENT(IN) :: X(3, *)
      TYPE(INIMAP1D_STRUCT), DIMENSION(NINIMAP1D), INTENT(INOUT) :: INIMAP1D
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (UNIT_TYPE_), INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*40, KEY2*ncharline, TITR*nchartitle
      INTEGER :: II, JJ, IFUNC1, IFUNC2, IFUNC3, IFUNC4, LNODID1, LNODID2, KK, ID, UID, J
      DATA MESS/'INFILE'/
      INTEGER :: GRBRICID_LOC, GRQUADID_LOC, GRSH3NID_LOC, IAD1, IAD2
      my_real :: X0, Y0, Z0, X1, Y1, Z1, NORM, FAC1, FAC2, FAC_VEL, DUMMY
      INTEGER :: SIZE, LL
      LOGICAL :: FOUND
      CHARACTER FILENAME*ncharline
      CHARACTER MSG_DESCRIPTION*32
      LOGICAL :: IS_AVAILABLE

      IF (NINIMAP1D > 0) THEN
         WRITE(IOUT, 2000)
      ENDIF

      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_START('/INIMAP1D')

      DO KK = 1, NINIMAP1D
         INIMAP1D(KK)%CORRECTLY_READ=.TRUE.
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, UNIT_ID = UID, OPTION_TITR = TITR,
     .        KEYWORD2 = KEY2)
         INIMAP1D(KK)%ID = ID
         INIMAP1D(KK)%TITLE = TRIM(TITR)
         WRITE(IOUT, 2001) ID
         WRITE(IOUT, 2002) TRIM(TITR)
            INIMAP1D(KK)%FILE = .FALSE.
         IF (KEY2(1:2) == 'VP') THEN
            INIMAP1D(KK)%FORMULATION = 1
            WRITE(IOUT, 2010)
         ELSE IF (KEY2(1:2) == 'VE') THEN
            INIMAP1D(KK)%FORMULATION = 2
            WRITE(IOUT, 2020)
         ELSE IF (KEY2(1:5) == 'FILE ') THEN
            INIMAP1D(KK)%FORMULATION = 1
            INIMAP1D(KK)%FILE = .TRUE.
            WRITE(IOUT, 2025)
         ENDIF

         INIMAP1D(KK)%PROJ = -1
         CALL HM_GET_INTV('type', INIMAP1D(KK)%PROJ, IS_AVAILABLE, LSUBMODEL)
         INIMAP1D(KK)%NODEID1 = -1
         INIMAP1D(KK)%NODEID2 = -1

         IF (INIMAP1D(KK)%PROJ == 3) THEN
C     Spherical mapping
C     --> Read 1 node
            CALL HM_GET_INTV('node_ID1', INIMAP1D(KK)%NODEID1, IS_AVAILABLE, LSUBMODEL)
            WRITE(IOUT, 2030) "SPHERICAL"
         ELSE IF (INIMAP1D(KK)%PROJ == 1) THEN
C     Planar mapping
C     --> Read 2 nodes
            CALL HM_GET_INTV('node_ID1', INIMAP1D(KK)%NODEID1, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('node_ID2', INIMAP1D(KK)%NODEID2, IS_AVAILABLE, LSUBMODEL)
            WRITE(IOUT, 2030) "PLANAR"
         ELSE IF (INIMAP1D(KK)%PROJ == 2) THEN
C     Cylindrical mapping
C     --> Read 2 nodes
            CALL HM_GET_INTV('node_ID1', INIMAP1D(KK)%NODEID1, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('node_ID2', INIMAP1D(KK)%NODEID2, IS_AVAILABLE, LSUBMODEL)
            WRITE(IOUT, 2030) "CYLINDRICAL"
         ENDIF

         INIMAP1D(KK)%GRBRICID = 0
         INIMAP1D(KK)%GRQUADID = 0
         INIMAP1D(KK)%GRSH3NID = 0

         CALL HM_GET_INTV('grbric_ID', INIMAP1D(KK)%GRBRICID, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('grquad_ID', INIMAP1D(KK)%GRQUADID, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_INTV('grtria_ID', INIMAP1D(KK)%GRSH3NID, IS_AVAILABLE, LSUBMODEL)

C     ==============
C     Check if found
C     ==============
         IF (INIMAP1D(KK)%GRBRICID + INIMAP1D(KK)%GRQUADID + INIMAP1D(KK)%GRSH3NID == 0) THEN
            CALL ANCMSG(MSGID=1554, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .           C1='IN /INIMAP1D OPTION')
         ELSE
            GRBRICID_LOC = -1
            GRQUADID_LOC = -1
            GRSH3NID_LOC = -1
            IF (INIMAP1D(KK)%GRBRICID /= 0) THEN
               DO J = 1,NGRBRIC
                  IF (INIMAP1D(KK)%GRBRICID == IGRBRIC(J)%ID) THEN
                     GRBRICID_LOC  = J
                     INIMAP1D(KK)%GRBRICID = J
                     EXIT
                  ENDIF
               ENDDO
               IF (GRBRICID_LOC == -1) THEN
                  CALL ANCMSG(MSGID=1554,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1='IN /INIMAP1D OPTION',
     .                 I1=INIMAP1D(KK)%GRBRICID)
               ENDIF
            ENDIF
            IF (INIMAP1D(KK)%GRQUADID /= 0) THEN
               DO J = 1,NGRQUAD
                  IF (INIMAP1D(KK)%GRQUADID == IGRQUAD(J)%ID) THEN
                     GRQUADID_LOC = J
                     INIMAP1D(KK)%GRQUADID = J
                     EXIT
                  ENDIF
               ENDDO
               IF (GRQUADID_LOC == -1) THEN
                  CALL ANCMSG(MSGID=1554,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1='IN /INIMAP1D OPTION',
     .                 I1=INIMAP1D(KK)%GRQUADID)
               ENDIF
            ENDIF
            IF (INIMAP1D(KK)%GRSH3NID /= 0) THEN
               DO J = 1,NGRSH3N
                  IF (INIMAP1D(KK)%GRSH3NID == IGRSH3N(J)%ID) THEN
                     GRSH3NID_LOC = J
                     INIMAP1D(KK)%GRSH3NID = J
                     EXIT
                  ENDIF
               ENDDO
               IF (GRSH3NID_LOC == -1) THEN
                  CALL ANCMSG(MSGID=1554,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1='IN /INIMAP1D OPTION',
     .                 I1=INIMAP1D(KK)%GRSH3NID)
               ENDIF
            ENDIF
         ENDIF

C     ==============
C     functions
C     ==============
         IF(.NOT. INIMAP1D(KK)%FILE)THEN
            CALL HM_GET_INTV('FUN_IDV', IFUNC3, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_FLOATV('FSCALEV', FAC_VEL, IS_AVAILABLE, LSUBMODEL, UNITAB)

           MSG_DESCRIPTION = 'CANNOT READ VELOCITY FUNCTION ID'
           WRITE(IOUT, 2040) IFUNC3, FAC_VEL
           IF (IFUNC3 > 0) THEN
              JJ = 0
              FOUND = .FALSE.
              DO II = NFUNCT + 2, 2 * NFUNCT + 1
                 JJ = JJ + 1
                 IF (NPC(II) == IFUNC3) THEN
                    INIMAP1D(KK)%FUNC_VEL = JJ
                    FOUND = .TRUE.
                    EXIT
                 ENDIF
              ENDDO
              IF (.NOT. FOUND) THEN
                 CALL ANCMSG(MSGID = 120, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                C1 = 'IN /INIMAP1D OPTION', I1 = IFUNC3)
              ENDIF
           ELSE
              INIMAP1D(KK)%FUNC_VEL = 0
           ENDIF
           IF (FAC_VEL == ZERO) FAC_VEL = ONE
           INIMAP1D(KK)%FAC_VEL = FAC_VEL

           CALL HM_GET_INTV('Nb_integr', SIZE, IS_AVAILABLE, LSUBMODEL)

           INIMAP1D(KK)%NBMAT = SIZE
           ALLOCATE(INIMAP1D(KK)%FUNC_ALPHA(SIZE), INIMAP1D(KK)%FUNC_RHO(SIZE),
     .          INIMAP1D(KK)%FUNC_ENER(SIZE), INIMAP1D(KK)%FUNC_PRES(SIZE),
     .          INIMAP1D(KK)%FAC_PRES_ENER(SIZE), INIMAP1D(KK)%FAC_RHO(SIZE))
           INIMAP1D(KK)%FUNC_ALPHA(1:SIZE) = 0
           DO LL = 1, SIZE
              CALL HM_GET_INT_ARRAY_INDEX('fct_Idvfi', IFUNC1, LL, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('fct_IDri', IFUNC2, LL, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalerhoi', FAC1, LL, IS_AVAILABLE, LSUBMODEL, UNITAB)
              CALL HM_GET_INT_ARRAY_INDEX('fct_IDpei', IFUNC3, LL, IS_AVAILABLE, LSUBMODEL)
              CALL HM_GET_FLOAT_ARRAY_INDEX('Fscalepei', FAC2, LL, IS_AVAILABLE, LSUBMODEL, UNITAB)

C     FUNC_RHO, FUNC_PRES AND FUNC_VEL
C     OR FUNC_RHO, FUNC_ENER AND FUNC_VEL depending on the forumlation
              IF (IFUNC1 > 0) THEN
                 JJ = 0
                 FOUND = .FALSE.
                 DO II = NFUNCT + 2, 2 * NFUNCT + 1
                    JJ = JJ + 1
                    IF (NPC(II) == IFUNC1) THEN
                       INIMAP1D(KK)%FUNC_ALPHA(LL) = JJ
                       FOUND = .TRUE.
                       EXIT
                    ENDIF
                 ENDDO
                 IF (.NOT. FOUND) THEN
                    CALL ANCMSG(MSGID = 120, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                   C1 = 'IN /INIMAP1D OPTION', I1 = IFUNC3)
                 ENDIF
              ELSE
                 INIMAP1D(KK)%FUNC_ALPHA(LL) = 0
              ENDIF
              IF (IFUNC2 > 0) THEN
                 JJ = 0
                 FOUND = .FALSE.
                 DO II = NFUNCT + 2, 2 * NFUNCT + 1
                    JJ = JJ + 1
                    IF (NPC(II) == IFUNC2) THEN
                       INIMAP1D(KK)%FUNC_RHO(LL) = JJ
                       FOUND = .TRUE.
                       EXIT
                    ENDIF
                 ENDDO
                 IF (.NOT. FOUND) THEN
                    CALL ANCMSG(MSGID = 120, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                   C1 = 'IN /INIMAP1D OPTION', I1 = IFUNC3)
                 ENDIF
              ELSE
                 INIMAP1D(KK)%FUNC_RHO(LL) = 0
              ENDIF
              IF (IFUNC3 > 0) THEN
                 JJ = 0
                 FOUND = .FALSE.
                 DO II = NFUNCT + 2, 2 * NFUNCT + 1
                    JJ = JJ + 1
                    IF (NPC(II) == IFUNC3) THEN
                       IF (INIMAP1D(KK)%FORMULATION == 1) THEN
                          INIMAP1D(KK)%FUNC_PRES(LL) = JJ
                          INIMAP1D(KK)%FUNC_ENER(LL) = 0
                       ENDIF
                       IF (INIMAP1D(KK)%FORMULATION == 2) THEN
                          INIMAP1D(KK)%FUNC_ENER(LL) = JJ
                          INIMAP1D(KK)%FUNC_PRES(LL) = 0
                       ENDIF
                       FOUND = .TRUE.
                       EXIT
                    ENDIF
                 ENDDO
                 IF (.NOT. FOUND) THEN
                    CALL ANCMSG(MSGID = 120, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .                   C1 = 'IN /INIMAP1D OPTION', I1 = IFUNC3)
                 ENDIF
              ELSE
                 INIMAP1D(KK)%FUNC_PRES(LL) = 0
                 INIMAP1D(KK)%FUNC_ENER(LL) = 0
              ENDIF
              IF (FAC1 == ZERO) FAC1 = ONE
              IF (FAC2 == ZERO) FAC2 = ONE
              INIMAP1D(KK)%FAC_RHO(LL) = FAC1
              INIMAP1D(KK)%FAC_PRES_ENER(LL) = FAC2
              IF (INIMAP1D(KK)%FORMULATION == 1) THEN
                 WRITE(IOUT, 2060) LL, IFUNC1, IFUNC2, FAC1, IFUNC3, FAC2
              ENDIF
              IF (INIMAP1D(KK)%FORMULATION == 2) THEN
                 WRITE(IOUT, 2050) LL, IFUNC1, IFUNC2, FAC1, IFUNC3, FAC2
              ENDIF
           ENDDO

         ELSEIF(INIMAP1D(KK)%FILE)THEN
            CALL HM_GET_STRING('Filename', FILENAME, ncharline, IS_AVAILABLE)
            WRITE(IOUT, 2026)TRIM(FILENAME)
            CALL LEC_INIMAP1D_FILE(INIMAP1D(KK), FILENAME, ID, TITR)
         ENDIF

         IF (INIMAP1D(KK)%NODEID1 > 0) THEN
            LNODID1 = USR2SYS(INIMAP1D(KK)%NODEID1, ITABM1, MESS, INIMAP1D(KK)%NODEID1)
            INIMAP1D(KK)%NODEID1 = LNODID1
         ENDIF
         IF (INIMAP1D(KK)%NODEID2 > 0) THEN
            LNODID2 = USR2SYS(INIMAP1D(KK)%NODEID2, ITABM1, MESS, INIMAP1D(KK)%NODEID2)
            INIMAP1D(KK)%NODEID2 = LNODID2
         ENDIF
         IF (INIMAP1D(KK)%PROJ == 1) THEN
            X0 = X(1, LNODID1)
            Y0 = X(2, LNODID1)
            Z0 = X(3, LNODID1)
            X1 = X(1, LNODID2)
            Y1 = X(2, LNODID2)
            Z1 = X(3, LNODID2)
            NORM = SQRT((X1 - X0) * (X1 - X0) + (Y1 - Y0) * (Y1 - Y0) +
     .           (Z1 - Z0) * (Z1 - Z0))
            INIMAP1D(KK)%NX = (X1 - X0) / NORM
            INIMAP1D(KK)%NY = (Y1 - Y0) / NORM
            INIMAP1D(KK)%NZ = (Z1 - Z0) / NORM
            IF(N2D /=0 )INIMAP1D(KK)%NX = ZERO  !force to 0.00 instead of epsilon : Y,Z only in 2D
         ENDIF

      ENDDO ! KK = 1, NINIMAP1D
      RETURN

 998  CALL ANCMSG(MSGID = 1892, ANMODE = ANINFO, MSGTYPE = MSGERROR,I1=INIMAP1D(KK)%ID, C1=INIMAP1D(KK)%TITLE,C2=MSG_DESCRIPTION)
      CALL FREERR(3)
      RETURN

 999  CALL FREERR(3)

      RETURN
 2000 FORMAT(//
     .'     1D INITIAL MAPPING (/INIMAP1D)  '/
     .'     ------------------------------  ')
 2001 FORMAT('                 ID : ', 1X, I10)
 2002 FORMAT('              TITLE : ', A)
 2010 FORMAT('        FORMULATION : VP (INITIALIZATION FROM DENSITY AND PRESSURE FUNCTIONS)')
 2020 FORMAT('        FORMULATION : VE (INITIALIZATION FROM DENSITY AND SPECIFIC EINT FUNCTIONS)')
 2025 FORMAT('        FORMULATION : FILE (INITIALIZATION FROM STATE FILE)')
 2026 FORMAT('           FILENAME : ', A)
 2030 FORMAT('       MAPPING TYPE : ', A)
 2040 FORMAT('      --VELOCITY FUNCT ID, SCALE FACTOR: ', I10, 1PG20.13)
 2050 FORMAT('    PHASE ', I10,
     .     /, '     VOLUME FRACTION FUNCT ID: ', I10,
     .     /, '     MASS DENSITY FUNCT ID, SCALE FACTOR: ', I10, 1PG20.13,
     .     /, '     SPECIFIC ENERGY FUNCT ID, SCALE FACTOR: ', I10, 1PG20.13)
 2060 FORMAT('    PHASE ', I10,
     .     /, '     VOLUME FRACTION FUNCT ID: ', I10,
     .     /, '     MASS DENSITY FUNCT ID, SCALE FACTOR: ', I10, 1PG20.13,
     .     /, '     PRESSURE FUNCT ID, SCALE FACTOR: ', I10, 1PG20.13)
      END SUBROUTINE HM_READ_INIMAP1D
