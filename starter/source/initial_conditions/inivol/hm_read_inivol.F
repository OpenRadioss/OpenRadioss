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
      !||====================================================================
      !||    hm_read_inivol             ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||--- called by ------------------------------------------------------
      !||    lectur                     ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    hm_sz_r2r                  ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    trace_in1                  ../starter/source/system/trace_back.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
      !||    inivol_def_mod             ../starter/share/modules1/inivol_mod.F
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    r2r_mod                    ../starter/share/modules1/r2r_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_INIVOL(INIVOL, KVOL, IGRSURF, IPART, MULTI_FVM, BUFMAT, IPM, NBSUBMAT, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE INIVOL_DEF_MOD , ONLY : INIVOL_STRUCT_, NUM_INIVOL, SKVOL
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE R2R_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(SIPART)
      INTEGER,INTENT(IN)::IPM(NPROPMI*NUMMAT)
      my_real,INTENT(IN),TARGET :: BUFMAT(*)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      TYPE (SURF_), DIMENSION(NSURF+NSETS)   :: IGRSURF
      TYPE (INIVOL_STRUCT_), INTENT(INOUT), DIMENSION(:), ALLOCATABLE  :: INIVOL
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      my_real, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: KVOL
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB
      INTEGER,INTENT(INOUT) :: NBSUBMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IP,ID,PART_ID,JREC,IGS,PART_USER_ID,IDSURF
      INTEGER SUBMAT_ID,IREVERSED,NCONT,NLIN,ISU,NN,JJ,NINTMAX,ITYP,NUMEL_TOT
      INTEGER SRFTYP,FAC,K,ICUMU,IMAT,IADBUF,NUPARAM,ISUBMAT,STAT,KK,N_R2R
      my_real VFRAC,ILAW
      my_real, DIMENSION(:),POINTER :: UPARAM
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER MESS*40
      CHARACTER(LEN=NCHARKEY) :: KEY
      CHARACTER(LEN=NCHARLINE) ERR_MSG
      DATA MESS/'INITIAL VOLUME FRACTION                 '/
      LOGICAL IS_ENCRYPTED,IS_AVAILABLE
      LOGICAL DETECTED_ERROR
      LOGICAL :: REQUIRED_2D_POLYGON_CLIPPING ! 2d polygons
      LOGICAL :: REQUIRED_MONTE_CARLO_METHOD  ! 3d discretized surface, infinita plane, ellipsoids
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C     INIVOL(IGS)%ID                      : INITIAL VOLUME FRACTION IDENTIFIER
C     INIVOL(IGS)%TITLE                   : INIVOL title
C     INIVOL(IGS)%NUM_CONTAINER           : NUMBER of INIVOL containers (SURFACES)
C     INIVOL(IGS)%PART_ID                 : INIVOL part to be filled
C     INIVOL(IGS)%CONTAINER(KK)%SURF_ID   : INIVOL CONTAINER SURFACE
C     INIVOL(IGS)%CONTAINER(KK)%SUBMAT_ID : Submat identifier of the multi-material ALE to fill the Part
C     INIVOL(IGS)%CONTAINER(KK)%ICUMU     : Flag for cumulate volume filling
C     INIVOL(IGS)%CONTAINER(KK)%VFRAC     : Filling ratio:
C                                          = 0 ! filling the side along normal direction
C                                          = 1 ! filling the side against normal direction
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !---OUTPUT MESSAGE & SIZES
      SKVOL = 0
      SINIVOL = 0
      ERR_MSG='INITIAL VOLUME FRACTION'                                                          
      ERR_CATEGORY='INITIAL VOLUME FRACTION'                                                     
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))                                                  
      IF (NUM_INIVOL > 0)THEN                                                                       
        WRITE(ISTDO,'(A)')' .. INITIAL VOLUME FRACTION'                                          
        WRITE(IOUT,'(//A)')'     INITIAL VOLUME FRACTION'                                      
        WRITE(IOUT,'(A/)') '     -----------------------'
        NBSUBMAT = 4 !(LAW51)   
        IF (MULTI_FVM%IS_USED) NBSUBMAT = MAX(NBSUBMAT, MULTI_FVM%NBMAT)  !up to 20 (LAW151)
        NUMEL_TOT = MAX(NUMELTG,MAX(NUMELS,NUMELQ))
        SKVOL = NBSUBMAT*NUMEL_TOT
        SINIVOL = NUM_INIVOL
      ENDIF    
      
      !---ALLOCATION
      IF(.NOT.ALLOCATED(INIVOL))ALLOCATE (INIVOL(SINIVOL) ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='INIVOL')

      IF(.NOT.ALLOCATED(KVOL))ALLOCATE (KVOL(SKVOL)     ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR,C1='KVOL')
      IF (SKVOL > 0) KVOL  = 0

      !---READ CARDS
      CALL HM_OPTION_START('/INIVOL')
      IS_ENCRYPTED= .FALSE.                                                                  
      IS_AVAILABLE = .FALSE.                                                                 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)                                                                                             
      IGS=0
      NINTMAX = 3
      N_R2R = 0
C
      DO IGS=1,NUM_INIVOL
C     
        IF (NSUBDOM > 0) THEN
          N_R2R = N_R2R + 1
          IF(TAG_INIVOL(N_R2R) == 0) CALL HM_SZ_R2R(TAG_INIVOL,N_R2R,LSUBMODEL)
        ENDIF

        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
        CALL HM_GET_INTV('secondarycomponentlist', PART_USER_ID, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('NIP', NLIN, IS_AVAILABLE, LSUBMODEL)
        
        WRITE(IOUT, 1001) TITR, ID, PART_USER_ID
             
        PART_ID = 0
        !search PART_ID                                                                                                      
        DO J=1,NPART                                                                                                         
          IP = IPART(LIPART1*(J-1)+4)                                                                                                    
          IF(PART_USER_ID == IP)THEN
            PART_ID = J
            EXIT                                                                                                             
          ENDIF                                                                                                              
        ENDDO                                                                                                                
        IF(PART_ID == 0)CALL ANCMSG(MSGID=886,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=PART_USER_ID)  !PART_ID not found

        INIVOL(IGS)%ID = ID
        INIVOL(IGS)%TITLE(1:nchartitle)=' '
        INIVOL(IGS)%TITLE = TITR(1:LEN_TRIM(TITR))
        INIVOL(IGS)%NUM_CONTAINER = NLIN
        INIVOL(IGS)%PART_ID = PART_ID
        INIVOL(IGS)%REQUIRED_MONTE_CARLO_METHOD = .FALSE.
        INIVOL(IGS)%REQUIRED_2D_POLYGON_CLIPPING = .FALSE.
        ALLOCATE(INIVOL(IGS)%CONTAINER(NLIN))

        WRITE(IOUT, '(A)') "     surf_ID SUBMAT_ID IREVERSED     ICUMU               VFRAC"
        DO KK=1,NLIN
          CALL HM_GET_INT_ARRAY_INDEX('SETSURFID_ARR', IDSURF,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_INT_ARRAY_INDEX('ALE_PHASE', SUBMAT_ID,KK, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('fill_opt_arr', IREVERSED,KK, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('ICUMU', ICUMU,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_FLOAT_ARRAY_INDEX('FILL_RATIO', VFRAC, KK, IS_AVAILABLE, LSUBMODEL,UNITAB)

          WRITE(IOUT,'(2X,I10,I10,I9,I6,F20.0)')IDSURF,SUBMAT_ID,IREVERSED,ICUMU,VFRAC

          IF((SUBMAT_ID <= 0 .OR. SUBMAT_ID > 4).AND.  .NOT.MULTI_FVM%IS_USED )THEN
            CALL ANCMSG(MSGID=887,MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=ID,C1=TITR)
          ENDIF
          IF(IREVERSED < 0 .OR. IREVERSED > 1)THEN
            CALL ANCMSG(MSGID=888,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
          ENDIF
          IF(VFRAC < ZERO .or. VFRAC > ONE)THEN
            CALL ANCMSG(MSGID=1596,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
          ENDIF

          IMAT = IPART(LIPART1*(PART_ID-1)+1)
          ILAW=   IPM((IMAT-1)*NPROPMI + 2)     !IPM(2,IMAT)
          IF(ILAW/=51 .AND. ILAW/=151)THEN
            CALL ANCMSG(MSGID=821, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)             
          ENDIF   

          !!get bijective application to retrieve internal order of submaterial.
          !! (internally & historically submat4 is explosive submaterial)
          IF(ILAW == 51)THEN
            IADBUF = IPM((IMAT-1)*NPROPMI + 7)!IPM(7,IMAT)                       
            NUPARAM= IPM((IMAT-1)*NPROPMI + 9)!IPM(9,IMAT)                       
            UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)          
            SUBMAT_ID=UPARAM(276+SUBMAT_ID)
          ENDIF
          
          IF(VFRAC == ZERO)VFRAC=ONE
          IF(ICUMU /= 0 .AND. ICUMU /= 1)THEN
            ICUMU = 0
          ENDIF
          ISU=0
          NN =0
          DO J=1,NSURF                                                                     
            IF (IDSURF == IGRSURF(J)%ID) THEN                                              
              ISU=J                                                                        
              NN = IGRSURF(ISU)%NSEG                                                       
              EXIT                                                                         
            END IF                                                                         
          ENDDO                                                                            
          FAC = 0                                                                          
          IF (ISU > 0) THEN                                                                
            SRFTYP=IGRSURF(ISU)%TYPE
            IF(SRFTYP == 101 .OR. SRFTYP == 200) THEN
              FAC = 1
            ELSE
              DO J=1,NN
                ITYP=IGRSURF(ISU)%ELTYP(J)
                IF(ITYP==0 .or. ITYP==3 .or. ITYP==7) FAC = FAC + 1
              ENDDO
            ENDIF

            REQUIRED_2D_POLYGON_CLIPPING = .FALSE.
            REQUIRED_MONTE_CARLO_METHOD = .FALSE.
            IF(ISU > 0 .AND. NN > 1 .AND. N2D /= 0)THEN
              !TEST SURFACE CLOSURE (CLOSED POLYGON)
              IF(IGRSURF(ISU)%NODES(1,1) /= IGRSURF(ISU)%NODES(NN,2)) THEN
                CALL ANCMSG(MSGID=3063,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,I2=IGRSURF(ISU)%ID,C1=TITR)
              ENDIF
              !TEST POLYGON DEFINITION --- (last point of current segment is first point of next one : otherwise set DETECTED_ERROR to .TRUE.
              DETECTED_ERROR=.FALSE.
              DO J=1,NN-1
                IF(IGRSURF(ISU)%NODES(J,2) /= IGRSURF(ISU)%NODES(J+1,1))THEN
                  DETECTED_ERROR = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(DETECTED_ERROR)THEN
                CALL ANCMSG(MSGID=3064,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,I2=IGRSURF(ISU)%ID,C1=TITR)
              ENDIF
              REQUIRED_2D_POLYGON_CLIPPING = .TRUE.
            ELSEIF(ISU > 0)THEN
              REQUIRED_MONTE_CARLO_METHOD = .TRUE.
              IF(ISU > 0 .AND. NN <= 2 .AND. N2D /= 0 .AND. SRFTYP == 0)THEN
                CALL ANCMSG(MSGID=3064,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,I2=IGRSURF(ISU)%ID,C1=TITR) !not enough points to define a polygon
              ENDIF
            ENDIF
            !---   discretized surface                                                     
            INIVOL(IGS)%CONTAINER(KK)%SURF_ID = ISU
            INIVOL(IGS)%CONTAINER(KK)%SUBMAT_ID = SUBMAT_ID
            INIVOL(IGS)%CONTAINER(KK)%IREVERSED = IREVERSED
            INIVOL(IGS)%CONTAINER(KK)%VFRAC = INT(VFRAC*EP9)
            INIVOL(IGS)%CONTAINER(KK)%ICUMU = ICUMU
            INIVOL(IGS)%CONTAINER(KK)%REQUIRED_MONTE_CARLO_METHOD = REQUIRED_MONTE_CARLO_METHOD
            INIVOL(IGS)%CONTAINER(KK)%REQUIRED_2D_POLYGON_CLIPPING = REQUIRED_2D_POLYGON_CLIPPING
            IF(REQUIRED_MONTE_CARLO_METHOD)  INIVOL(IGS)%REQUIRED_MONTE_CARLO_METHOD = .TRUE.
            IF(REQUIRED_2D_POLYGON_CLIPPING) INIVOL(IGS)%REQUIRED_2D_POLYGON_CLIPPING = .TRUE.
C
            IF(N2D == 0 .AND. FAC == 0)THEN                                                               
              CALL ANCMSG(MSGID=890,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)          
            ELSEIF(N2D > 0 .AND. SRFTYP /=0 .AND. SRFTYP /=101 .AND. SRFTYP /= 200)THEN
              CALL ANCMSG(MSGID=2012,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR) 
            ENDIF                                                                          
          ELSE  ! ISU == 0                                                                 
            CALL ANCMSG(MSGID=889,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=IDSURF)  
          ENDIF 
        ENDDO   !next line 
        WRITE(IOUT,'(A//)') 
      END DO !next option
  
      NBSUBMAT = 4
      IF (MULTI_FVM%IS_USED)NBSUBMAT = MAX(NBSUBMAT, MULTI_FVM%NBMAT)
             
C-----------------------------
      RETURN
C-----------------------------
 1001 FORMAT(
     .     5X,'TITLE  . . . . . . . . . . . . . . . .=',A/,
     .     5X,'IDENTFIER (ID) . . . . . . . . . . . .=',I10/,
     .     5X,'PART IDENTIFIER. . . . . . . . . . . .=',I10)

      RETURN
      END
      

