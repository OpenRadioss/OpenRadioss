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
Chd|  HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        TRACE_IN1                     source/system/trace_back.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INIVOL(INIVOL , KVOL, IGRSURF ,IPART  ,MULTI_FVM, BUFMAT, IPM, ITAB, NBSUBMAT, LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "submod_c.inc"
#include      "tabsiz_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(SIPART)
      INTEGER,INTENT(IN)::IPM(NPROPMI*NUMMAT),ITAB(NUMNOD)
      my_real,INTENT(IN),TARGET :: BUFMAT(*)
      TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
      TYPE (SURF_)   , DIMENSION(NSURF+NSETS)   :: IGRSURF
      TYPE (INIVOL_)  , INTENT(INOUT), DIMENSION(:), ALLOCATABLE  :: INIVOL
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      my_real, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: KVOL
      TYPE (UNIT_TYPE_),INTENT(IN)      :: UNITAB 
      INTEGER,INTENT(INOUT) :: NBSUBMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real, DIMENSION(:) ,POINTER  :: UPARAM
      INTEGER I,J,IP,ID,IPARTFILL,JREC,IGS,PARTID,IDSURF,
     . IPHASE,IFILL,NCONT,NLIN,ISU,NN,JJ,NINTMAX,ITYP,NUMEL_TOT,
     . SRFTYP,FAC,K,ICUMU,IMAT,IADBUF,NUPARAM,ISUBMAT,STAT,KK,N_R2R
      my_real FILL_RATIO,ILAW
      CHARACTER TITR*nchartitle,MESS*40,KEY*ncharkey
      DATA MESS/'INITIAL VOLUME FRACTION                 '/
      CHARACTER*ncharline ERR_MSG
      LOGICAL IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C     INIVOL(IGS)%ID         : INITIAL VOLUME FRACTION IDENTIFIER
C     INIVOL(IGS)%TITLE      : INIVOL title
C     INIVOL(IGS)%NBCONTY    : NUMBER of INIVOL containers (SURFACES)
C     INIVOL(IGS)%IPARTFILL  : INIVOL part to be filled
C     INIVOL(IGS)%SURFCONTY(KK) :  INIVOL CONTAINER SURFACE
C     INIVOL(IGS)%CONTY(KK)%IPHASE : Phase of the multi-material ALE to fill the Part
C     INIVOL(IGS)%CONTY(KK)%ICUMU : Flag for cumulate volume filling
C     INIVOL(IGS)%CONTY(KK)%FILL_RATIO : Filling ratio:
C                                = 0 ! filling the side along normal direction 
C                                = 1 ! filling the side against normal direction
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !---OUTPUT MESSAGE & SIZES
      SKVOL = 0
      SINIVOL = 0
      ERR_MSG='INITIAL VOLUME FRACTION'                                                          
      ERR_CATEGORY='INITIAL VOLUME FRACTION'                                                     
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))                                                  
      IF (NINIVOL > 0)THEN                                                                       
        WRITE(ISTDO,'(A)')' .. INITIAL VOLUME FRACTION'                                          
        WRITE(IOUT,'(//A)')'     INITIAL VOLUME FRACTION'                                      
        WRITE(IOUT,'(A/)')'     -----------------------'  
        NBSUBMAT = 4 !(LAW51)   
        IF (MULTI_FVM%IS_USED) NBSUBMAT = MAX(NBSUBMAT, MULTI_FVM%NBMAT)  !up to 20 (LAW151)
        NUMEL_TOT = MAX(NUMELTG,MAX(NUMELS,NUMELQ))
        SKVOL = NBSUBMAT*NUMEL_TOT
        SINIVOL = NINIVOL
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
      DO IGS=1,NINIVOL
C     
        IF (NSUBDOM > 0) THEN
          N_R2R = N_R2R + 1
          IF(TAG_INIVOL(N_R2R) == 0) CALL HM_SZ_R2R(TAG_INIVOL,N_R2R,LSUBMODEL)
        ENDIF
C
        ! CALL FREDEC_KEY_2ID_T(PARTID,ID,TITR)
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
        CALL HM_GET_INTV('secondarycomponentlist', PARTID, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('NIP', NLIN, IS_AVAILABLE, LSUBMODEL)
        
        WRITE(IOUT, 1001) TITR, ID, PARTID
             
        IPARTFILL = 0
        !search PART_ID                                                                                                      
        DO J=1,NPART                                                                                                         
          IP = IPART(LIPART1*(J-1)+4)                                                                                                    
          IF(PARTID == IP)THEN                                                                                               
            IPARTFILL = J                                                                                                    
            EXIT                                                                                                             
          ENDIF                                                                                                              
        ENDDO                                                                                                                
        IF(IPARTFILL == 0)CALL ANCMSG(MSGID=886,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=PARTID)  !PART_ID not found  


        INIVOL(IGS)%ID = ID
        INIVOL(IGS)%TITLE(1:nchartitle)=' '
        INIVOL(IGS)%TITLE = TITR(1:LEN_TRIM(TITR))
        INIVOL(IGS)%NBCONTY=NLIN
        INIVOL(IGS)%IPARTFILL = IPARTFILL
        MY_ALLOCATE(INIVOL(IGS)%CONTY,NLIN)
        MY_ALLOCATE(INIVOL(IGS)%SURFCONTY,NLIN)

        WRITE(IOUT, '(A)') "     surf_ID ALE_PHASE FILL_OPT ICUMU          FILL_RATIO"
        DO KK=1,NLIN
          CALL HM_GET_INT_ARRAY_INDEX('SETSURFID_ARR', IDSURF,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_INT_ARRAY_INDEX('ALE_PHASE', IPHASE,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_INT_ARRAY_INDEX('fill_opt_arr', IFILL,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_INT_ARRAY_INDEX('ICUMU', ICUMU,KK, IS_AVAILABLE, LSUBMODEL)         
          CALL HM_GET_FLOAT_ARRAY_INDEX('FILL_RATIO', FILL_RATIO, KK, IS_AVAILABLE, LSUBMODEL,UNITAB)  

          WRITE(IOUT, '(2X,I10,I10,I9,I6,F20.0)')    IDSURF,IPHASE, IFILL, ICUMU, FILL_RATIO        

          !IF(FLAG == 0)THEN
            IF((IPHASE < 0 .OR. IPHASE > 4).AND.  .NOT.MULTI_FVM%IS_USED )THEN
              CALL ANCMSG(MSGID=887,MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=ID,C1=TITR)
            ENDIF
            IF(IFILL < 0 .OR. IFILL > 1)THEN
              CALL ANCMSG(MSGID=888,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
            ENDIF
            IF(FILL_RATIO < ZERO .or. FILL_RATIO > ONE)THEN
              CALL ANCMSG(MSGID=1596,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
            ENDIF
          !ENDIF
                                                                                                  
          IMAT = IPART(LIPART1*(IPARTFILL-1)+1)  
          ILAW=   IPM((IMAT-1)*NPROPMI + 2)     !IPM(2,IMAT)
          IF(ILAW/=51 .AND. ILAW/=151)THEN
            CALL ANCMSG(MSGID=821, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)             
          ENDIF   

          !!get bijective application to retrieve internal order of submaterial.
          !! (internally & historically phase4 is explosive submaterial)
          IF(ILAW==51)THEN                    
            IADBUF = IPM((IMAT-1)*NPROPMI + 7)!IPM(7,IMAT)                       
            NUPARAM= IPM((IMAT-1)*NPROPMI + 9)!IPM(9,IMAT)                       
            UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)          
            IPHASE=UPARAM(276+IPHASE)
          ENDIF
          
          IF(FILL_RATIO==ZERO)FILL_RATIO = ONE
          IF(ICUMU/=0 .AND. ICUMU/=1)THEN
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
            IF(SRFTYP == 101) FAC = FAC + 1                                                                         
            IF(SRFTYP == 200) FAC = FAC + 1                                                
            !---   planar surface                                                          
            IF(FAC /= 1)THEN                                                               
              DO J=1,NN                                                                    
                ITYP=IGRSURF(ISU)%ELTYP(J)                                                 
                IF(ITYP==3 .or. ITYP==7) FAC = FAC + 1                                 
              ENDDO                                                                        
            ENDIF                                                                          
            !---   discretized surface                                                     
            INIVOL(IGS)%SURFCONTY(KK) = ISU                                     
            INIVOL(IGS)%CONTY(KK)%IPHASE = IPHASE                               
            INIVOL(IGS)%CONTY(KK)%IFILL = IFILL                                 
            INIVOL(IGS)%CONTY(KK)%FILL_RATIO = INT(FILL_RATIO*EP9)              
            INIVOL(IGS)%CONTY(KK)%ICUMU = ICUMU                                 
C
            IF(N2D == 0 .AND. FAC == 0)THEN                                                               
              CALL ANCMSG(MSGID=890,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)          
            ELSEIF(N2D > 0 .AND. SRFTYP /=101 .AND. SRFTYP /= 200)THEN
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
     .     5X,'INIVOL TITLE . . . . . . . . . . . . .=',A/,
     .     5X,'INIVOL IDENTFIER . . . . . . . . . . .=',I10/,     
     .     5X,'PART IDENTIFIER. . . . . . . . . . . .=',I10)

      RETURN
      END
      

