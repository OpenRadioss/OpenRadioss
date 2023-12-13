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
Chd|  LEC_INIMAP1D_FILE             source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|-- called by -----------
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CLOSE_C                       ../common_source/tools/input_output/write_routtines.c
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        OPEN_C                        ../common_source/tools/input_output/write_routtines.c
Chd|        READ_DB                       source/restart/ddsplit/wrrest.F
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE LEC_INIMAP1D_FILE(INIMAP1D, FILENAME, ID, TITLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------   
      USE INIMAP1D_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE INOUTFILE_MOD            
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INIMAP1D_STRUCT), INTENT(INOUT) :: INIMAP1D
      CHARACTER*ncharline,INTENT(INOUT) :: FILENAME
      INTEGER, INTENT(IN) :: ID
      CHARACTER TITLE*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*40,BUFFER*14,ROOTNAME*ncharline
      DATA MESS/'INFILE'/
      INTEGER NUM_CENTROIDS,N_CYCLE,K,ISUBMAT,NBMAT,NUM_NODE_VEL,INPUT_VERSION
      my_real X_SCALE,Y_SCALE,X_SHIFT,Y_SHIFT,TT
      INTEGER PREV, NEXT
      INTEGER :: LEN, LEN_TMP_NAME
      CHARACTER :: TMP_NAME*2048, FILNAM*2048
      INTEGER :: IFILNAM(2048),II, NCYCLE, NCELL_TOT, INIVERS
      LOGICAL FILE_EXIST
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        LEN = LEN_TRIM(FILENAME)
        FILNAM=FILENAME(1:LEN)
        IF(LEN >= 3)THEN
          IF( FILNAM(LEN-2:LEN) == ".gz")THEN
            FILNAM(LEN-2:LEN)='   '
            LEN=LEN-3
          ENDIF
        ENDIF
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN)
        TMP_NAME=FILNAM(1:LEN)
        DO II=1,LEN_TMP_NAME
          IFILNAM(II)=ICHAR(TMP_NAME(II:II))
        END DO
        CALL CUR_FIL_C(IUINIMAP)
        CALL OPEN_C(IFILNAM,LEN_TMP_NAME,7)
        FILE_EXIST=.FALSE.
        inquire( file=trim(FILENAME), exist=FILE_EXIST )
        IF(.NOT.FILE_EXIST) THEN
          inquire( file=trim(FILENAME)//".gz", exist=FILE_EXIST )
          IF(.NOT.FILE_EXIST) THEN
            CALL ANCMSG(MSGID = 1884, ANMODE = ANINFO, MSGTYPE = MSGERROR, I1 = ID, C1=TITLE)
            INIMAP1D%CORRECTLY_READ = .FALSE.
            return
           ENDIF
        ENDIF    
              
        CALL READ_I_C(INIVERS,1)
        CALL READ_DB(TT,1)
        CALL READ_I_C(NCYCLE,1)
        CALL READ_I_C(NUM_CENTROIDS,1)
        CALL READ_I_C(NUM_NODE_VEL,1)
        CALL READ_I_C(NBMAT,1) 
        
          IF(.NOT.ALLOCATED(INIMAP1D%SUBMAT))ALLOCATE(INIMAP1D%SUBMAT(NBMAT))                   

          !---ABSCISSA
          IF(.NOT.ALLOCATED(INIMAP1D%X))ALLOCATE(INIMAP1D%X(NUM_CENTROIDS))
          CALL READ_DB( INIMAP1D%X(1),NUM_CENTROIDS)
          CALL READ_DB( X_SHIFT,1)                                                                                    
          !MOVE_FUNCTION
          DO K=1,NUM_CENTROIDS
            INIMAP1D%X(K) = INIMAP1D%X(K)+X_SHIFT
          ENDDO               
          
          !---VOLUME FRACTION
          DO ISUBMAT=1,NBMAT                   
            IF(.NOT.ALLOCATED(INIMAP1D%SUBMAT(ISUBMAT)%VFRAC))ALLOCATE(INIMAP1D%SUBMAT(ISUBMAT)%VFRAC(NUM_CENTROIDS))
            CALL READ_DB( INIMAP1D%SUBMAT(ISUBMAT)%VFRAC(1),NUM_CENTROIDS)
          ENDDO  

          !---DENSITIES
          DO ISUBMAT=1,NBMAT                  
            IF(.NOT.ALLOCATED(INIMAP1D%SUBMAT(ISUBMAT)%RHO))ALLOCATE(INIMAP1D%SUBMAT(ISUBMAT)%RHO(NUM_CENTROIDS))
            CALL READ_DB( INIMAP1D%SUBMAT(ISUBMAT)%RHO(1),NUM_CENTROIDS)       
          ENDDO            

          !---PRESSURE
          DO ISUBMAT=1,NBMAT                  
            IF(.NOT.ALLOCATED(INIMAP1D%SUBMAT(ISUBMAT)%PRES))ALLOCATE(INIMAP1D%SUBMAT(ISUBMAT)%PRES(NUM_CENTROIDS))
            CALL READ_DB( INIMAP1D%SUBMAT(ISUBMAT)%PRES(1),NUM_CENTROIDS)
          ENDDO  
 
          !---VELOCITY
          IF(.NOT.ALLOCATED(INIMAP1D%VEL))ALLOCATE(INIMAP1D%VEL(NUM_NODE_VEL))
          IF(.NOT.ALLOCATED(INIMAP1D%X_VEL))ALLOCATE(INIMAP1D%X_VEL(NUM_NODE_VEL))
          IF(NUM_NODE_VEL == NUM_CENTROIDS)THEN
            CALL READ_DB( INIMAP1D%VEL(1),NUM_NODE_VEL )
            DO K=1, NUM_NODE_VEL  
              INIMAP1D%X_VEL(K) = INIMAP1D%X(K)       
            ENDDO
          ELSE
            CALL READ_DB( INIMAP1D%X_VEL(1),NUM_NODE_VEL )
            CALL READ_DB( INIMAP1D%VEL(1),NUM_NODE_VEL )
          ENDIF 
           
        CALL CLOSE_C()

        !check abscissa
        ! not needed : order already correct when it is built
        !PREV = INIMAP1D%X(1)
        !DO K=2,NUM_CENTROIDS-1
        !  NEXT = INIMAP1D%X(K)
        !  IF(PREV <= F)THEN
        !      CALL ANCMSG(MSGID = 1884, ANMODE = ANINFO, MSGTYPE = MSGERROR, I1 = INIMAP1D%ID, C1=INIMAP1D%TITLE)
        !    EXIT
        !  ENDIF
        !ENDDO

        INIMAP1D%NBMAT = NBMAT 
        INIMAP1D%NUM_CENTROIDS = NUM_CENTROIDS     
        INIMAP1D%NUM_NODE_VEL = NUM_NODE_VEL        
        INIMAP1D%FUNC_VEL = -1 
        IF(.NOT.ALLOCATED(INIMAP1D%FUNC_ALPHA))ALLOCATE(INIMAP1D%FUNC_ALPHA(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP1D%FUNC_RHO))ALLOCATE(INIMAP1D%FUNC_RHO(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP1D%FUNC_ENER))ALLOCATE(INIMAP1D%FUNC_ENER(NBMAT))                          
        IF(.NOT.ALLOCATED(INIMAP1D%FUNC_PRES))ALLOCATE(INIMAP1D%FUNC_PRES(NBMAT))                          
        DO ISUBMAT=1,NBMAT   
          INIMAP1D%FUNC_ALPHA(ISUBMAT) = -1        
          INIMAP1D%FUNC_RHO(ISUBMAT)   = -1        
          INIMAP1D%FUNC_ENER(ISUBMAT)  = -1                        
          INIMAP1D%FUNC_PRES(ISUBMAT)  = -1                        
        ENDDO

        IF(.NOT.ALLOCATED(INIMAP1D%FAC_RHO))ALLOCATE(INIMAP1D%FAC_RHO(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP1D%FAC_PRES_ENER))ALLOCATE(INIMAP1D%FAC_PRES_ENER(NBMAT))                  
        DO ISUBMAT=1,NBMAT
          INIMAP1D%FAC_RHO(ISUBMAT) = ONE
          INIMAP1D%FAC_PRES_ENER(ISUBMAT) = ONE
        ENDDO
        INIMAP1D%FAC_VEL = ONE
           
        
        RETURN
        
 999             CALL ANCMSG(MSGID = 1884, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I1 = INIMAP1D%ID, C1=INIMAP1D%TITLE)
  

      END SUBROUTINE LEC_INIMAP1D_FILE
