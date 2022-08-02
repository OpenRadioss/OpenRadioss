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
Chd|  LEC_INIMAP2D_FILE             source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|-- called by -----------
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CLOSE_C                       source/output/tools/write_routines.c
Chd|        CUR_FIL_C                     source/output/tools/write_routines.c
Chd|        READ_DB                       source/restart/ddsplit/wrrest.F
Chd|        READ_I_C                      source/output/tools/write_routines.c
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LEC_INIMAP2D_FILE(INIMAP2D, FILENAME, ID, TITLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------   
      USE INIMAP2D_MOD
      USE FUNC2D_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "com04_c.inc" 
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INIMAP2D_STRUCT), INTENT(INOUT) :: INIMAP2D
      CHARACTER*ncharline, INTENT(INOUT) :: FILENAME
      INTEGER, INTENT(IN) :: ID
      CHARACTER TITLE*nchartitle
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*40,BUFFER*14,ROOTNAME*ncharline
      DATA MESS/'INFILE'/
      INTEGER NUM_CENTROIDS,N_CYCLE,K,ISUBMAT,NBMAT,NUM_NODE_VEL,INPUT_VERSION,NB_COMP
      my_real X_SCALE,Y_SCALE,X_SHIFT,Y_SHIFT,TT
      INTEGER :: LEN, LEN_TMP_NAME
      CHARACTER :: TMP_NAME*2048, FILNAM*204
      INTEGER :: IFILNAM(2048),II, NCYCLE, NCELL_TOT, INIVERS
      LOGICAL FILE_EXIST
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        LEN = LEN_TRIM(FILENAME)
        FILNAM=FILENAME(:LEN)
        IF(LEN >= 3)THEN
          IF( FILNAM(LEN-2:LEN) == ".gz")THEN
            FILNAM(LEN-2:LEN)=' '
            LEN=LEN-3
          ENDIF
        ENDIF
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN
        !TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN)
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
            CALL ANCMSG(MSGID = 1885, ANMODE = ANINFO, MSGTYPE = MSGERROR, I1 = ID, C1=TITLE)
            INIMAP2D%CORRECTLY_READ = .FALSE.
            return
           ENDIF
        ENDIF 
                      
        CALL READ_I_C(INIVERS,1)
        CALL READ_DB(TT,1)
        CALL READ_I_C(NCYCLE,1)
        CALL READ_I_C(NUM_CENTROIDS,1)
        CALL READ_I_C(NUM_NODE_VEL,1)
        CALL READ_I_C(NBMAT,1)   
        
          IF(.NOT.ALLOCATED(INIMAP2D%SUBMAT))ALLOCATE(INIMAP2D%SUBMAT(NBMAT))                   

          !---ABSCISSA
          IF(.NOT.ALLOCATED(INIMAP2D%XVAL))ALLOCATE(INIMAP2D%XVAL(2,NUM_CENTROIDS))
          DO K=1,NUM_CENTROIDS
            CALL READ_DB(INIMAP2D%XVAL(1,K),1)
          ENDDO
          DO K=1,NUM_CENTROIDS
            CALL READ_DB(INIMAP2D%XVAL(2,K),1)
          ENDDO
          
          !---VOLUME FRACTION
          DO ISUBMAT=1,NBMAT                  
            IF(.NOT.ALLOCATED(INIMAP2D%SUBMAT(ISUBMAT)%VFRAC))ALLOCATE(INIMAP2D%SUBMAT(ISUBMAT)%VFRAC(1,NUM_CENTROIDS))
            DO K=1, NUM_CENTROIDS  
              CALL READ_DB(INIMAP2D%SUBMAT(ISUBMAT)%VFRAC(1,K),1)        
            ENDDO 
          ENDDO                                                                                     

          !---DENSITIES
          DO ISUBMAT=1,NBMAT                  
            IF(.NOT.ALLOCATED(INIMAP2D%SUBMAT(ISUBMAT)%RHO))ALLOCATE(INIMAP2D%SUBMAT(ISUBMAT)%RHO(1,NUM_CENTROIDS))
            DO K=1, NUM_CENTROIDS  
              CALL READ_DB(INIMAP2D%SUBMAT(ISUBMAT)%RHO(1,K),1)        
            ENDDO 
          ENDDO                                                                                                  

          !---PRESSURE
          DO ISUBMAT=1,NBMAT                  
            IF(.NOT.ALLOCATED(INIMAP2D%SUBMAT(ISUBMAT)%PRES))ALLOCATE(INIMAP2D%SUBMAT(ISUBMAT)%PRES(1,NUM_CENTROIDS))
            DO K=1, NUM_CENTROIDS  
              CALL READ_DB(INIMAP2D%SUBMAT(ISUBMAT)%PRES(1,K),1)        
            ENDDO                                                                                  
          ENDDO  
 
          !---VELOCITY
          IF(.NOT.ALLOCATED(INIMAP2D%XVAL_V))ALLOCATE(INIMAP2D%XVAL_V(2,NUM_NODE_VEL))
          IF(.NOT.ALLOCATED(INIMAP2D%VEL))ALLOCATE(INIMAP2D%VEL(2,NUM_NODE_VEL))
          IF(NUM_NODE_VEL == NUM_CENTROIDS)THEN
            DO K=1, NUM_NODE_VEL 
              CALL READ_DB( INIMAP2D%VEL(1,K), 1)
              INIMAP2D%XVAL_V(1,K) = INIMAP2D%XVAL(1,K)    
            ENDDO    
            DO K=1, NUM_NODE_VEL 
              CALL READ_DB( INIMAP2D%VEL(2,K), 1)
              INIMAP2D%XVAL_V(2,K) = INIMAP2D%XVAL(2,K)    
            ENDDO   
           ELSE
             DO K=1, NUM_NODE_VEL ; CALL READ_DB( INIMAP2D%XVAL_V(1,K), 1) ; ENDDO 
             DO K=1, NUM_NODE_VEL ; CALL READ_DB( INIMAP2D%XVAL_V(2,K), 1) ; ENDDO 
             DO K=1, NUM_NODE_VEL ; CALL READ_DB( INIMAP2D%VEL(1,K), 1) ; ENDDO 
             DO K=1, NUM_NODE_VEL ; CALL READ_DB( INIMAP2D%VEL(2,K), 1) ; ENDDO 
           ENDIF
           
        CALL CLOSE_C()

        INIMAP2D%NBMAT = NBMAT 
        INIMAP2D%NUM_CENTROIDS = NUM_CENTROIDS     
        INIMAP2D%NUM_NODE_VEL = NUM_NODE_VEL        
        INIMAP2D%FUNC_VEL = -1 
        IF(.NOT.ALLOCATED(INIMAP2D%FUNC_ALPHA))ALLOCATE(INIMAP2D%FUNC_ALPHA(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP2D%FUNC_RHO))ALLOCATE(INIMAP2D%FUNC_RHO(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP2D%FUNC_ENER))ALLOCATE(INIMAP2D%FUNC_ENER(NBMAT))                          
        IF(.NOT.ALLOCATED(INIMAP2D%FUNC_PRES))ALLOCATE(INIMAP2D%FUNC_PRES(NBMAT))  
        DO ISUBMAT=1,NBMAT   
          INIMAP2D%FUNC_ALPHA(ISUBMAT) = -1        
          INIMAP2D%FUNC_RHO(ISUBMAT) = -1        
          INIMAP2D%FUNC_ENER(ISUBMAT) = -1                        
          INIMAP2D%FUNC_PRES(ISUBMAT) = -1                        
        ENDDO

        IF(.NOT.ALLOCATED(INIMAP2D%FAC_RHO))ALLOCATE(INIMAP2D%FAC_RHO(NBMAT))          
        IF(.NOT.ALLOCATED(INIMAP2D%FAC_PRES_ENER))ALLOCATE(INIMAP2D%FAC_PRES_ENER(NBMAT))                  
        DO ISUBMAT=1,NBMAT
          INIMAP2D%FAC_RHO(ISUBMAT) = ONE
          INIMAP2D%FAC_PRES_ENER(ISUBMAT) = ONE
        ENDDO
        INIMAP2D%FAC_VEL = ONE
           
        
        RETURN
        
 999    CALL ANCMSG(MSGID = 1885, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .              I1 = INIMAP2D%ID, C1 = INIMAP2D%TITLE)

      END SUBROUTINE LEC_INIMAP2D_FILE
