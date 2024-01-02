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
Chd|  SPMD_STATE_INIMAP2D_EXCH_DATA source/output/sta/spmd_state_inimap2d_exch_data.F
Chd|-- called by -----------
Chd|        STAT_INIMAP2D_FILE_SPMD       source/output/sta/stat_inimap2d_file_spmd.F
Chd|        STAT_INIMAP2D_SPMD            source/output/sta/stat_inimap2d_spmd.F
Chd|-- calls ---------------
Chd|        STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|====================================================================
       SUBROUTINE SPMD_STATE_INIMAP2D_EXCH_DATA()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE STATE_INIMAP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,J,K,L,NG,NREC,MSGOFF2,IDOM,IPOS
      INTEGER NCELL, NPTS, NBMAT

      DATA MSGOFF/10002/

      my_real, ALLOCATABLE,DIMENSION(:,:) :: BUFF_R  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !------------------------!                                                              
      !---EXCHANGE DATA CELL                                                                  
      !------------------------!                                                              
      IF (ISPMD/=0) THEN                                                                    
        NCELL = STATE_INIMAP_BUF(1)%NUM_CENTROIDS                                             
        NBMAT = STATE_INIMAP_BUF(1)%NSUBMAT                                                   
        ALLOCATE (BUFF_R(NCELL, 3 + 4*NBMAT))                                                 
        !1:abscissa   ;   2,3,4 vfrac_i, rho_i, E_i   ;  ...Etc                               
        DO I=1,NCELL                                                                          
          BUFF_R(I,1) = STATE_INIMAP_BUF(1)%POS_CENTROIDS(I)
          BUFF_R(I,2) = STATE_INIMAP_BUF(1)%POS2_CENTROIDS(I)
          BUFF_R(I,3) = STATE_INIMAP_BUF(1)%CELL_IDS(I)
          K=3                                                                                 
          DO J=1,NBMAT                                                                        
            K=K+1                                                                             
            BUFF_R(I,K) = STATE_INIMAP_BUF(1)%SUBMAT(J)%VFRAC(I)                              
            K=K+1                                                                             
            BUFF_R(I,K) = STATE_INIMAP_BUF(1)%SUBMAT(J)%RHO(I)                                
            K=K+1                                                                             
            BUFF_R(I,K) = STATE_INIMAP_BUF(1)%SUBMAT(J)%E(I)                                  
            K=K+1                                                                             
            BUFF_R(I,K) = STATE_INIMAP_BUF(1)%SUBMAT(J)%PRES(I)                                  
          ENDDO                                                                               
        ENDDO                                                                                 
        MSGTYP = MSGOFF                                                                       
        SIZ = NCELL*(3+4*NBMAT)                                                               
        CALL MPI_SEND(BUFF_R, SIZ, REAL,IT_SPMD(1),MSGTYP,MPI_COMM_WORLD,ierror)  
        IF(ALLOCATED(BUFF_R))DEALLOCATE(BUFF_R)          
      ELSE                                                                                                         
        DO I=2,NSPMD                                                                          
          ! Reception du buffer flottant double des adresses DATA_I                           
          MSGTYP = MSGOFF                                                                     
          NCELL = STATE_INIMAP_BUF(I)%NUM_CENTROIDS
          NBMAT = STATE_INIMAP_BUF(I)%NSUBMAT                                             
          SIZ = NCELL*(3+4*NBMAT) 
          !IF(SIZ == 0)CYCLE                                                            
          ALLOCATE (BUFF_R(NCELL, 3+4*NBMAT ))                                              
          CALL MPI_RECV(BUFF_R,SIZ,REAL,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,STATUS,ierror)       
          ALLOCATE(STATE_INIMAP_BUF(I)%CELL_IDS(NCELL))                                  
          ALLOCATE(STATE_INIMAP_BUF(I)%POS_CENTROIDS(NCELL))                                  
          ALLOCATE(STATE_INIMAP_BUF(I)%POS2_CENTROIDS(NCELL)) 
          ALLOCATE(STATE_INIMAP_BUF(I)%SUBMAT(NBMAT))                                         
          DO J=1,NBMAT                                                                        
            ALLOCATE(STATE_INIMAP_BUF(I)%SUBMAT(J)%VFRAC(NCELL))                              
            ALLOCATE(STATE_INIMAP_BUF(I)%SUBMAT(J)%RHO(NCELL))                                
            ALLOCATE(STATE_INIMAP_BUF(I)%SUBMAT(J)%E(NCELL))                                  
            ALLOCATE(STATE_INIMAP_BUF(I)%SUBMAT(J)%PRES(NCELL))   
          ENDDO                                                                            
          DO L=1,NCELL                                                                     
            STATE_INIMAP_BUF(I)%POS_CENTROIDS(L) = BUFF_R(L,1)                             
            STATE_INIMAP_BUF(I)%POS2_CENTROIDS(L) = BUFF_R(L,2)  
            STATE_INIMAP_BUF(I)%CELL_IDS(L) = BUFF_R(L,3)                                        
            K=3
            DO J=1,NBMAT                                                                   
              K=K+1                                                                        
              STATE_INIMAP_BUF(I)%SUBMAT(J)%VFRAC(L) = BUFF_R(L,K)                         
              K=K+1                                                                        
              STATE_INIMAP_BUF(I)%SUBMAT(J)%RHO(L) = BUFF_R(L,K)                           
              K=K+1                                                                        
              STATE_INIMAP_BUF(I)%SUBMAT(J)%E(L) = BUFF_R(L,K)                             
              K=K+1                                                                        
              STATE_INIMAP_BUF(I)%SUBMAT(J)%PRES(L) = BUFF_R(L,K)                             
            ENDDO                                                                          
          ENDDO    
          IF(ALLOCATED(BUFF_R))DEALLOCATE (BUFF_R)                                                                        
        ENDDO !next I=2,NSPMD                                                              
      ENDIF       
                                                                               
      !------------------------!                                    
      !---EXCHANGE VEL                                       
      !------------------------!                                                     
      IF (ISPMD /= 0) THEN                                                 
        NPTS = STATE_INIMAP_BUF(1)%NUM_POINTS                             
        ALLOCATE (BUFF_R(NPTS,5))                           
        DO I=1,NPTS                                        
          BUFF_R(I,1) = STATE_INIMAP_BUF(1)%POS_NODES(I)
          BUFF_R(I,2) = STATE_INIMAP_BUF(1)%POS2_NODES(I)       
          BUFF_R(I,3) = STATE_INIMAP_BUF(1)%VEL_NODES(I)
          BUFF_R(I,4) = STATE_INIMAP_BUF(1)%VEL2_NODES(I)
          BUFF_R(I,5) = STATE_INIMAP_BUF(1)%NODE_IDS(I)         
        ENDDO                                                 
        MSGTYP = MSGOFF                                            
        SIZ = 5*NPTS                                            
        CALL MPI_SEND(BUFF_R, SIZ, REAL,IT_SPMD(1),MSGTYP,MPI_COMM_WORLD,ierror)
        IF(ALLOCATED(BUFF_R))DEALLOCATE (BUFF_R)    
      !deallocate useless data once sent
        NBMAT=STATE_INIMAP_BUF(1)%NSUBMAT
        DO J=1,NBMAT
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(J)%VFRAC))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(J)%VFRAC)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(J)%RHO  ))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(J)%RHO)        
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(J)%E    ))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(J)%E)
          IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT(J)%PRES))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT(J)%PRES)
        ENDDO
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%SUBMAT        ))DEALLOCATE(STATE_INIMAP_BUF(1)%SUBMAT)
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_NODES     ))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_NODES)   
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS2_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(1)%POS2_NODES)          
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%VEL_NODES     ))DEALLOCATE(STATE_INIMAP_BUF(1)%VEL_NODES)  
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%VEL2_NODES    ))DEALLOCATE(STATE_INIMAP_BUF(1)%VEL2_NODES)           
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS_CENTROIDS ))DEALLOCATE(STATE_INIMAP_BUF(1)%POS_CENTROIDS) 
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%POS2_CENTROIDS))DEALLOCATE(STATE_INIMAP_BUF(1)%POS2_CENTROIDS)  
        IF(ALLOCATED(STATE_INIMAP_BUF(1)%NODE_IDS      ))DEALLOCATE(STATE_INIMAP_BUF(1)%NODE_IDS)                              
      ELSE                                                 
        NPTS = STATE_INIMAP_BUF(1)%NUM_POINTS                                         
        DO I=2,NSPMD                                                
          ! Reception du buffer flottant double des adresses DATA_I                        
          MSGTYP = MSGOFF                                                
          NPTS = STATE_INIMAP_BUF(I)%NUM_POINTS                                            
          SIZ = 5*NPTS                                         
          ALLOCATE (BUFF_R(NPTS, 5 ))  
          CALL MPI_RECV(BUFF_R,SIZ,REAL,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,STATUS,ierror)    
          ALLOCATE(STATE_INIMAP_BUF(I)%POS_NODES(NPTS))                 
          ALLOCATE(STATE_INIMAP_BUF(I)%POS2_NODES(NPTS))                                  
          ALLOCATE(STATE_INIMAP_BUF(I)%VEL_NODES(NPTS))                 
          ALLOCATE(STATE_INIMAP_BUF(I)%VEL2_NODES(NPTS)) 
          ALLOCATE(STATE_INIMAP_BUF(I)%NODE_IDS(NPTS))                                  
          DO L=1,NPTS                                      
            STATE_INIMAP_BUF(I)%POS_NODES(L) = BUFF_R(L,1)  
            STATE_INIMAP_BUF(I)%POS2_NODES(L) = BUFF_R(L,2)  
            STATE_INIMAP_BUF(I)%VEL_NODES(L) = BUFF_R(L,3)  
            STATE_INIMAP_BUF(I)%VEL2_NODES(L) = BUFF_R(L,4)  
            STATE_INIMAP_BUF(I)%NODE_IDS(L) = BUFF_R(L,5)             
          ENDDO   
          IF(ALLOCATED(BUFF_R))DEALLOCATE (BUFF_R)                                             
        ENDDO !next I=2,NSPMD                                                
      ENDIF                                               
                 
C-----------------------------------------------
#endif
       RETURN
       END SUBROUTINE SPMD_STATE_INIMAP2D_EXCH_DATA
