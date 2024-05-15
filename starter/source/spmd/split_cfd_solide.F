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
Chd|  SPLIT_CFD_SOLIDE              source/spmd/split_cfd_solide.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|====================================================================
        SUBROUTINE SPLIT_CFD_SOLIDE(NUMELS,ALE_CONNECTIVITY,IXS,ALE_ELM,SIZE_ALE_ELM)
!$COMMENT
!       SPLIT_CFD_SOLIDE description :
!              SPLIT_CFD_SOLIDE retains all the useful solid elements
!              for the splitting in DDSPLIT for each domain in order to 
!              avoid a quadratic loop in DDSPLIT
!       SPLIT_CFD_SOLIDE organization :
!               - first allocation of ALE_ELM%SOL_ID array
!               - for each solid element, if a neighbouring element 
!                 is useful for the splitting, catch the processor list
!               - fill the ALE_ELM array and if the ALE_ELM size is not
!                 sufficient, then increase the size
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPLIT_CFD_MOD
        USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NUMELS
        INTEGER, DIMENSION(NIXS,*), INTENT(IN) :: IXS    
        INTEGER, DIMENSION(NSPMD) :: SIZE_ALE_ELM  
        TYPE(split_cfd_type), DIMENSION(NSPMD), INTENT(INOUT) :: ALE_ELM
        TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NUMELS   : integer ; number of solid element
!       NIXS     : integer ; dimension of IXS array

!       IXS      : integer ; dimension=NIXS,NUMELS ; property of solid element
!       SIZE_ALE_ELM : integer ; dimension=NSPMD ; size of ALE_ELM%SOL_ID array
!       ALE_ELM : split_cfd_type ; dimension=NSPMD ; solid element ID used 
!                 during the domain splitting (ALE part)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL, DIMENSION(NSPMD) :: BOOL
        INTEGER :: I,J,K,L,N,NS,IAD1,LGTH
        INTEGER :: SOLV,ISPMD,NBR_PROC,NEW_SIZE_II,MIN_SIZE
        INTEGER, DIMENSION(NSPMD) :: ID_SPMD,II
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP   
C ----------------------------------------

        !       -----------------------------
        !       allocation of ALE_ELM (overestimation of the size)
        SIZE_ALE_ELM(1:NSPMD) = NUMELS/NSPMD+1
        DO ISPMD=1,NSPMD
                ALLOCATE( ALE_ELM(ISPMD)%SOL_ID(SIZE_ALE_ELM(ISPMD)) )
        ENDDO
        !       -----------------------------
        II(1:NSPMD) = 0
        DO I=1,NUMELS
                !       -----------------------------
                ! check if a neighbouring element is used
                BOOL(1:NSPMD)=.FALSE.
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                DO J=1,8
                  NS = IXS(J+1,I)
                  DO K=1,LGTH
                    SOLV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
                    IF (SOLV>0) THEN
                      DO L=1,8
                        N = IXS(L+1,SOLV)
                        CALL PLIST_IFRONT(ID_SPMD,N,NBR_PROC)
                        DO ISPMD=1,NBR_PROC
                          BOOL(ID_SPMD(ISPMD)) = .TRUE.
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDDO
                ENDDO
                !       -----------------------------
                !       fill the ALE_ELM array / increase the size
                DO ISPMD=1,NSPMD
                  IF( BOOL(ISPMD) ) THEN
                    II(ISPMD) = II(ISPMD) + 1
                    IF( II(ISPMD)>SIZE_ALE_ELM(ISPMD) ) THEN
                      !       need to check the size for small test, ie. when NUMELS < NSPMS
                      MIN_SIZE=MAX(1,5* NUMELS/ ( 4*NSPMD ))
                      NEW_SIZE_II = SIZE_ALE_ELM(ISPMD) + MIN_SIZE
                      ALLOCATE( TMP( NEW_SIZE_II ) )
                      TMP(1:SIZE_ALE_ELM(ISPMD)) = ALE_ELM(ISPMD)%SOL_ID( 1:SIZE_ALE_ELM(ISPMD) )
                      CALL MOVE_ALLOC(FROM=TMP,TO=ALE_ELM(ISPMD)%SOL_ID)
                      SIZE_ALE_ELM(ISPMD) = NEW_SIZE_II
                    ENDIF
                    ALE_ELM(ISPMD)%SOL_ID(II(ISPMD)) = I
                  ENDIF
                ENDDO
                !       -----------------------------
        ENDDO
        !       -----------------------------

        SIZE_ALE_ELM(1:NSPMD) = II(1:NSPMD)

        RETURN
        END SUBROUTINE SPLIT_CFD_SOLIDE



Chd|====================================================================
Chd|  DEALLOCATE_SPLIT_CFD_SOLIDE   source/spmd/split_cfd_solide.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|====================================================================
        SUBROUTINE DEALLOCATE_SPLIT_CFD_SOLIDE(ALE_ELM)
!$COMMENT
!       DEALLOCATE_SPLIT_CFD_SOLIDE description :
!              DEALLOCATE_SPLIT_CFD_SOLIDE deallocates the
!               ALE_ELM array
!       DEALLOCATE_SPLIT_CFD_SOLIDE organization :
!               - loop over NSPMD + deallocation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPLIT_CFD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(split_cfd_type), DIMENSION(NSPMD), INTENT(INOUT) :: ALE_ELM 
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       ALE_ELM : split_cfd_type ; dimension=NSPMD ; solid element ID used 
!                 during the domain splitting (ALE part)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ISPMD
C ----------------------------------------
        DO ISPMD=1,NSPMD
                IF( ALLOCATED(ALE_ELM(ISPMD)%SOL_ID) ) DEALLOCATE( ALE_ELM(ISPMD)%SOL_ID )
        ENDDO
        RETURN
        END SUBROUTINE DEALLOCATE_SPLIT_CFD_SOLIDE



