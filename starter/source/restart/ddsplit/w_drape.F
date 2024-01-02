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
Chd|  W_DRAPE                       source/restart/ddsplit/w_drape.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|====================================================================
      SUBROUTINE W_DRAPE(DRAPE,INDX, CEP,P,NUMEL,NUMEL_L,NELDRP_L,LEN_IA,LEN_AM)          
         USE DRAPE_MOD
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*), P, NUMEL, NUMEL_L, LEN_AM,LEN_IA
      TYPE(DRAPE_), DIMENSION(NUMELC_DRAPE + NUMELTG_DRAPE):: DRAPE
      INTEGER, DIMENSION(NUMEL) :: INDX
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, EL, J,NPT,NDIM,IDX,NUMELDRP_L,NDIM_SLICE,
     .        NDIM_INDX_PLY,K,II,IS,NEL,NSLICE,NELDRP_L,NPT_DRP
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITMP
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPTDRP_ELT,ISLICE,INDX_PLY,
     .                                        NPT_ELT,INDEX
C-----------------------------------------------
!     allocate 2d array
! --------------------------------------
C
      ALLOCATE(NPT_ELT(NUMEL_L),NPTDRP_ELT(NUMEL_L), INDEX(NUMEL_L)) 
C    
      INDEX = 0
      NPTDRP_ELT =0
      NPT_ELT =0
      !
      NDIM = 0
      NEL = 0
      NDIM_SLICE = 0
      NDIM_INDX_PLY = 0
      NDIM = 0
      NELDRP_L = 0
      DO I = 1, NUMEL
        IF(CEP(I)== P) THEN
          II = INDX(I)
          NEL = NEL + 1 
          IF(II > 0) THEN
            NELDRP_L = NELDRP_L + 1  
            INDEX(NEL) = NELDRP_L
            NPT_DRP =  DRAPE(II)%NPLY_DRAPE
            NPT = DRAPE(II)%NPLY
            NPTDRP_ELT(NELDRP_L)   = NPT_DRP
            NPT_ELT(NELDRP_L ) = NPT
            DO J = 1,NPT_DRP
              NSLICE = DRAPE(II)%DRAPE_PLY(J)%NSLICE
              NDIM = NDIM + NSLICE
            ENDDO
            NDIM_SLICE = NDIM_SLICE + NPT_DRP
            NDIM_INDX_PLY = NDIM_INDX_PLY + NPT
          ENDIF
         ENDIF 
      ENDDO
      IF(NELDRP_L > 0) THEN
C      
          ALLOCATE(ITMP(NDIM,2),RTMP(NDIM,2),ISLICE(NDIM_SLICE),INDX_PLY(NDIM_INDX_PLY))
C
          NDIM_SLICE = 0
          NDIM_INDX_PLY = 0
          NDIM = 0 
          NELDRP_L = 0
          NEL = 0
          DO I = 1, NUMEL
            IF(CEP(I)== P) THEN
              II = INDX(I)
              NEL = NEL + 1 
              IF(II > 0) THEN
                NELDRP_L = NELDRP_L + 1  
                NPT_DRP =  DRAPE(II)%NPLY_DRAPE
                NPT = DRAPE(II)%NPLY
                DO J = 1,NPT_DRP
                  NSLICE = DRAPE(II)%DRAPE_PLY(J)%NSLICE
                  ISLICE(NDIM_SLICE + J) =  NSLICE
                  DO IS = 1,NSLICE
                      ITMP(NDIM + IS,1) = DRAPE(II)%DRAPE_PLY(J)%IDRAPE(IS,1) 
                      ITMP(NDIM + IS,2) = DRAPE(II)%DRAPE_PLY(J)%IDRAPE(IS,2)
                      RTMP(NDIM + IS,1) = DRAPE(II)%DRAPE_PLY(J)%RDRAPE(IS,1)
                      RTMP(NDIM + IS,2) = DRAPE(II)%DRAPE_PLY(J)%RDRAPE(IS,2)
                  ENDDO
                  NDIM = NDIM + NSLICE
                ENDDO
                NDIM_SLICE = NDIM_SLICE + NPT_DRP
                DO J = 1,NPT
                  INDX_PLY(NDIM_INDX_PLY + J) = DRAPE(II)%INDX_PLY(J)
                ENDDO
                NDIM_INDX_PLY = NDIM_INDX_PLY + NPT
              ENDIF
            ENDIF  
          ENDDO      
C     
          CALL WRITE_I_C(NDIM, 1)
          CALL WRITE_I_C(NDIM_SLICE,1)
          Call WRITE_I_C(NDIM_INDX_PLY,1) 
          CALL WRITE_I_C(INDEX,NUMEL_L)
          CALL WRITE_I_C(NPTDRP_ELT,NELDRP_L)
          CALL WRITE_I_C(NPT_ELT,NELDRP_L)
          CALL WRITE_I_C(ISLICE,NDIM_SLICE)
          CALL WRITE_I_C(INDX_PLY,NDIM_INDX_PLY)
          CALL WRITE_I_C(ITMP,2*NDIM)     
          CALL WRITE_DB(RTMP,2*NDIM)
          LEN_AM = LEN_AM + 2*NDIM            
          LEN_IA = LEN_IA + 2*NDIM +  NUMEL_L   + 2*NELDRP_L    
     .                    + NDIM_SLICE + NDIM_INDX_PLY  + 3   
           DEALLOCATE(ITMP,RTMP,ISLICE,INDX_PLY,NPT_ELT,NPTDRP_ELT, INDEX) 
       ENDIF
! --------------------------------------
      RETURN
      END
