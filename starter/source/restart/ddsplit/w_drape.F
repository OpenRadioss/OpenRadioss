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
Chd|  W_DRAPE                       source/restart/ddsplit/w_drape.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|====================================================================
      SUBROUTINE W_DRAPE(DRAPE,CEP,P,NUMEL,NUMEL_L,LEN_IA,LEN_AM)          
         USE DRAPE_MOD
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*), P, NUMEL, NUMEL_L, LEN_AM,LEN_IA
      TYPE(DRAPE_) :: DRAPE(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, EL, J,NPT,NDIM,IDX
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITMP,
     .                                      NPT_ELT,INDEX
C-----------------------------------------------
!     allocate 2d array
! --------------------------------------
C
      EL = 0
      ALLOCATE(NPT_ELT(NUMEL_L), INDEX(NUMEL_L+1)) 
C    
      INDEX = 0
      NDIM = 0
      DO I = 1, NUMEL
        IF(CEP(I).EQ.P) THEN
          EL = EL + 1
          NPT_ELT(EL)   = DRAPE(I)%NPT
          NDIM = NDIM + NPT_ELT(EL)
          INDEX(EL + 1) = INDEX(EL) + NPT_ELT(EL)
        ENDIF
      ENDDO
C      
      ALLOCATE(ITMP(NDIM),RTMP(NDIM,2))
C      
      IDX  = 0 
      DO I = 1, NUMEL
        IF(CEP(I).EQ.P) THEN
          NPT = DRAPE(I)%NPT
          DO J = 1, NPT
              ITMP(IDX + J)  =  DRAPE(I)%IDRAPE(J)
              RTMP(IDX + J,1) = DRAPE(I)%RDRAPE(J,1)
              RTMP(IDX + J,2) = DRAPE(I)%RDRAPE(J,2)
          ENDDO
          IDX = IDX + NPT
        ENDIF
      ENDDO
C     
      CALL WRITE_I_C(NDIM, 1) 
      CALL WRITE_I_C(INDEX,NUMEL_L)
      CALL WRITE_I_C(NPT_ELT,NUMEL_L)      
      CALL WRITE_I_C(ITMP,NDIM)            
      CALL WRITE_DB(RTMP,2*NDIM)
      LEN_AM = LEN_AM + 2*NDIM            
      LEN_IA = LEN_IA + 2*NUMEL_L + NDIM + 1   
      DEALLOCATE(ITMP,RTMP,INDEX,NPT_ELT)                
! --------------------------------------
      RETURN
      END
