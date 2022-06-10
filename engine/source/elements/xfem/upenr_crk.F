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
Chd|  UPENR_CRK                     source/elements/xfem/upenr_crk.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENR_CRK(ADDCNE_CRK ,INOD_CRK ,NODFT ,NODLT,
     .                     NODENR     ,ENRTAG   ,NODLEVXF,PROCNE_CRK)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODFT,NODLT,NODENR(*),
     .        ENRTAG(NUMNOD,*),NODLEVXF(*),PROCNE_CRK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,N,NSX,NCT_CRK,NC_CRK,EN0,ENR,ILEV,NLEV,OK_UP,ENR_MAX
      INTEGER, DIMENSION(:),ALLOCATABLE :: IFAC0
C=======================================================================
      ALLOCATE(IFAC0(0:IENRNOD))
c
      DO N  = NODFT,NODLT
        NSX = INOD_CRK(N)
        IF (NSX <= 0) CYCLE
        IFAC0   = 0
        NCT_CRK = ADDCNE_CRK(NSX)-1
        NC_CRK  = ADDCNE_CRK(NSX+1)-ADDCNE_CRK(NSX)
        NLEV    = NODLEVXF(NSX)
        DO ILEV=1,NLEV
          OK_UP = 0
          DO KK = NCT_CRK+1, NCT_CRK+NC_CRK                                 
            EN0 = ABS(CRKLVSET(ILEV)%ENR0(1,KK))                            
            IF (EN0 > 0) THEN
              ENR_MAX = ENRTAG(N,EN0)                           
              IF (ENR_MAX > 0) THEN  ! il y a une fissure dans l'adresse sky  
                IFAC0(EN0) = ENR_MAX                                          
                OK_UP = 1                                                     
              ENDIF                                                           
            ENDIF                                                           
          ENDDO                                                             
c
          IF (OK_UP == 1) THEN
            DO KK = NCT_CRK+1, NCT_CRK+NC_CRK
              EN0 = ABS(CRKLVSET(ILEV)%ENR0(1,KK))
              ENR = IFAC0(EN0)
              IF (EN0 > 0 .and. ENR > 0) CRKLVSET(ILEV)%ENR0(1,KK) = ENR
            ENDDO
          ENDIF
        ENDDO   !  ILEV=1,NLEV
C
        ENRTAG(N,1:IENRNOD) = 0
      ENDDO     !  N = NODFT,NODLT
c-----
      DEALLOCATE(IFAC0)
C-----------
      RETURN
      END
