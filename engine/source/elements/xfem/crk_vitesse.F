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
Chd|  CRK_VITESSE                   source/elements/xfem/crk_vitesse.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_VITESSE(
     .           ADDCNE_CRK,INOD_CRK ,NODLEVXF  ,NODFT     ,NODLT    ,
     .           X         ,V        ,VR        ,A         ,AR       ,
     .           ITAB      )                                              
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
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODLEVXF(*),ITAB(NUMNOD)
      my_real X(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,KK,IL,N,IAD,IAD1,IAD2,NN,NC,NCT,EN0,EN,EN1,IFI,ILAY,
     .   IXEL,ILEV,NLEV,N1,N2,NX1
      my_real X1,X2,X3,V1,V2,V3,A1,A2,A3,AR1,AR2,AR3,DX,DY,DZ
C=======================================================================
      DO N = NODFT,NODLT
        NN = INOD_CRK(N)
        IF (NN <= 0) CYCLE
        NCT  = ADDCNE_CRK(NN)-1
        NC   = ADDCNE_CRK(NN+1)-ADDCNE_CRK(NN)
        NLEV = NODLEVXF(NN)
c---
        DO ILEV = 1,NLEV                                            
          IXEL = MOD(ILEV-1, NXEL) + 1
          ILAY = (ILEV-IXEL)/NXEL  + 1
          DO KK = NCT+1, NCT+NC
            EN0  = CRKLVSET(ILEV)%ENR0(2,KK)  ! enr initial du debut de cycle
            EN   = CRKLVSET(ILEV)%ENR0(1,KK)  ! enr mise a jour dans le cycle
            IFI  = XFEM_PHANTOM(ILAY)%IFI(KK)
c---
            IF (EN0 > 0) THEN           ! free node
              A1  = CRKAVX(ILEV)%A(1,KK)
              A2  = CRKAVX(ILEV)%A(2,KK)
              A3  = CRKAVX(ILEV)%A(3,KK)
              AR1 = CRKAVX(ILEV)%AR(1,KK)
              AR2 = CRKAVX(ILEV)%AR(2,KK)
              AR3 = CRKAVX(ILEV)%AR(3,KK)
c             new velocity
              CRKAVX(ILEV)%V(1,KK)  = CRKAVX(ILEV)%V(1,KK) + DT12*A1
              CRKAVX(ILEV)%V(2,KK)  = CRKAVX(ILEV)%V(2,KK) + DT12*A2
              CRKAVX(ILEV)%V(3,KK)  = CRKAVX(ILEV)%V(3,KK) + DT12*A3
              CRKAVX(ILEV)%VR(1,KK) = CRKAVX(ILEV)%VR(1,KK)+ DT12*AR1
              CRKAVX(ILEV)%VR(2,KK) = CRKAVX(ILEV)%VR(2,KK)+ DT12*AR2
              CRKAVX(ILEV)%VR(3,KK) = CRKAVX(ILEV)%VR(3,KK)+ DT12*AR3
              CRKAVX(ILEV)%A(1,KK)  = ZERO
              CRKAVX(ILEV)%A(2,KK)  = ZERO
              CRKAVX(ILEV)%A(3,KK)  = ZERO
              CRKAVX(ILEV)%AR(1,KK) = ZERO
              CRKAVX(ILEV)%AR(2,KK) = ZERO
              CRKAVX(ILEV)%AR(3,KK) = ZERO
c             new coordinates
              DX = DT2 * CRKAVX(ILEV)%V(1,KK) 
              DY = DT2 * CRKAVX(ILEV)%V(2,KK) 
              DZ = DT2 * CRKAVX(ILEV)%V(3,KK) 
              CRKAVX(ILEV)%X(1,KK) = CRKAVX(ILEV)%X(1,KK) + DX
              CRKAVX(ILEV)%X(2,KK) = CRKAVX(ILEV)%X(2,KK) + DY
              CRKAVX(ILEV)%X(3,KK) = CRKAVX(ILEV)%X(3,KK) + DZ
            ELSEIF (EN0 <= 0) THEN
              CRKAVX(ILEV)%X(1,KK) = X(1,N)
              CRKAVX(ILEV)%X(2,KK) = X(2,N)
              CRKAVX(ILEV)%X(3,KK) = X(3,N)
              IF (EN > 0) THEN  ! noeud relache => vit noeud std instead of zero
                CRKAVX(ILEV)%V(1,KK)  = V(1,N)
                CRKAVX(ILEV)%V(2,KK)  = V(2,N)
                CRKAVX(ILEV)%V(3,KK)  = V(3,N)
                CRKAVX(ILEV)%VR(1,KK) = VR(1,N)
                CRKAVX(ILEV)%VR(2,KK) = VR(2,N)
                CRKAVX(ILEV)%VR(3,KK) = VR(3,N)
              ELSEIF (IFI /= 0) THEN    ! fissure statique => noeud standard
                CRKAVX(ILEV)%V(1,KK)  = V(1,N)
                CRKAVX(ILEV)%V(2,KK)  = V(2,N)
                CRKAVX(ILEV)%V(3,KK)  = V(3,N)
                CRKAVX(ILEV)%VR(1,KK) = VR(1,N)
                CRKAVX(ILEV)%VR(2,KK) = VR(2,N)
                CRKAVX(ILEV)%VR(3,KK) = VR(3,N)
              ENDIF
            END IF
c---
          ENDDO  ! DO KK = NCT+1, NCT+NC
        ENDDO    ! DO ILEV= NLEV,1,-1
      ENDDO      ! N = NODFT,NODLT
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  CRK_ZERO_ACCEL                source/elements/xfem/crk_vitesse.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_ZERO_ACCEL(ADDCNE_CRK,INOD_CRK,NODFT,NODLT,NODLEVXF)
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
#include      "com08_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODFT,NODLT,NODLEVXF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,N,NN,NCT,NC,ILEV,NLEV
c=======================================================================
      DO  N = NODFT,NODLT
        NN = INOD_CRK(N)
        IF (NN > 0) THEN
C---
          NCT  = ADDCNE_CRK(NN)-1
          NC   = ADDCNE_CRK(NN+1)-ADDCNE_CRK(NN)
          NLEV = NODLEVXF(NN)
          DO KK = NCT+1, NCT+NC
            DO ILEV=1,NLEV
              CRKAVX(ILEV)%A(1,KK)  = ZERO
              CRKAVX(ILEV)%A(2,KK)  = ZERO
              CRKAVX(ILEV)%A(3,KK)  = ZERO
              CRKAVX(ILEV)%AR(1,KK) = ZERO
              CRKAVX(ILEV)%AR(2,KK) = ZERO
              CRKAVX(ILEV)%AR(3,KK) = ZERO
            END DO
          END DO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CRK_ZERO_FSKY                 source/elements/xfem/crk_vitesse.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_ZERO_FSKY(CRKSKY,ADDCNE_CRK,INOD_CRK,NODFT,NODLT,
     .                         NODLEVXF)
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODFT,NODLT,NODLEVXF(*)
      TYPE(XFEM_SKY_)  , DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,N,NN,NCT,NC,ILEV,NLEV
C=======================================================================
      DO N = NODFT,NODLT
        NN = INOD_CRK(N)
        IF (NN >  0) THEN
          NCT  = ADDCNE_CRK(NN)-1
          NC   = ADDCNE_CRK(NN+1)-ADDCNE_CRK(NN)
          NLEV = NODLEVXF(NN)
          DO KK = NCT+1, NCT+NC
            DO ILEV=1,NLEV
              CRKSKY(ILEV)%FSKY(1,KK) = ZERO
              CRKSKY(ILEV)%FSKY(2,KK) = ZERO
              CRKSKY(ILEV)%FSKY(3,KK) = ZERO
              CRKSKY(ILEV)%FSKY(4,KK) = ZERO
              CRKSKY(ILEV)%FSKY(5,KK) = ZERO
              CRKSKY(ILEV)%FSKY(6,KK) = ZERO
            END DO
          END DO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
