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
Chd|  NEED_FOR_REACTIONS            source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE NEED_FOR_REACTIONS(NPBY,H3D_DATA,COMPTREAC)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),COMPTREAC
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, IFAIL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       LOOK FOR RBODY WITH FAILURE              !
      !------------------------------------------------!

      NRBFAIL = 0
      DO N=1,NRBYKIN
        IFAIL  =NPBY(18,N)
        IF(IFAIL/=0) THEN
          NRBFAIL   = NRBFAIL+1
        END IF
      ENDDO
      IF(NRBFAIL/=0) COMPTREAC = 1  

      IF (ANIM_V(17) == 1 .OR. H3D_DATA%N_VECT_FREAC == 1 .OR. 
     .    ANIM_V(18) == 1 .OR. H3D_DATA%N_VECT_MREAC == 1 ) THEN
        COMPTREAC = 1
      END IF

      IF(IREAC /= 0) COMPTREAC=1
      !------------------------------------------------!

      RETURN
      END
Chd|====================================================================
Chd|  STORE_FORCES                  source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORE_FORCES(NODFT ,NODLT ,A ,AR ,FREAC)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      my_real
     .   A(3,NUMNOD) , AR(3,NUMNOD), FREAC(6,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       SAVING FORCES (FEXT+FINT+FCONT)          !
      !------------------------------------------------!

      DO N=NODFT,NODLT
        FREAC(1,N) = A(1,N) 
        FREAC(2,N) = A(2,N) 
        FREAC(3,N) = A(3,N) 
      ENDDO

      IF (IRODDL/=0) THEN
        DO N=NODFT,NODLT
          FREAC(4,N) = AR(1,N)
          FREAC(5,N) = AR(2,N)
          FREAC(6,N) = AR(3,N)
        ENDDO
      END IF
      
      !------------------------------------------------!

      RETURN
      END
Chd|====================================================================
Chd|  STORE_ACCEL_FIELD             source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORE_ACCEL_FIELD(NODFT ,NODLT ,A     ,AR    ,MS    ,
     2                             IN    ,FREAC ,IFLAG )
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, IFLAG
      my_real
     .   A(3,NUMNOD) , AR(3,NUMNOD), MS(NUMNOD), IN(IRODDL*NUMNOD), FREAC(6,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       COMPUTE REACTION FORCES (FINALIZATION)   !
      !------------------------------------------------!

      DO N=NODFT,NODLT
        FREAC(1,N) = FREAC(1,N) + IFLAG * MS(N)*A(1,N) 
        FREAC(2,N) = FREAC(2,N) + IFLAG * MS(N)*A(2,N) 
        FREAC(3,N) = FREAC(3,N) + IFLAG * MS(N)*A(3,N)
      ENDDO

      IF (IRODDL/=0) THEN
        DO N=NODFT,NODLT
          FREAC(4,N) = FREAC(4,N) + IFLAG * IN(N)*AR(1,N) 
          FREAC(5,N) = FREAC(5,N) + IFLAG * IN(N)*AR(2,N) 
          FREAC(6,N) = FREAC(6,N) + IFLAG * IN(N)*AR(3,N) 
        ENDDO
      END IF
      
      !------------------------------------------------!

      RETURN
      END
Chd|====================================================================
Chd|  FINALIZE_REACTIONS            source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FINALIZE_REACTIONS(NODFT ,NODLT ,A ,AR ,MS ,
     2                                   IN    ,FREAC )
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      my_real
     .   A(3,NUMNOD) , AR(3,NUMNOD), MS(NUMNOD), IN(IRODDL*NUMNOD), FREAC(6,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       COMPUTE REACTION FORCES (FINALIZATION)   !
      !------------------------------------------------!

      DO N=NODFT,NODLT
        FREAC(1,N) = MS(N)*A(1,N) - FREAC(1,N) 
        FREAC(2,N) = MS(N)*A(2,N) - FREAC(2,N) 
        FREAC(3,N) = MS(N)*A(3,N) - FREAC(3,N) 
      ENDDO

      IF (IRODDL/=0) THEN
        DO N=NODFT,NODLT
          FREAC(4,N) = IN(N)*AR(1,N) - FREAC(4,N)
          FREAC(5,N) = IN(N)*AR(2,N) - FREAC(5,N)
          FREAC(6,N) = IN(N)*AR(3,N) - FREAC(6,N)
        ENDDO
      END IF
      
      !------------------------------------------------!

      RETURN
      END
