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
Chd|  REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUTsource/output/reactions.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT(NPBY,H3D_DATA,COMPTREAC)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is setting COMPTREAC to 1 if FREAC & MREAC output are requested
C
C  CRITERA WHICH SET COMPTREAC TO 1 :
C    - RBODY IS SWITCHED OFF DUE TO CRITERION ON REACTION FORCES (Ifail=NPBY(18)=1)
C    - /ANIM/VECT/FREAC is requested (ANIM_V(17) = 1)
C    - /ANIM/VECT/MREAC is requested (ANIM_V(18) = 1)
C    - /H3D/NODA/FREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /H3D/NODA/MREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /TH/NODE/REAC is requested (IREAC=1)
C
C  SEQUENCE IN SOLVER LOOP :
C    - REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT : check if output is requested
C    - REACTION_FORCES_1 : add FEXT+FINT
C                          --> FREAC = FEXT+FINT
C    - REACTION_FORCES_2 : add additional contribution Fgrav,Fbcs_cyclic,Fcentrif ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif)
C    - REACTION_FORCES_2 : add additional contribution Fdamping ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif) + (Fdamping)
C    - REACTION_FORCES_3 : Finally get what we need
C                          --> FREAC = FTOT - (FEXT+FINT) - (Fgrav,Fbcs_cyclic,Fcentrif) - (Fdamping)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPBY(NNPBY,*)
      INTEGER,INTENT(INOUT) :: COMPTREAC
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, IFAIL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !---------------------------------------!
      ! CRITERION FOR RBODY WITH 'Ifail' FLAG !
      !---------------------------------------!
      NRBFAIL = 0
      DO N=1,NRBYKIN
        IFAIL = NPBY(18,N)
        IF(IFAIL /= 0) THEN
          NRBFAIL = NRBFAIL + 1
        END IF
      ENDDO
      IF(NRBFAIL /= 0) THEN
        COMPTREAC = 1
      ENDIF

      !-----------------------------------------------------!
      ! CRITERION BASED ON OUTPUT REQUEST (ANIM, OUTP, H3D) !
      !-----------------------------------------------------!
      IF (ANIM_V(17) == 1 .OR. H3D_DATA%N_VECT_FREAC == 1 .OR. ANIM_V(18) == 1 .OR. H3D_DATA%N_VECT_MREAC == 1 ) THEN
        COMPTREAC = 1
      END IF

      !-----------------------------------------------------!
      ! CRITERION BASED ON OUTPUT REQUEST (TH)              !
      !-----------------------------------------------------!
      IF(IREAC /= 0) THEN
        COMPTREAC=1
      ENDIF

C-----------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  REACTION_FORCES_1             source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REACTION_FORCES_1(NODFT ,NODLT ,A ,AR ,FREAC)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is STEP 1 on 3 to OBTAIN FREAC array for output.
C Output is calculated if COMPTREAC=1
C
C  CRITERA WHICH SET COMPTREAC TO 1 :
C    - RBODY IS SWITCHED OFF DUE TO CRITERION ON REACTION FORCES (Ifail=NPBY(18)=1)
C    - /ANIM/VECT/FREAC is requested (ANIM_V(17) = 1)
C    - /ANIM/VECT/MREAC is requested (ANIM_V(18) = 1)
C    - /H3D/NODA/FREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /H3D/NODA/MREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /TH/NODE/REAC is requested (IREAC=1)
C
C  SEQUENCE IN SOLVER LOOP :
C    - REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT : check if output is requested
C    - REACTION_FORCES_1 : add FEXT+FINT
C                          --> FREAC = FEXT+FINT
C    - REACTION_FORCES_2 : add additional contribution Fgrav,Fbcs_cyclic,Fcentrif ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif)
C    - REACTION_FORCES_2 : add additional contribution Fdamping ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif) + (Fdamping)
C    - REACTION_FORCES_3 : Finally get what we need
C                          --> FREAC = FTOT - (FEXT+FINT) - (Fgrav,Fbcs_cyclic,Fcentrif) - (Fdamping)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NODFT, NODLT
      my_real,INTENT(IN) :: A(3,NUMNOD) , AR(3,NUMNOD)
      my_real,INTENT(INOUT) :: FREAC(6,NUMNOD)
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
      
C-----------------------------------------------
      RETURN
      END SUBROUTINE REACTION_FORCES_1

Chd|====================================================================
Chd|  REACTION_FORCES_2             source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REACTION_FORCES_2(NODFT,NODLT,A,AR,MS,IN,FREAC,IFLAG )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is STEP 2 on 3 to OBTAIN FREAC array for output.
C Output is calculated if COMPTREAC=1
C
C  CRITERA WHICH SET COMPTREAC TO 1 :
C    - RBODY IS SWITCHED OFF DUE TO CRITERION ON REACTION FORCES (Ifail=NPBY(18)=1)
C    - /ANIM/VECT/FREAC is requested (ANIM_V(17) = 1)
C    - /ANIM/VECT/MREAC is requested (ANIM_V(18) = 1)
C    - /H3D/NODA/FREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /H3D/NODA/MREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /TH/NODE/REAC is requested (IREAC=1)
C
C  SEQUENCE IN SOLVER LOOP :
C    - REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT : check if output is requested
C    - REACTION_FORCES_1 : add FEXT+FINT
C                          --> FREAC = FEXT+FINT
C    - REACTION_FORCES_2 : add additional contribution Fgrav,Fbcs_cyclic,Fcentrif ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif)
C    - REACTION_FORCES_2 : add additional contribution Fdamping ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif) + (Fdamping)
C    - REACTION_FORCES_3 : Finally get what we need
C                          --> FREAC = FTOT - (FEXT+FINT) - (Fgrav,Fbcs_cyclic,Fcentrif) - (Fdamping)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NODFT, NODLT, IFLAG
      my_real,INTENT(IN) :: A(3,NUMNOD) , AR(3,NUMNOD), MS(NUMNOD), IN(IRODDL*NUMNOD)
      my_real,INTENT(INOUT) :: FREAC(6,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !-----------------------------------------------------------!
      !       COMPUTE REACTION FORCES (ADDITIONAL CONTRIBUTION)   !
      !-----------------------------------------------------------!
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
      
C-----------------------------------------------
      RETURN
      END SUBROUTINE REACTION_FORCES_2

Chd|====================================================================
Chd|  REACTION_FORCES_3             source/output/reactions.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REACTION_FORCES_3(NODFT,NODLT,A,AR,MS,IN,FREAC)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is STEP 3 on 3 to OBTAIN FREAC array for output.
C Output is calculated if COMPTREAC=1
C
C  CRITERA WHICH SET COMPTREAC TO 1 :
C    - RBODY IS SWITCHED OFF DUE TO CRITERION ON REACTION FORCES (Ifail=NPBY(18)=1)
C    - /ANIM/VECT/FREAC is requested (ANIM_V(17) = 1)
C    - /ANIM/VECT/MREAC is requested (ANIM_V(18) = 1)
C    - /H3D/NODA/FREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /H3D/NODA/MREAC is requested (H3D_DATA%N_VECT_FREAC = 1)
C    - /TH/NODE/REAC is requested (IREAC=1)
C
C  SEQUENCE IN SOLVER LOOP :
C    - REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUT : check if output is requested
C    - REACTION_FORCES_1 : add FEXT+FINT
C                          --> FREAC = FEXT+FINT
C    - REACTION_FORCES_2 : add additional contribution Fgrav,Fbcs_cyclic,Fcentrif ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif)
C    - REACTION_FORCES_2 : add additional contribution Fdamping ...
C                          first time with IFLAG=-1, then followed with IFLAG = 1 : Fadd=m(A-A~)
C                          --> FREAC = (FEXT+FINT) + (Fgrav,Fbcs_cyclic,Fcentrif) + (Fdamping)
C    - REACTION_FORCES_3 : Finally get what we need
C                          --> FREAC = FTOT - (FEXT+FINT) - (Fgrav,Fbcs_cyclic,Fcentrif) - (Fdamping)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NODFT, NODLT
      my_real,INTENT(IN) :: A(3,NUMNOD) , AR(3,NUMNOD), MS(NUMNOD), IN(IRODDL*NUMNOD)
      my_real,INTENT(INOUT) :: FREAC(6,NUMNOD)
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
      
C-----------------------------------------------
      RETURN
      END SUBROUTINE REACTION_FORCES_3
