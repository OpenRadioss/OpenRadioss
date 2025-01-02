Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    integratorl   ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE INTEGRATORL(NODFT,NODLT,D     ,DR    ,
     1                      X    ,V    ,VR     ,A     ,AR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT 
C     REAL
      my_real
     . D(3,*),DR(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real
     .  DTI
C------- integrateur ---------- 
      DTI = ONE/DT2
           DO I=NODFT,NODLT
            V(1,I)=D(1,I)*DTI
            V(2,I)=D(2,I)*DTI
            V(3,I)=D(3,I)*DTI
            A(1,I)=ZERO
            A(2,I)=ZERO
            A(3,I)=ZERO
           ENDDO
           IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
            VR(1,I)=DR(1,I)*DTI
            VR(2,I)=DR(2,I)*DTI
            VR(3,I)=DR(3,I)*DTI
            AR(1,I)=ZERO
            AR(2,I)=ZERO
            AR(3,I)=ZERO
            ENDDO
           ENDIF
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator    ../engine/source/implicit/integrator.F
      !||--- calls      -----------------------------------------------------
      !||    inte_dyna     ../engine/source/implicit/imp_dyna.F
      !||    integrator0   ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE INTEGRATOR(NODFT,NODLT ,NDT   ,D    ,DR    ,
     1                      X     ,V    ,VR    ,A    ,AR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,NDT
C     REAL
      my_real
     . D(3,*),DR(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real
     .  DTI
C------- integrateur ---------- 
      IF (IDYNA>0) CALL INTE_DYNA(NODFT,NODLT ,
     .                               D    ,DR    ,V    ,VR    )
      DTI = ONE/DT2/NDT
       DO I=NODFT,NODLT
        V(1,I)=DTI*D(1,I)
        V(2,I)=DTI*D(2,I)
        V(3,I)=DTI*D(3,I)
       ENDDO
       IF (IRODDL/=0) THEN
        DO I=NODFT,NODLT
         VR(1,I)=DTI*DR(1,I)
         VR(2,I)=DTI*DR(2,I)
         VR(3,I)=DTI*DR(3,I)
        ENDDO
       ENDIF
C
      CALL INTEGRATOR0(NODFT,NODLT,V     ,X    )
      DO I=NODFT,NODLT
       A(1,I)=ZERO
       A(2,I)=ZERO
       A(3,I)=ZERO
      ENDDO
      IF (IRODDL/=0) THEN
       DO I=NODFT,NODLT
        AR(1,I)=ZERO
        AR(2,I)=ZERO
        AR(3,I)=ZERO
       ENDDO
      ENDIF
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator0     ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    integrator      ../engine/source/implicit/integrator.F
      !||    integrator_hp   ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE INTEGRATOR0(NODFT,NODLT,V     ,X    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT 
C     REAL
      my_real
     . V(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C------- integrateur de base--------- 
           DO I=NODFT,NODLT
            X(1,I)=X(1,I)+V(1,I)*DT2
            X(2,I)=X(2,I)+V(2,I)*DT2
            X(3,I)=X(3,I)+V(3,I)*DT2
           ENDDO
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator1   ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE INTEGRATOR1(NODFT,NODLT,D     ,X    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT 
C     REAL
      my_real
     . D(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
C------- integrateur de base--------- 
           DO I=NODFT,NODLT
            X(1,I)=X(1,I)+D(1,I)
            X(2,I)=X(2,I)+D(2,I)
            X(3,I)=X(3,I)+D(3,I)
           ENDDO
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator2       ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    al_constraint1    ../engine/source/implicit/nl_solv.F
      !||    al_constrainth1   ../engine/source/implicit/nl_solv.F
      !||====================================================================
      SUBROUTINE INTEGRATOR2(NODFT,NODLT,IDDL,NDOF,IKC,D,DR,DD,DDR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  D(3,*),DR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)+DD(J,I)
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)+DDR(J,I)
              ENDDO
             ENDIF
            ENDDO
          ENDIF
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    frac_d   ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE FRAC_D(NODFT,NODLT,IDDL,NDOF,IKC,D,DR,FAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  FAC,D(3,*),DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID
C-----------------------------------------------
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)*FAC
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)*FAC
              ENDDO
             ENDIF
            ENDDO
          ENDIF
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    frac_dd           ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    al_constraint1    ../engine/source/implicit/nl_solv.F
      !||    al_constrainth1   ../engine/source/implicit/nl_solv.F
      !||====================================================================
      SUBROUTINE FRAC_DD(NODFT,NODLT,IDDL,NDOF,IKC,D,DR,DD,DDR,FAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  FAC,D(3,*),DR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID
C-----------------------------------------------
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)+FAC*DD(J,I)
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)+FAC*DDR(J,I)
              ENDDO
             ENDIF
            ENDDO
          ENDIF
C--------------------------------------------
      RETURN
      END
C-----_HP: Hybrid SMP // inside     
      !||====================================================================
      !||    integratorl_hp   ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    imp_solv         ../engine/source/implicit/imp_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini       ../engine/source/implicit/imp_solv.F
      !||====================================================================
      SUBROUTINE INTEGRATORL_HP(D     ,DR    ,
     1                          X    ,V    ,VR     ,A     ,AR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     . D(3,*),DR(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT,ITSK
      INTEGER I,K
      my_real
     .  DTI
C------- integrateur ---------- 
      DTI = ONE/DT2
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
           DO I=NODFT,NODLT
            V(1,I)=D(1,I)*DTI
            V(2,I)=D(2,I)*DTI
            V(3,I)=D(3,I)*DTI
            A(1,I)=ZERO
            A(2,I)=ZERO
            A(3,I)=ZERO
           ENDDO
           IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
            VR(1,I)=DR(1,I)*DTI
            VR(2,I)=DR(2,I)*DTI
            VR(3,I)=DR(3,I)*DTI
            AR(1,I)=ZERO
            AR(2,I)=ZERO
            AR(3,I)=ZERO
            ENDDO
           ENDIF
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator_hp   ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    imp_solv        ../engine/source/implicit/imp_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini      ../engine/source/implicit/imp_solv.F
      !||    inte_dyna       ../engine/source/implicit/imp_dyna.F
      !||    integrator0     ../engine/source/implicit/integrator.F
      !||====================================================================
      SUBROUTINE INTEGRATOR_HP(NDT   ,D    ,DR    ,
     1                         X     ,V     ,VR    ,A    ,AR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDT
C     REAL
      my_real
     . D(3,*),DR(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT,ITSK
      INTEGER I,K
      my_real
     .  DTI
C------- integrateur ---------- 
      DTI = ONE/DT2/NDT
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
      IF (IDYNA>0) CALL INTE_DYNA(NODFT,NODLT ,
     .                               D    ,DR    ,V    ,VR    )
       DO I=NODFT,NODLT
        V(1,I)=DTI*D(1,I)
        V(2,I)=DTI*D(2,I)
        V(3,I)=DTI*D(3,I)
       ENDDO
       IF (IRODDL/=0) THEN
        DO I=NODFT,NODLT
         VR(1,I)=DTI*DR(1,I)
         VR(2,I)=DTI*DR(2,I)
         VR(3,I)=DTI*DR(3,I)
        ENDDO
       ENDIF
C
      CALL INTEGRATOR0(NODFT,NODLT,V     ,X    )
      DO I=NODFT,NODLT
       A(1,I)=ZERO
       A(2,I)=ZERO
       A(3,I)=ZERO
      ENDDO
      IF (IRODDL/=0) THEN
       DO I=NODFT,NODLT
        AR(1,I)=ZERO
        AR(2,I)=ZERO
        AR(3,I)=ZERO
       ENDDO
      ENDIF
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator1_hp   ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    imp_solv         ../engine/source/implicit/imp_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini       ../engine/source/implicit/imp_solv.F
      !||====================================================================
      SUBROUTINE INTEGRATOR1_HP(D     ,X    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     . D(3,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,ITSK
      INTEGER I,K
C------- integrateur de base--------- 
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
           DO I=NODFT,NODLT
            X(1,I)=X(1,I)+D(1,I)
            X(2,I)=X(2,I)+D(2,I)
            X(3,I)=X(3,I)+D(3,I)
           ENDDO
!$OMP END PARALLEL 
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    integrator2_hp      ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    al_constraint1_hp   ../engine/source/implicit/nl_solv.F
      !||    nl_solv             ../engine/source/implicit/nl_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini          ../engine/source/implicit/imp_solv.F
      !||====================================================================
      SUBROUTINE INTEGRATOR2_HP(IDDL,NDOF,IKC,D,DR,DD,DDR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  D(3,*),DR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,ITSK
      INTEGER I,J,ID
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,ID,I,J)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)+DD(J,I)
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)+DDR(J,I)
              ENDDO
             ENDIF
            ENDDO
          ENDIF
!$OMP END PARALLEL
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    frac_d_hp    ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    nl_solv      ../engine/source/implicit/nl_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini   ../engine/source/implicit/imp_solv.F
      !||====================================================================
      SUBROUTINE FRAC_D_HP(IDDL,NDOF,IKC,D,DR,FAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  FAC,D(3,*),DR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,ITSK
      INTEGER I,J,ID
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,ID,I,J)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)*FAC
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)*FAC
              ENDDO
             ENDIF
            ENDDO
          ENDIF
!$OMP END PARALLEL
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    frac_dd_hp          ../engine/source/implicit/integrator.F
      !||--- called by ------------------------------------------------------
      !||    al_constraint1_hp   ../engine/source/implicit/nl_solv.F
      !||    nl_solv             ../engine/source/implicit/nl_solv.F
      !||--- calls      -----------------------------------------------------
      !||    imp_smpini          ../engine/source/implicit/imp_solv.F
      !||====================================================================
      SUBROUTINE FRAC_DD_HP(IDDL,NDOF,IKC,D,DR,DD,DDR,FAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*) ,NDOF(*)
C     REAL
      my_real
     .  FAC,D(3,*),DR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODFT,NODLT ,ITSK
      INTEGER I,J,ID
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I,J,ID)
      CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT ,NUMNOD )
          DO I=NODFT,NODLT
           DO J=1,3
            ID = IDDL(I)+J
            IF (IKC(ID)==0) D(J,I)=D(J,I)+FAC*DD(J,I)
           ENDDO
          ENDDO
          IF (IRODDL/=0) THEN
            DO I=NODFT,NODLT
             IF (NDOF(I)>3) THEN
              DO J=1,3
               ID = IDDL(I)+J+3
               IF (IKC(ID)==0) DR(J,I)=DR(J,I)+FAC*DDR(J,I)
              ENDDO
             ENDIF
            ENDDO
          ENDIF
!$OMP END PARALLEL
C--------------------------------------------
      RETURN
      END
