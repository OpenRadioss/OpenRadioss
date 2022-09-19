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
Cf51e6 Nouvelle routine
Chd|====================================================================
Chd|  DSSTAT                        source/implicit/dsolve/dsstat.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSSTAT(KLC   , KLI , NDI , NDF, VS ,
     .                  IPRI  , IOPT, NSCM, S  , IFI,
     .                  INFFAC, IPIV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDI, NDF, IPRI, IOPT, NSCM, INFFAC, IPIV(*)
      my_real
     .        KLC(NDF,*), KLI(NDI,*),VS(NDI,*),
     .        S(NSCM,*), IFI(NSBMAX,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IAD, J, NRHS, INFRES, LFACW
      my_real
*     .        F(NDI,MAX(NDF,NSCM)), B(NDI), RLFACW
     .        B(NDI), RLFACW
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: F
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FACW
C
      ALLOCATE(F(NDI,MAX(NDF,NSCM)))
C-----------------------
C     CALCUL DES SECONDS MEMBRES
C-----------------------
      DO I=1,NDI
         DO J=1,NDF
            F(I,J)=-KLC(J,I)
         ENDDO
      ENDDO
C-----------------------
C     FACTORISATION DE LA MATRICE DE RIGIDITE SUR DDLS INTERNES
C-----------------------
      INFFAC=0
      IF (NDI>0) THEN
C Block size query
         LFACW=-1
         CALL DSYTRF('U', NDI, KLI, NDI, IPIV, RLFACW, LFACW, INFFAC)
         LFACW=NINT(RLFACW)
         ALLOCATE(FACW(LFACW))
C Factorization
         CALL DSYTRF('U', NDI, KLI, NDI, IPIV, FACW, LFACW, INFFAC)
      ENDIF
      IF (INFFAC>0) RETURN
C-----------------------
C     RESOLUTIONS
C-----------------------
      NRHS=NDF
      IF (NDI>0)
     .   CALL DSYTRS('U', NDI, NRHS, KLI, NDI, IPIV, F, NDI, INFRES)
      DO I=1,NDI
         DO J=1,NDF
            VS(I,J)=F(I,J)
         ENDDO
      ENDDO
C
      IF (IPRI>0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,'(A25,I8)') ' NUMBER OF STATIC MODES: ',NDF
         WRITE(IOUT,*)
      ENDIF
C
      IF (IOPT==2) THEN
         IF (NDI>0) DEALLOCATE(FACW)
         RETURN
      ENDIF
C
      DO I=1,NDI
         DO J=1,NSCM
            F(I,J)=S(J,I)
         ENDDO
      ENDDO
      NRHS=NSCM
      IF (NDI>0)
     .   CALL DSYTRS('U', NDI, NRHS, KLI, NDI, IPIV, F, NDI, INFRES)
      DO I=1,NDI
         DO J=1,NSCM
            IFI(J,I)=F(I,J)
         ENDDO
      ENDDO
C
      DEALLOCATE(F)
      IF (NDI>0) DEALLOCATE(FACW)
      RETURN
      END

