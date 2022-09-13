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
Chd|  DSDYN                         source/implicit/dsolve/dsdyn.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|        DSEIG                         source/implicit/dsolve/dseig.F
Chd|        DSMSHIFT                      source/implicit/dsolve/dsdyn.F
Chd|====================================================================
      SUBROUTINE DSDYN(KL    , ML   , NDF  , NDI   , NM   ,
     .                 MAXITR, TOL  , VD   , NVMAX , NVD  ,
     .                 CUTFRQ, NBLF0, IPRI , IMDIAG, IPROC,
     .                 ISD   , ILVL , FREQ2, LDM   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "sysunit.inc"
*
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDF, NDI, NM, MAXITR, NVMAX, NVD, NBLF0, IPRI, IMDIAG,
     .        IPROC, ISD, ILVL, LDM
      my_real
     .        KL(NDI,*), ML(LDM,*), TOL, VD(NDI,*), CUTFRQ, FREQ2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISTOP, N, NEV, NCV, MAXN, MAXNEV, MAXNCV, LDV, ISHFTS,
     .        MODE, INFO, I, II, LEN, NBLF, IDBL, IDF, NUV, NBLF1,
     .        ISTOP1
      my_real
     .        SHIFT, TSHIFT, PULS, FRQ, FR1, FR2
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: KLS, VECT, FREQ
      CHARACTER WHICH*2
C
      IF (NDI==1) THEN
         NVD=1
         FREQ2(1)=SQRT(KL(1,1)/ML(1,1))/TWO/PI
         IF (IPRI>0) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,'(A26,I8)') ' NUMBER OF DYNAMIC MODES: ',NVD
            WRITE(IOUT,'(A)') '         NUMBER  EIGENVALUE   FREQUENCY'
            WRITE(IOUT,'(5X,I10,1X,1PG11.4,1X,1PG11.4)')
     .                    NVD,(TWO*PI*FREQ2(1))**2,FREQ2(1)
         ENDIF
         VD(1,1)=ONE/ML(1,1)
         RETURN
      ENDIF   
C
      NBLF=MIN(NBLF0,NDI-2)
      NBLF=MIN(NBLF,NVMAX)
C
      MAXNCV=NM*(NVMAX+1)
      ALLOCATE(KLS(NDI,NDI), VECT(NDI, MAXNCV), FREQ(MAXNCV,2))
C
C Suppression des modes rigides sur un superelement
C      SHIFT=ZERO
      SHIFT=(EM02/FAC_TIME*TWO*PI)**2
      TSHIFT=SHIFT
      ISTOP=0
      NVD=0
      ISTOP1=0
      DO WHILE (ISTOP==0)
         CALL DSMSHIFT(KLS, KL,  ML, TSHIFT, IMDIAG,
     .                 NDI, LDM)
         N=NDI
         NEV=MIN(NBLF+1,N-NVD)
         NCV=MIN(NM*NEV,N)
         MAXN=N
         MAXNEV=NEV
         LDV=MAXN
         WHICH='LA'
         ISHFTS=1
         MODE=3
         INFO=0
C
         CALL DSEIG(N,      NEV,    NCV,    WHICH,  INFO,
     .              MAXN,   MAXNEV, MAXNCV, LDV,    ISHFTS,
     .              MAXITR, MODE,   TOL,    KLS,    ML, 
     .              FREQ,   VECT,   0,      IMDIAG, TSHIFT,
     .              LDM   )
C
         NUV=NEV-1
         IF (NEV==N-NVD) NUV=NEV
C
         DO I=1,NUV
            PULS=MAX(ZERO,FREQ(I,1))
            FRQ=SQRT(PULS)/TWO/PI
            IF (FRQ<=CUTFRQ.AND.NVD<NVMAX.AND.NVD<NDI.AND.
     .          FREQ(I,1)>=TSHIFT) THEN
               NVD=NVD+1
               FREQ2(NVD)=FRQ
               DO II=1,NDI
                  VD(II,NVD)=VECT(II,I)
               ENDDO
               IF (NVD==NVMAX) THEN
                  IF (IPRI>0) THEN
*                     WRITE(ISTDO,'(A67,I5,A9,I5)') 
*     . ' ** INFO : MAXIMUM DYNAMIC MODES NUMBER REACHED FOR'//
*     . ' SUPERELEMENT ',ISD,' OF PROC ',IPROC
*                     WRITE(ISTDO,'(A23,I5,A11,I5)') 
*     . ' ** CONDENSATION LEVEL:',ILVL,' - NSVMAX: ',NVMAX
                     WRITE(IOUT,'(A67,I5,A9,I5)') 
     . ' ** INFO : MAXIMUM DYNAMIC MODES NUMBER REACHED FOR'//
     . ' SUPERELEMENT ',ISD,' OF PROC ',IPROC
                     WRITE(IOUT,'(A23,I5,A11,I5)') 
     . ' ** CONDENSATION LEVEL:',ILVL,' - NSVMAX: ',NVMAX
                  ENDIF
                  ISTOP=1
               ENDIF
            ELSE
               ISTOP=1
            ENDIF
         ENDDO
C
         IF (NVD==NDI.OR.ISTOP1==1) ISTOP=1
C
         IF (ISTOP==0) THEN
            FR1=FREQ(NEV,1)
            IDBL=0
            IDF=2
            DO WHILE (IDBL==0.AND.IDF<=NEV)
               FR2=FREQ(NEV-IDF+1,1)
               IF (ABS((FR2-FR1)/FR1)<=EM01) THEN
                  IDF=IDF+1
                  NVD=NVD-1
               ELSE
                  IDBL=1
               ENDIF
            ENDDO
            IF (IDF==NEV+1) THEN
               NBLF1=NBLF
               NBLF=2*NBLF
               NBLF=MIN(NBLF,NDI-2)
               IF (NBLF1==NBLF) ISTOP1=1
               SHIFT=ZERO
            ELSE
               NBLF=MIN(NBLF0,NDI-2)
               FR1=FREQ(NEV-IDF+2,1)
               SHIFT=FR2+(FR1-FR2)*ZEP9-TSHIFT
            ENDIF
            TSHIFT=TSHIFT+SHIFT
         ENDIF
      ENDDO
C
      IF (IPRI>0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,'(A26,I8)') ' NUMBER OF DYNAMIC MODES: ',NVD
         WRITE(IOUT,'(A)') '         NUMBER  EIGENVALUE   FREQUENCY'
         DO I=1,NVD
            WRITE(IOUT,'(5X,I10,1X,1PG11.4,1X,1PG11.4)')
     .                    I,(TWO*PI*FREQ2(I))**2,FREQ2(I)
         ENDDO
      ENDIF
C
C
      DEALLOCATE(KLS, VECT, FREQ)
C
      RETURN
      END
Chd|====================================================================
Chd|  DSMSHIFT                      source/implicit/dsolve/dsdyn.F
Chd|-- called by -----------
Chd|        DSDYN                         source/implicit/dsolve/dsdyn.F
Chd|        DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSMSHIFT(KLS, KL , ML, SHIFT, IMDIAG,
     .                    NDI, LDM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMDIAG, NDI, LDM
      my_real
     .        KLS(NDI,*), KL(NDI,*), ML(LDM,*), SHIFT      
     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C
      IF (IMDIAG==0) THEN
         DO I=1,NDI
            DO J=1,NDI
               KLS(I,J)=KL(I,J)-SHIFT*ML(I,J)
            ENDDO
         ENDDO
      ELSEIF (IMDIAG==1) THEN
         DO I=1,NDI
            DO J=1,NDI
               KLS(I,J)=KL(I,J)
            ENDDO
         ENDDO
         DO I=1,NDI
            KLS(I,I)=KL(I,I)-SHIFT*ML(I,1)
         ENDDO
      ENDIF
C
      RETURN
      END
      
           
