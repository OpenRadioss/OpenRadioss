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
!||    failini               ../starter/source/elements/solid/solide/failini.F
!||--- called by ------------------------------------------------------
!||    s10init3              ../starter/source/elements/solid/solide10/s10init3.F
!||    s16init3              ../starter/source/elements/thickshell/solide16/s16init3.F
!||    s20init3              ../starter/source/elements/solid/solide20/s20init3.F
!||    s4init3               ../starter/source/elements/solid/solide4/s4init3.F
!||    s6cinit3              ../starter/source/elements/thickshell/solide6c/s6cinit3.F
!||    s8cinit3              ../starter/source/elements/thickshell/solide8c/s8cinit3.F
!||    s8zinit3              ../starter/source/elements/solid/solide8z/s8zinit3.F
!||    scinit3               ../starter/source/elements/thickshell/solidec/scinit3.F
!||    sinit3                ../starter/source/elements/solid/solide/sinit3.F
!||    suinit3               ../starter/source/elements/elbuf_init/suinit3.F
!||--- calls      -----------------------------------------------------
!||    biquad_coefficients   ../starter/source/materials/fail/biquad/biquad_coefficients.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE FAILINI(
     .   ELBUF_STR,NPTR,NPTS,NPTT,NLAY,IPM,SIGSP,NSIGI,FAIL_INI,
     .   SIGI,NSIGS,IX,NIX,PT,RNOISE,PERTURB,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "param_c.inc"
#include       "vect01_c.inc"
#include       "com01_c.inc" 
#include       "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPTR,NPTS,NPTT,IPM(NPROPMI,*),NSIGI,NLAY,
     .   FAIL_INI(*),IX(NIX,*),NSIGS,NIX,PT(*),PERTURB(NPERTURB)
      my_real
     .   SIGSP(NSIGI,*),SIGI(NSIGS,*),RNOISE(NPERTURB,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,N,IIP,JPT, II, JJ, IPT, IPP,IUS,IPSU,
     .        IFLAGINI,JPS,IL,IR,IS,IT,NV,NVAR_RUPT,NVMAX,NFAIL,
     .        IP,IMAT,IRUP
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DFMAX
      TYPE(BUF_FAIL_),POINTER :: FBUF
      my_real
     .  c1,c2,c3,c4,c5,X_1(2),X_2(3)
C-----------------------------------------------
      IF (ISIGI /= 0 .AND. NVSOLID4 /= 0) THEN
C                     
        DO I=LFT,LLT 
c
         II=NFT+I
         JJ=PT(II)
         IF(JJ==0)CYCLE
c         
         DO IL=1,NLAY 
           NFAIL =  ELBUF_STR%BUFLY(IL)%NFAIL
           DO IUS=1,NFAIL
            JPS = NVSOLID1  + NVSOLID2 + 4 + NUSOLID + NVSOLID3 
            NVMAX = NVSOLID4 /(NPTR*NPTS*NPTT*NLAY*5)
            DO IR = 1,NPTR
             DO IS = 1,NPTS
              DO IT = 1,NPTT
                FBUF  => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT) 
                UVARF => FBUF%FLOC(IUS)%VAR     
                DFMAX => FBUF%FLOC(IUS)%DAMMX   
                NVAR_RUPT = FBUF%FLOC(IUS)%NVAR
                DFMAX(I)= SIGSP(JPS+1+(IUS-1)*NLAY*NPTR*NPTS*NPTT*NVMAX+
     .                    (IL-1)*NVMAX*NPTR*NPTS*NPTT,JJ)
                JPS = JPS + 1
                DO NV=1,NVAR_RUPT 
                  UVARF((NV-1)*LLT+I)=
     .               SIGSP(JPS+1+(IUS-1)*NLAY*NPTR*NPTS*NPTT*NVMAX+
     .                          (IL-1)*NVMAX*NPTR*NPTS*NPTT,JJ) 
                  JPS = JPS + 1
               ENDDO
              ENDDO
             ENDDO
            ENDDO 
           ENDDO
         ENDDO
        ENDDO
      ENDIF 
c-----------------------------
      IF( NPERTURB /= 0 ) THEN 
       DO J=1,NPERTURB
         IF(PERTURB(J) == 2)THEN
           DO I=LFT,LLT 
             IF (RNOISE(J,I+NFT) /= ZERO) THEN
               DO IL=1,NLAY   
                 NFAIL =  ELBUF_STR%BUFLY(IL)%NFAIL
                 IMAT  =  ELBUF_STR%BUFLY(IL)%IMAT
                 DO IUS=1,NFAIL                                  
                   IRUP = MAT_PARAM(IMAT)%FAIL(IUS)%IRUPT
                   IF (IRUP == 30) THEN           ! /FAIL/BIQUAD
                     MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(8) = 1
                     c1 = ZERO
                     c2 = ZERO
                     c3 = MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(9) * RNOISE(J,I+NFT)
                     c4 = ZERO
                     c5 = ZERO
                     L = INT(MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(10))
c
                     CALL BIQUAD_COEFFICIENTS(c1 ,c2 ,c3  ,c4  ,c5  ,L  ,
     .                                        X_1,X_2,ZERO,ZERO,ZERO,ZERO)
c
                     DO IT = 1,NPTT 
                       DO IS = 1,NPTS
                         DO IR = 1,NPTR 
                  FBUF  => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)
                  UVARF => FBUF%FLOC(IUS)%VAR
                  UVARF((3-1)*LLT+I) = C2
                  UVARF((4-1)*LLT+I) = X_1(1)
                  UVARF((5-1)*LLT+I) = X_1(2)
                  UVARF((6-1)*LLT+I) = X_2(1)
                  UVARF((7-1)*LLT+I) = X_2(2)
                  UVARF((8-1)*LLT+I) = X_2(3)
                         ENDDO
                       ENDDO
                     ENDDO
                   ENDIF
                 ENDDO
               ENDDO
             ENDIF
           ENDDO
         ENDIF
       ENDDO
      ENDIF
c-----------
      RETURN
      END
