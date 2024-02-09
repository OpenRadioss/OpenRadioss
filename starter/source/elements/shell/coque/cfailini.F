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
Chd|  CFAILINI                      source/elements/shell/coque/cfailini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        BIQUAD_COEFFICIENTS           source/materials/fail/biquad/biquad_coefficients.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE CFAILINI(ELBUF_STR,MAT_PARAM,
     .           NPTT    ,NLAY     ,SIGSH    ,NSIGSH  ,PTSH    ,
     .           RNOISE  ,PERTURB  ,ALDT     ,THK     )
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
      INTEGER NPTT,NLAY,NSIGSH,PERTURB(NPERTURB),PTSH(*)
      my_real
     .   SIGSH(NSIGSH,*),RNOISE(NPERTURB,*),ALDT(*),THK(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IIP,JPT, IFL,II, JJ, IPT, IPP,IUS,IPSU,
     .        IFLAGINI,JPS,IL,IT,NV,NVAR_RUPT,NVMAX,
     .        NFAIL,N,IMAT,L,IRUP
      my_real, DIMENSION(:), POINTER :: UVARF,DFMAX
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(BUF_FAIL_),POINTER :: FBUF
      my_real :: c1,c2,c3,c4,c5,X_1(2),X_2(3)
C=======================================================================
c     UVAR INITIALIZATION 
c------------------------
      DO IL=1,NLAY                                                      
        BUFLY => ELBUF_STR%BUFLY(IL)
        NFAIL = BUFLY%NFAIL                               
        DO IFL=1,NFAIL
          IRUP = BUFLY%FAIL(1,1,1)%FLOC(IFL)%ILAWF
c
          IF (IRUP == 23) THEN     ! /fail/tab
            DO IT = 1,NPTT                                             
              FBUF  => BUFLY%FAIL(1,1,IT)                   
              UVARF => FBUF%FLOC(IFL)%VAR                   
              DO I = LFT,LLT                                
                UVARF(LLT  +I) = THK(I)     ! UVAR(2)       
                UVARF(LLT*4+I) = ALDT(I)    ! UVAR(5)       
                UVARF(LLT*5+I) = ONE         ! UVAR(6) = IPOS   
                UVARF(LLT*6+I) = ONE         ! UVAR(7) = IPOS          
                UVARF(LLT*7+I) = ONE         ! UVAR(8) = IPOS          
              ENDDO                                                            
            ENDDO                                                          
          END IF
c          
        END DO
      END DO   ! NLAY                                                   
c------------------------
      IF( NVSHELL1 /= 0 ) THEN
       IF (ISIGI /= 0) THEN
C                     
        DO I=LFT,LLT 
          II = I+NFT
          JJ=PTSH(II)
          IF(JJ == 0)CYCLE
          DO IL=1,NLAY   
            NFAIL =  ELBUF_STR%BUFLY(IL)%NFAIL
            DO IUS=1,NFAIL
              JPS = NVSHELL + NUSHELL + 3  + NORTSHEL
              NVMAX = NVSHELL1 /(NPTT*NLAY*5)
              DO IT = 1,NPTT
                FBUF  => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IT)
                UVARF => FBUF%FLOC(IUS)%VAR
                DFMAX => FBUF%FLOC(IUS)%DAMMX  
                NVAR_RUPT = FBUF%FLOC(IUS)%NVAR
                DFMAX(I)= SIGSH(JPS+1+(IUS-1)*NLAY*NPTT*NVMAX+
     .                  (IL-1)*NVMAX*NPTT,JJ)
              JPS = JPS + 1
                DO NV=1,NVAR_RUPT
                  UVARF((NV-1)*LLT+I)=
     .            SIGSH(JPS+1+(IUS-1)*NLAY*NPTT*NVMAX+(IL-1)*NVMAX*NPTT,JJ)
                  JPS = JPS + 1
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDIF 
      ENDIF 
c
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
                   IF (IRUP == 30) THEN    ! /FAIL/BIQUAD
                     MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(8) = 1  ! flag PERTURB
                     c1 = ZERO
                     c2 = ZERO
                     c3 = MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(9) * RNOISE(J,I+NFT)
                     c4 = ZERO
                     c5 = ZERO
                     L = INT(MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(10))
c
                     CALL BIQUAD_COEFFICIENTS(c1,c2,c3,c4,c5,L,X_1,X_2,ZERO,ZERO,ZERO,ZERO)
c
                     DO IT = 1,NPTT  
                       FBUF  => ELBUF_STR%BUFLY(IL)%FAIL(1,1,IT)
                       UVARF => FBUF%FLOC(IUS)%VAR
                       UVARF((3-1)*LLT+I) = C2
                       UVARF((4-1)*LLT+I) = X_1(1)
                       UVARF((5-1)*LLT+I) = X_1(2)
                       UVARF((6-1)*LLT+I) = X_2(1)
                       UVARF((7-1)*LLT+I) = X_2(2)
                       UVARF((8-1)*LLT+I) = X_2(3)
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
Chd|====================================================================
Chd|  CFAILINI4                     source/elements/shell/coque/cfailini.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        BIQUAD_COEFFICIENTS           source/materials/fail/biquad/biquad_coefficients.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE CFAILINI4(
     .           ELBUF_STR,NPTR    ,NPTS    ,NPTT     ,NLAY    ,
     .           SIGSH    ,NSIGSH  ,PTSH    ,RNOISE   ,PERTURB ,
     .           MAT_PARAM,ALDT    ,THK     )
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
      INTEGER NPTR,NPTS,NPTT,NSIGSH,NLAY,
     .   PTSH(*),PERTURB(NPERTURB)
      my_real
     .   SIGSH(NSIGSH,*),RNOISE(NPERTURB,*),ALDT(*),THK(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IIP,JPT,IFL, II, JJ, IPT, IPP,IUS,IPSU,
     .        IFLAGINI,JPS,IL,IR,IS,IT,NV,NVAR_RUPT,NVMAX,NFAIL,N,
     .        IMAT,L,IRUP
      my_real ,DIMENSION(:), POINTER  :: UVARF,DFMAX
      my_real :: c1,c2,c3,c4,c5,X_1(2),X_2(3)
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(BUF_FAIL_),POINTER :: FBUF
C=======================================================================
c     UVAR INITIALIZATION 
c------------------------
      DO IL=1,NLAY                                                      
        BUFLY => ELBUF_STR%BUFLY(IL)
        NFAIL = BUFLY%NFAIL                               
        DO IFL=1,NFAIL
          IRUP = BUFLY%FAIL(1,1,1)%FLOC(IFL)%ILAWF
c
          IF (IRUP == 23) THEN     ! /fail/tab
            DO IT = 1,NPTT                                             
              DO IS = 1,NPTS                                            
                DO IR = 1,NPTR                                           
                  FBUF  => BUFLY%FAIL(IR,IS,IT)
                  UVARF => FBUF%FLOC(IFL)%VAR
                  DO I = LFT,LLT
                    UVARF(LLT  +I) = THK(I)     ! UVAR(2)
                    UVARF(LLT*4+I) = ALDT(I)    ! UVAR(5) 
                    UVARF(LLT*5+I) = ONE         ! UVAR(6) = IPOS   
                    UVARF(LLT*6+I) = ONE         ! UVAR(7) = IPOS      
                    UVARF(LLT*7+I) = ONE         ! UVAR(8) = IPOS      
                  ENDDO                                                        
                ENDDO                                                        
              ENDDO                                                         
            ENDDO                                                          
          END IF
c          
        END DO
      END DO   ! NLAY                                                   
c------------------------
      IF( NVSHELL1 /= 0 ) THEN
        IF (ISIGI /= 0) THEN
          DO I=LFT,LLT 
            II = I+NFT
            JJ = PTSH(II)
            IF(JJ == 0)CYCLE
            DO IL=1,NLAY
             NFAIL =  ELBUF_STR%BUFLY(IL)%NFAIL
             DO IUS=1,NFAIL
               JPS = NVSHELL + NUSHELL + 3 + NORTSHEL
               NVMAX = NVSHELL1 /(MAX(1,NPTR)*MAX(1,NPTS)*MAX(1,NPTT)*
     .                  MAX(1,NLAY)*5)
               DO IT = 1,NPTT
                DO IS = 1,NPTS
                 DO IR = 1,NPTR
           FBUF  => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT) 
           UVARF => FBUF%FLOC(IUS)%VAR
                   DFMAX => FBUF%FLOC(IUS)%DAMMX   
           NVAR_RUPT = FBUF%FLOC(IUS)%NVAR
                   DFMAX(I)=SIGSH(JPS+1+(IUS-1)*NLAY*NPTR*NPTS*NPTT*NVMAX+
     .                     (IL-1)*NVMAX*NPTR*NPTS*NPTT,JJ)
           JPS = JPS + 1
           DO NV=1,NVAR_RUPT 
             UVARF((NV-1)*LLT+I)=
     .                  SIGSH(JPS+1+(IUS-1)*NLAY*NPTR*NPTS*NPTT*NVMAX+
     .                     (IL-1)*NVMAX*NPTR*NPTS*NPTT,JJ)
             JPS = JPS + 1
           ENDDO
                 ENDDO
                ENDDO
               ENDDO
             ENDDO
            ENDDO
          ENDDO
        ENDIF 
      ENDIF 
c------------------------
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
                   IF (IRUP == 30) THEN
                     MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(8) = 1
                     c1 = ZERO
                     c2 = ZERO
                     c3 = MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(9) * RNOISE(J,I+NFT)
                     c4 = ZERO
                     c5 = ZERO
                     L = INT(MAT_PARAM(IMAT)%FAIL(IUS)%UPARAM(10))
c
                     CALL BIQUAD_COEFFICIENTS(c1,c2,c3,c4,c5,L,X_1,X_2,ZERO,ZERO,ZERO,ZERO)
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
