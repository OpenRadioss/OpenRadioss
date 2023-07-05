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
Chd|  CUSERINI                      source/elements/shell/coque/cuserini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CUSERINI(ELBUF_STR,
     1           JFT     ,JLT     ,NFT     ,NEL     ,NPT     ,
     2           ISTRAIN ,SIGSH   ,NUMEL   ,IX      ,NIX     ,
     3           NSIGSH  ,NUMSH   ,PTSH    ,IR      ,IS      ,
     4           NLAY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   ROUTINE GENERIQUE 4NOEUDS-3NOEUDS 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NUMEL,NIX,NFT,NPT,ISTRAIN,IR,IS,NLAY,NSIGSH,
     .   NEL,NUMSH,ILAW
      INTEGER IX(NIX,*),PTSH(*)
      my_real
     .   SIGSH(NSIGSH,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,KK,N,NPTI,NU,NIP,NUVAR,NVARS,NPG,IPT,
     .        IPT_ALL,IT,ILAY,NPTT,L_SIGB
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real, DIMENSION(:), POINTER :: UVAR,SIGA,SIGB,SIGC
C=======================================================================
      DO I=JFT,JLT
        IF (ABS(ISIGI) /=3 .AND. ABS(ISIGI)/=4 .AND. ABS(ISIGI)/=5)THEN
          II = I+NFT
          N = NINT(SIGSH(1,II))
          IF(N == IX(NIX,II))THEN
            JJ = II
          ELSE
            JJ = II
            DO J = 1,NUMEL
              II= J
              N = NINT(SIGSH(1,II))
              IF(N == 0) GOTO 200
              IF(N == IX(NIX,JJ))GOTO 70
            ENDDO
            GOTO 200
 70         CONTINUE
          ENDIF
        ELSE
          JJ=NFT+I
          N =IX(NIX,JJ)
          II=PTSH(JJ)
          IF(II == 0)GOTO 200
        END IF
        NIP  = NINT(SIGSH(NVSHELL + 2,II))       
        NPG  = NINT(SIGSH(NVSHELL + 3,II))
        NVARS= NINT(SIGSH(NVSHELL + 4,II))
        NUVAR = ELBUF_STR%BUFLY(1)%NVAR_MAT 
c
        IF (ELBUF_STR%BUFLY(1)%ILAW == 36) THEN  ! backstress is no more stored in uvar
          L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
          IF (NVARS > 3 .and. NIP > 0 .and. L_SIGB > 0) THEN
            IPT_ALL = 0
            DO ILAY=1,NLAY
              BUFLY => ELBUF_STR%BUFLY(ILAY)
              NPTT  = BUFLY%NPTT
              NUVAR = BUFLY%NVAR_MAT
              DO IT=1,NPTT
                SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
                IPT = IPT_ALL + IT
                DO J = 1,3
                  JJ = (J-1)*NEL + I
                  SIGB(JJ) = SIGSH(NVSHELL + 4 + (IPT -1)*NVARS + J ,II)
                ENDDO
              ENDDO
              IPT_ALL = IPT_ALL + NPTT
            ENDDO  !  DO ILAY=1,NPT
          ENDIF
c
        ELSE IF (ELBUF_STR%BUFLY(1)%ILAW == 78) THEN  ! backstress is no more stored in uvar
          L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
          NUVAR  = ELBUF_STR%BUFLY(1)%NVAR_MAT
          IPT_ALL = 0
          DO ILAY=1,NLAY
            BUFLY => ELBUF_STR%BUFLY(ILAY)
            NPTT  = BUFLY%NPTT
            DO IT=1,NPTT
              IPT  = IPT_ALL + IT
              UVAR => BUFLY%MAT(IR,IS,IT)%VAR
              SIGA => BUFLY%LBUF(IR,IS,IT)%SIGA
              SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
              SIGC => BUFLY%LBUF(IR,IS,IT)%SIGC
              KK = NVSHELL + 4 + (IPT-1)*NVARS
              DO NU = 1,NUVAR
                JJ = (NU-1)*NEL + I
                UVAR(JJ) = SIGSH(KK + NU,II)
              ENDDO
              KK = KK + NUVAR
              DO J = 1,L_SIGB
                JJ = (J-1)*NEL + I
                SIGA(JJ) = SIGSH(KK + J ,II)
              ENDDO
              DO J = 1,L_SIGB
                JJ = (J-1)*NEL + I
                SIGB(JJ) = SIGSH(KK + L_SIGB + J ,II)
              ENDDO
              DO J = 1,L_SIGB
                JJ = (J-1)*NEL + I
                SIGC(JJ) = SIGSH(KK + L_SIGB*2 + J ,II)
              ENDDO
            ENDDO
            IPT_ALL = IPT_ALL + NPTT
          ENDDO  !  DO ILAY=1,NPT
c
c
        ELSE IF (ELBUF_STR%BUFLY(1)%ILAW == 87) THEN  ! backstress is no more stored in uvar
          L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
          NUVAR  = ELBUF_STR%BUFLY(1)%NVAR_MAT
          IPT_ALL = 0
          DO ILAY=1,NLAY
            BUFLY => ELBUF_STR%BUFLY(ILAY)
            NPTT  = BUFLY%NPTT
            DO IT=1,NPTT
              IPT  = IPT_ALL + IT
              UVAR => BUFLY%MAT(IR,IS,IT)%VAR
              SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
              KK = NVSHELL + 4 + (IPT-1)*NVARS

              DO NU = 1,NUVAR
                JJ = (NU-1)*NEL + I
                UVAR(JJ) = SIGSH(KK + NU,II)
              ENDDO
              KK = KK + NUVAR
              DO J = 1,L_SIGB
                JJ = (J-1)*NEL + I
                SIGB(JJ) = SIGSH(KK + J ,II)
              ENDDO
            ENDDO
            IPT_ALL = IPT_ALL + NPTT
          ENDDO  !  DO ILAY=1,NPT
c
        ELSE IF (NPG <= 1) THEN                                      
          IF (NIP == 0) THEN
            UVAR => ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VAR                         
            DO NU = 1,MIN(NVARS,NUVAR) 
              UVAR((NU -1)*NEL + I) = SIGSH(NVSHELL + 4 + NU, II)
            ENDDO                                                
          ELSE                                                  
            IPT_ALL = 0
            DO ILAY=1,NLAY
              NPTT  = ELBUF_STR%BUFLY(ILAY)%NPTT
              NUVAR = ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
              DO IT=1,NPTT
                IPT = IPT_ALL + IT
                UVAR => ELBUF_STR%BUFLY(ILAY)%MAT(IR,IS,IT)%VAR
                DO NU = 1,MIN(NVARS,NUVAR) 
                  UVAR((NU -1)*NEL + I) = 
     .            SIGSH(NVSHELL + 4  + NU + (IPT -1)*NVARS , II)
                ENDDO
              ENDDO
              IPT_ALL = IPT_ALL + NPTT
            ENDDO  !  DO ILAY=1,NPT
c
          ENDIF                                                 
        ENDIF                                                   
 200    CONTINUE
      ENDDO
C-----------
      RETURN
      END
