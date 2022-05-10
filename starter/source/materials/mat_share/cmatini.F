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
Chd|  CMATINI                       source/materials/mat_share/cmatini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CMATINI(ELBUF_STR, 
     1           JFT     ,JLT     ,NFT     ,NEL     ,NPT     ,
     2           ISTRAIN ,SIGSH   ,NUMEL   ,IX      ,NIX     ,
     3           NSIGSH  ,NUMSH   ,PTSH    ,IR      ,IS      ,
     4           NLAY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NUMEL,NIX,NFT,NPT,ISTRAIN,IR,IS,NLAY,NSIGSH,
     .   NEL,NUMSH
      INTEGER IX(NIX,*),PTSH(*)
      my_real
     .   SIGSH(NSIGSH,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
      INTEGER I,I1,II,J,JJ,N,NPTI,NU,NIP, NPG,IPT, 
     .        IPT_ALL,IT,ILAY,NPTT, PT
      my_real,
     .   DIMENSION(:), POINTER :: FACYLD 
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
       !print*, ' dans cmatini ' 
      DO I=JFT,JLT
        IF (ABS(ISIGI) /=3 .AND. ABS(ISIGI)/=4 .AND. ABS(ISIGI)/=5)THEN
          II = I+NFT
          N = NINT(SIGSH(NVSHELL + 1,II))
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
        NIP  = NINT(SIGSH(NVSHELL +  2,II))       
        NPG  = NINT(SIGSH(NVSHELL +  3,II))
!!        PT=NVSHELL+3  ! wrong position, overwriting FAIL
        PT = NVSHELL+NUSHELL+NORTSHEL+NVSHELL1+3
        !print*, 'npg, nip ', npg, nip
 
                                 
        IF (NPG <= 1) THEN                                      
          IF (NIP == 0) THEN
!----------------------------------------------------------
!!  ILAY --> not initialised here
!!  SIGSH(PT, II) --> not filled within initia for NIP = 0
!----------------------------------------------------------
!!             LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,1)
!!             LBUF%FAC_YLD(I) = SIGSH(PT, II)                                          
          ELSE                                                  
c

            IPT_ALL = 0
            DO ILAY=1,NLAY
              NPTT  = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                IPT = IPT_ALL + IT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)   
                LBUF%FAC_YLD(I) = SIGSH(PT + IPT , II)
                  !print*, ' IT, IPT ,indx',  IT, IPT,PT+ IPT
                  !print*, ' sig ', SIGSH(PT + IPT , II)

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
