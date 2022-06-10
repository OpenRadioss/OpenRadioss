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
Chd|  FAIL_WINDSHIELD_INIT          source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        NGR2USRN                      source/system/nintrr.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_WINDSHIELD_INIT(
     .           ELBUF_STR,NEL      ,NFT      ,ITY      ,NGL      ,
     .           IPM      ,BUFMAT   ,IGRSH4N  ,IGRSH3N  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "param_c.inc"
#include       "scr17_c.inc"
#include       "com01_c.inc" 
#include       "com04_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT
      INTEGER IPM(NPROPMI,*),NGL(NEL)
      my_real ,DIMENSION(*), INTENT(IN) ,TARGET :: BUFMAT
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,KK,IEL,IL,IP,IR,IS,IT,NPTR,NPTS,NPTT,IFL,IAD,IADBUF,
     .   ITY,IGR,IGRID,NUMEL,IMAT,IRUPT,NFAIL,NUPAR,NUVAR,NINDX,IVAR,NUM
      INTEGER TAGSH(MAX(NUMELC,NUMELTG)),TAGEL(NEL),INDX(NEL)
      my_real ,DIMENSION(:), POINTER  :: UVAR, UPARAM
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(BUF_FAIL_),POINTER :: FBUF
      EXTERNAL  NGR2USRN
      INTEGER   NGR2USRN
c
c     Initialization of failure UVAR table => edge element flag for /FAIL/ALTER
C=======================================================================
      DO IL=1, ELBUF_STR%NLAY  
        BUFLY => ELBUF_STR%BUFLY(IL)
        NFAIL = BUFLY%NFAIL
        NPTR  = ELBUF_STR%NPTR
        NPTS  = ELBUF_STR%NPTS
        NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
        IMAT  = ELBUF_STR%BUFLY(IL)%IMAT
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
          FBUF => ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)  
          DO IFL = 1,NFAIL
            IRUPT = FBUF%FLOC(IFL)%ILAWF
c---
            IF (IRUPT == 28) THEN   ! windshield failure model
              IVAR = NEL*(10-1)     ! edge element flag = UVAR(10)
              IP   = (IFL - 1)*15                                             
              NUPAR  = IPM(112 + IP, IMAT)
              IADBUF = IPM(114 + IP ,IMAT)
              UPARAM => BUFMAT(IADBUF:IADBUF+NUPAR)
              IGR    = 0
              IF (ITY == 3) THEN
                KK = NGRNOD + NGRBRIC + NGRQUAD
                IGRID = UPARAM(12)
                UPARAM(11) = ONE / SQRT(PI)  ! GEORED for underintegrated 4N shells
                IF (IGRID > 0) IGR = NGR2USRN(IGRID,IGRSH4N,NGRSHEL,NUM)
                TAGSH(1:NUMELC)  = 0
              ELSEIF (ITY == 7) THEN
                IGRID = UPARAM(13)
                IF (IGRID > 0) IGR = NGR2USRN(IGRID,IGRSH3N,NGRSH3N,NUM)
                TAGSH(1:NUMELTG) = 0
              ENDIF
c
              IF (IGR > 0) THEN
                TAGEL(1:NEL) = 0
                IF (ITY == 3) THEN
                  NUMEL = IGRSH4N(IGR)%NENTITY
                  DO IEL=1,NUMEL
                    II = IGRSH4N(IGR)%ENTITY(IEL)
                    TAGSH(II) = 1
                  ENDDO
                ELSEIF (ITY == 7) THEN
                  NUMEL = IGRSH3N(IGR)%NENTITY
                  DO IEL=1,NUMEL
                    II = IGRSH3N(IGR)%ENTITY(IEL)
                    TAGSH(II) = 1
                  ENDDO
                ENDIF ! IF (ITY == 3)
                NINDX = 0
                DO I=1,NEL
c                  IEL = NGL(I)
                  IF (TAGSH(I+NFT) == 1) THEN
                    NINDX = NINDX + 1
                    INDX(NINDX) = I
                  ENDIF
                ENDDO
                DO II = 1,NINDX
                  I = INDX(II)            
                  FBUF%FLOC(IFL)%VAR(IVAR + I) = 1
                ENDDO
              ENDIF                
            ENDIF  !  IRUPT == 28
c---
          ENDDO    !  IFL = 1,NFAIL              
        ENDDO      !  IT=1,NPTT
        ENDDO      !  IT=1,NPTS
        ENDDO      !  IT=1,NPTR
      ENDDO        !  IL=1,NLAY   
c-----------
      RETURN
      END
