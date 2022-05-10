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
Chd|  THICKVAR                      source/elements/shell/coque/thickvar.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THICKVAR(IPARG,ELBUF_TAB,IXC,IXTG,THKSH4_VAR,
     .                    THKSH3_VAR,THKNOD,THKE,SH4TREE,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "scr01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
C     REAL ou REAL*8
      my_real
     .   THKSH4_VAR(*), THKSH3_VAR(*), THKNOD(*),
     .   THKE(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, N, NEL, ISH3N, IHBE, ITHK,
     .        LE,LELT,NELT(2*(4**LEVELMAX)),LEV,NE,SON,LELT1,LELT2
       my_real
     .        THK
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
      IF(NADMESH==0)THEN
 250  CONTINUE
#include "lockon.inc"
      IF(NSGDONE.GT.NGROUP) THEN
#include "lockoff.inc"
         GOTO 252
      ENDIF
      NG=NSGDONE
      NSGDONE = NG + 1
#include "lockoff.inc"
      IF (IPARG(8,NG) == 1)GOTO 250
      ITY   =IPARG(5,NG)
      IF(ITY.NE.3.AND.ITY.NE.7)GOTO 250

        IF (IDDW.GT.0) CALL STARTIMEG(NG)
        MTN=IPARG(1,NG)
        IF(MTN == 0) GOTO 250
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ITY=IPARG(5,NG)
        IHBE   =IPARG(23,NG)
        ISH3N  =IPARG(23,NG)
        ITHK    = IPARG(28,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
c
        IF (ITY == 3) THEN
          IF(ITHK==0)THEN
            DO I=1,NEL
              N=NFT+I
              THKSH4_VAR(N)=THKE(N)
              DO J=2,5
                THKNOD(IXC(J,N))=MAX(THKNOD(IXC(J,N)),THKSH4_VAR(N))
              END DO
            END DO
          ELSE
            DO I=1,NEL
              N=NFT+I
              THKSH4_VAR(N)=GBUF%THK(I)
              DO J=2,5
                THKNOD(IXC(J,N))=MAX(THKNOD(IXC(J,N)),THKSH4_VAR(N))
              END DO
            END DO
          END IF
        ELSEIF(ITY == 7)THEN
          IF(ITHK==0)THEN
            DO I=1,NEL
              N=NFT+I
              THKSH3_VAR(N)=THKE(NUMELC+N)
              DO J=2,4
                THKNOD(IXTG(J,N))=MAX(THKNOD(IXTG(J,N)),THKSH3_VAR(N))
              END DO
            END DO
          ELSE
            DO I=1,NEL
              N=NFT+I
              THKSH3_VAR(N)=GBUF%THK(I)
              DO J=2,4
                THKNOD(IXTG(J,N))=MAX(THKNOD(IXTG(J,N)),THKSH3_VAR(N))
              END DO
            END DO
          END IF
        END IF

        IF (IDDW.GT.0) CALL STOPTIMEG(NG)
      GOTO 250
 252  CONTINUE
C-----------------------------------------------
      ELSE
 450  CONTINUE
#include "lockon.inc"
      IF(NSGDONE.GT.NGROUP) THEN
#include "lockoff.inc"
         GOTO 452
      ENDIF
      NG=NSGDONE
      NSGDONE = NG + 1
#include "lockoff.inc"
      IF(IPARG(8,NG) == 1)GOTO 450
      ITY   =IPARG(5,NG)
      IF(ITY.NE.3.AND.ITY.NE.7)GOTO 450

        IF (IDDW.GT.0) CALL STARTIMEG(NG)
        MTN=IPARG(1,NG)
        IF(MTN == 0) GOTO 450
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ITY=IPARG(5,NG)
        IHBE   =IPARG(23,NG)
        ISH3N  =IPARG(23,NG)
        ITHK    = IPARG(28,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
c
        IF (ITY == 3) THEN
          DO I=1,NEL
            N=NFT+I

            LEV=SH4TREE(3,N)
            IF(LEV < 0 .OR. GBUF%OFF(I)==ZERO) CYCLE

            IF(ITHK==0)THEN
              THK=THKE(N)
            ELSE
              THK=GBUF%THK(I)
            END IF

            LELT   =1
            NELT(1)=N
   
            LELT1  =0
            LELT2  =1
                  
            DO WHILE (LEV < LEVELMAX)

              DO LE=LELT1+1,LELT2
  
                NE =NELT(LE)
                SON=SH4TREE(2,NE)

                IF(SON==0) CYCLE

                LELT=LELT+1
                NELT(LELT)=SON

                LELT=LELT+1
                NELT(LELT)=SON+1

                LELT=LELT+1
                NELT(LELT)=SON+2

                LELT=LELT+1
                NELT(LELT)=SON+3

              END DO

              LEV   =LEV+1
              LELT1 =LELT2
              LELT2 =LELT

            END DO

            DO LE=1,LELT
              NE =NELT(LE)
              THKSH4_VAR(NE)=THK
              DO J=2,5
                THKNOD(IXC(J,NE))=MAX(THKNOD(IXC(J,NE)),THK)
              END DO
            END DO
          END DO
        ELSEIF(ITY == 7)THEN
          DO I=1,NEL
            N=NFT+I

            LEV=SH3TREE(3,N)
            IF(LEV < 0 .OR. GBUF%OFF(I)==ZERO) CYCLE

            IF(ITHK==0)THEN
              THK=THKE(N)
            ELSE
              THK=GBUF%THK(I)
            END IF

            LELT   =1
            NELT(1)=N
   
            LELT1  =0
            LELT2  =1

            DO WHILE (LEV < LEVELMAX)

              DO LE=LELT1+1,LELT2
  
                NE =NELT(LE)
                SON=SH3TREE(2,NE)

                IF(SON==0) CYCLE

                LELT=LELT+1
                NELT(LELT)=SON

                LELT=LELT+1
                NELT(LELT)=SON+1

                LELT=LELT+1
                NELT(LELT)=SON+2

                LELT=LELT+1
                NELT(LELT)=SON+3

              END DO

              LEV   =LEV+1
              LELT1 =LELT2
              LELT2 =LELT

            END DO
            DO LE=1,LELT
              NE =NELT(LE)
              THKSH3_VAR(NE)=THK
              DO J=2,4
                THKNOD(IXTG(J,NE))=MAX(THKNOD(IXTG(J,NE)),THK)
              END DO
            END DO
          END DO
        END IF

        IF (IDDW.GT.0) CALL STOPTIMEG(NG)
      GOTO 450
 452  CONTINUE
      END IF
C-----------------------------------------------
      RETURN
      END
