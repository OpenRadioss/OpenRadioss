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
Chd|  SPMD_TRI25VOX0                source/mpi/interfaces/spmd_tri25vox0.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25VOX0(
     1   X      ,BMINMAL ,NRTM  ,STF   ,MARGE  ,
     2   CURV_MAX,GAP_M  ,IRECT  ,GAP  ,BGAPSMX,
     3   PMAX_GAP,VMAXDT,BGAPEMX, IEDGE,
     4   LEDGE, NEDGE, NLEDGE,
     5   GAPE ,DRAD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NRTM
      INTEGER, INTENT(IN) :: NLEDGE
      INTEGER, INTENT(IN) :: NEDGE
      INTEGER, INTENT(IN) :: IEDGE
      INTEGER, INTENT(IN) :: LEDGE(NLEDGE,NEDGE)
      INTEGER  IRECT(4,NRTM)
      my_real
     .        X(3,*), BMINMAL(*),
     .        STF(*), GAP_M(*), BGAPSMX,PMAX_GAP,VMAXDT,
     .        MARGE,GAP,CURV_MAX(NRTM),
     .        BGAPEMX,DRAD
      my_real GAPE(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
      my_real
     .        RATIO, AAA,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4

      INTEGER :: SOL_EDGE,SH_EDGE
      INTEGER :: IE,N1,N2
      my_real :: DX,DY,DZ
      INTEGER :: IDS(4)
      INTEGER :: TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C================================================================

      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells

      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL25
      NBY = LRVOXEL25
      NBZ = LRVOXEL25

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      DO NE=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NE) == ZERO)CYCLE
         AAA = MARGE+CURV_MAX(NE)+VMAXDT
     +       + MAX(MAX(PMAX_GAP,BGAPSMX+GAP_M(NE)),DRAD)


C verifier avec 
         IF(SOL_EDGE > 0) AAA = MAX(AAA,MARGE+BGAPEMX)

c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

         M1 = IRECT(1,NE)
         M2 = IRECT(2,NE)
         M3 = IRECT(3,NE)
         M4 = IRECT(4,NE)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XX3=X(1,M3)
         XX4=X(1,M4)
         XMAXE=MAX(XX1,XX2,XX3,XX4)
         XMINE=MIN(XX1,XX2,XX3,XX4)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YY3=X(2,M3)
         YY4=X(2,M4)
         YMAXE=MAX(YY1,YY2,YY3,YY4)
         YMINE=MIN(YY1,YY2,YY3,YY4)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZZ3=X(3,M3)
         ZZ4=X(3,M4)
         ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
         ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

         IF(SOL_EDGE > 0 ) THEN
           DX=EM02*(XMAXE-XMINE)
           DY=EM02*(YMAXE-YMINE)
           DZ=EM02*(ZMAXE-ZMINE)
           XMAXE=XMAXE+DX
           XMINE=XMINE-DX
           YMAXE=YMAXE+DY
           YMINE=YMINE-DY
           ZMAXE=ZMAXE+DZ
           ZMINE=ZMINE-DZ
         ENDIF

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(0,MIN(NBX,IX1))
         IY1=MAX(0,MIN(NBY,IY1))
         IZ1=MAX(0,MIN(NBZ,IZ1))

         IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=MAX(0,MIN(NBX,IX2))
         IY2=MAX(0,MIN(NBY,IY2))
         IZ2=MAX(0,MIN(NBZ,IZ2))

         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             TMP = 0
             DO IX = IX1, IX2
               TMP=IBSET(TMP,IX)
             END DO
#include "atomic.inc"
             CRVOXEL25(IY,IZ,1,LOC_PROC)=IOR(CRVOXEL25(IY,IZ,1,LOC_PROC),TMP)
           END DO
         END DO
      ENDDO

C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_TRI25VOX0_EDGE           source/mpi/interfaces/spmd_tri25vox0.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25VOX0_EDGE(
     1   X      ,BMINMAL ,NRTM  ,STFE   ,MARGE  ,
     2   CURV_MAX,GAP_M  ,IRECT  ,GAP  ,BGAPSMX,
     3   PMAX_GAP,VMAXDT,BGAPEMX, IEDGE,IGAP0  ,
     4   LEDGE, NEDGE, NLEDGE,
     5   GAPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NRTM
      INTEGER, INTENT(IN) :: NLEDGE
      INTEGER, INTENT(IN) :: NEDGE
      INTEGER, INTENT(IN) :: IEDGE, IGAP0
      INTEGER, INTENT(IN) :: LEDGE(NLEDGE,NEDGE)
      INTEGER  IRECT(4,NRTM)
      my_real
     .        X(3,*), BMINMAL(*),
     .        STFE(NEDGE), GAP_M(*), BGAPSMX,PMAX_GAP,VMAXDT,
     .        MARGE,GAP,CURV_MAX(NRTM),
     .        BGAPEMX
      my_real GAPE(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
      my_real
     .        RATIO, AAA,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4

      INTEGER :: SOL_EDGE,SH_EDGE
      INTEGER :: IE,N1,N2
      INTEGER :: TMP

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C================================================================

      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells

      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL25
      NBY = LRVOXEL25
      NBZ = LRVOXEL25

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

!$OMP DO
      DO IE = 1, NEDGE
C check with :
        IF(STFE(IE) == ZERO ) CYCLE
        IF(LEDGE(9,IE) == 0) CYCLE ! not main of secnd edge 

        M1 = LEDGE(1,IE)
c       IF(M1 > 0) THEN
c         IF(STF(M1) == ZERO) CYCLE
c       ENDIF

Clm     BGAPEMX already counted in BMINMAL
        AAA=ZERO
C       IF(IGAP0==0)THEN
C         AAA = MARGE+BGAPEMX+GAPE(IE)
C       ELSE
C         AAA = MARGE+DEUX*BGAPEMX+GAPE(IE)
C       END IF

        N1 = LEDGE(5,IE)
        N2 = LEDGE(6,IE)
        
        XX1=X(1,N1)
        XX2=X(1,N2)
        YY1=X(2,N1)
        YY2=X(2,N2)
        ZZ1=X(3,N1)
        ZZ2=X(3,N2)
        XMAXE=MAX(XX1,XX2)+GAPE(IE) ! +TZINF
        XMINE=MIN(XX1,XX2)-GAPE(IE) ! -TZINF
        YMAXE=MAX(YY1,YY2)+GAPE(IE) ! +TZINF
        YMINE=MIN(YY1,YY2)-GAPE(IE) ! -TZINF
        ZMAXE=MAX(ZZ1,ZZ2)+GAPE(IE) ! +TZINF
        ZMINE=MIN(ZZ1,ZZ2)-GAPE(IE) ! -TZINF
       !-------------------------------------------!
       !  VOXEL OCCUPIED BY THE EDGE               !
       !-------------------------------------------!
       !Voxel_lower_left_bound for this element---+

        IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

        IX1=MAX(0,MIN(NBX,IX1))
        IY1=MAX(0,MIN(NBY,IY1))
        IZ1=MAX(0,MIN(NBZ,IZ1))

        IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

        IX2=MAX(0,MIN(NBX,IX2))
        IY2=MAX(0,MIN(NBY,IY2))
        IZ2=MAX(0,MIN(NBZ,IZ2))

         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             TMP = 0
             DO IX = IX1, IX2
               TMP=IBSET(TMP,IX)
             END DO
#include "atomic.inc"
             CRVOXEL25(IY,IZ,1,LOC_PROC)=IOR(CRVOXEL25(IY,IZ,1,LOC_PROC),TMP)
           END DO
         END DO
      END DO
!$OMP END DO
      RETURN
      END

