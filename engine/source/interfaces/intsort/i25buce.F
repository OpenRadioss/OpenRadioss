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
!||    i25buce       ../engine/source/interfaces/intsort/i25buce.F
!||--- called by ------------------------------------------------------
!||    i25main_tri   ../engine/source/interfaces/intsort/i25main_tri.F
!||--- calls      -----------------------------------------------------
!||    i25trivox     ../engine/source/interfaces/intsort/i25trivox.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||    tri7box       ../engine/share/modules/tri7box.F
!||====================================================================
       SUBROUTINE I25BUCE(
     1   X        ,V       ,IRECT   ,NSV      ,STFN     , 
     2   NMN      ,NRTM    ,NSN     ,CAND_E   ,CAND_N   ,
     3   GAP      ,NOINT   ,II_STOK ,MULNSN   ,BMINMA   ,
     4   MARGE    ,CURV_MAX,PMAX_GAP,VMAXDT   ,
     5   ESHIFT   ,NIN     ,STF     ,GAP_S    ,
     6   NSNR     ,NCONT   ,GAP_M   ,ITASK    ,BGAPSMX  ,
     7   I_MEM    ,PENE_OLD,ITAB    ,NBINFLG  ,MBINFLG  ,
     8   ILEV     ,MSEGTYP ,
     9   FLAGREMNODE,KREMNOD,REMNOD ,
     A   IGAP     ,GAP_S_L ,GAP_M_L ,ICODT    ,ISKEW    ,
     B   DRAD     ,DGAPLOAD) 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,IDT,NIN, NSNR
      INTEGER IRECT(4,*),NSV(*),ITASK,ILEV, IGAP
      INTEGER CAND_E(*),CAND_N(*),MSEGTYP(*)
      INTEGER MULNSN,ESHIFT,NCONT,I_MEM,
     .        II_STOK,ITAB(*),NBINFLG(*),MBINFLG(*),
     .        FLAGREMNODE,KREMNOD(*),REMNOD(*),ICODT(*),ISKEW(*)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   GAP,PMAX_GAP,VMAXDT,
     .   BMINMA(6),CURV_MAX(NRTM),BGAPSMX, MARGE
      my_real
     .   X(3,*), V(3,*), STFN(*),PENE_OLD(5,NSN),
     .   STF(*), GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP0, IP1, MAXSIZ,
     .        LOC_PROC, N, ISZNSNR,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6), AAA
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8   
      INTEGER :: INIVOXEL_L

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C----- TRI DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C
C----- BORNES DU DOMAINE DEJA CALCULEES
C
      XYZM(1) = BMINMA(4)
      XYZM(2) = BMINMA(5)
      XYZM(3) = BMINMA(6)
      XYZM(4) = BMINMA(1)
      XYZM(5) = BMINMA(2)
      XYZM(6) = BMINMA(3)
      I_MEM = 0
C
      ISZNSNR = NSNR
C=============================================================================
c
c     Si MARGE - DELTA_PMAX_GAP - SOMME(Vrel*dt) < ZERO => RETRI
c        DELTA_PMAX_GAP = MAX((PENEmax(i)-GAP)-(PENEmax_Tri(i)-GAP)
c
C=============================================================================


      IF( NMN /= 0) THEN
       AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))
      ELSE
        AAA = 0
      ENDIF

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF
      
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      ENDIF

!$OMP SINGLE
      DO I=1,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
      ! wait not necessary, because there is a barrier at the start of I25TRIVOX
!$OMP END SINGLE NOWAIT

      CALL I25TRIVOX(
     1   NSN     ,NSNR     ,ISZNSNR  ,I_MEM   ,VMAXDT   ,
     2   IRECT   ,X        ,STF      ,STFN    ,XYZM     ,
     3   NSV     ,II_STOK  ,CAND_N   ,ESHIFT  ,CAND_E   ,
     4   MULNSN  ,NOINT    ,V        ,BGAPSMX  ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ     ,PMAX_GAP ,
     6   NRTM    ,GAP_S    ,GAP_M    ,MARGE   ,CURV_MAX ,
     7   NIN     ,ITASK    ,PENE_OLD,ITAB     ,NBINFLG  ,
     8   MBINFLG ,ILEV     ,MSEGTYP  ,
     9   FLAGREMNODE,KREMNOD,REMNOD  ,
     A   IGAP    ,GAP_S_L  ,GAP_M_L  ,ICODT   ,ISKEW    ,
     B   DRAD    ,DGAPLOAD )
C
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C
      RETURN
      END
