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
Chd|  I25BUCE_EDG                   source/interfaces/intsort/i25buce_edg.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I25BUCE_EDG(
     1   X        ,V       ,IRECT   ,INACTI    ,
     2   NSN      ,NMN      ,CANDM_E2E  ,CANDS_E2E    ,
     3   GAP      ,NOINT   ,II_STOK ,MULNSNE  ,BMINMA   ,
     4   MARGE    ,VMAXDT  ,DRAD    ,
     5   ESHIFT   ,NEDGE_T ,SSHIFT  ,NRTM_T   ,STF      ,
     6   STFE     ,NCONT   ,GAP_M   ,ITASK    ,BGAPEMX  ,
     7   I_MEM    ,ITAB    ,MBINFLG  ,EBINFLG ,LL_STOK  ,
     8   MULNSNS  ,ILEV    ,CAND_A   ,CAND_P  ,IGAP0    ,
     9   FLAGREMNODE,KREMNODE_EDG,REMNODE_EDG,KREMNODE_E2S ,
     .                                        REMNODE_E2S,
     A   IGAP    ,GAP_M_L   ,IEDGE    ,NEDGE    ,MSEGTYP,
     B   LEDGE   ,ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C   CANDM_E2S,CANDS_E2S  ,CAND_B ,CAND_PS ,GAPE ,
     D   GAP_E_L, NEDGE_LOCAL ,IFQ   ,CANDE2E_FX ,CANDE2E_FY,
     E   CANDE2E_FZ,CANDE2S_FX ,CANDE2S_FY,CANDE2S_FZ,
     F   IFPEN_E  ,IFPEN_E2S ,KREMNODE_EDG_SIZ,REMNODE_EDG_SIZ,
     G   KREMNODE_E2S_SIZ,REMNODE_E2S_SIZ,DGAPLOAD) 
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,NOINT,IDT,INACTI,IGAP0,IFQ
      INTEGER IRECT(4,*),ITASK,ILEV, IGAP, IEDGE, NEDGE, ESHIFT,NEDGE_T,SSHIFT,NRTM_T 
      INTEGER CANDM_E2E(*),CANDS_E2E(*),CAND_A(*),CANDM_E2S(*),CANDS_E2S(*),CAND_B(*),
     .        IFPEN_E(*), IFPEN_E2S(*)
      INTEGER MULNSNE,MULNSNS,NCONT,I_MEM(2),
     .        II_STOK, LL_STOK, ITAB(*),MBINFLG(*), LEDGE(NLEDGE,*), ADMSR(*), MSEGTYP(*)
      INTEGER EBINFLG(*)
      INTEGER :: NEDGE_LOCAL
      INTEGER , INTENT(IN) :: KREMNODE_EDG_SIZ,REMNODE_EDG_SIZ,KREMNODE_E2S_SIZ,REMNODE_E2S_SIZ 
      INTEGER , INTENT(IN) :: 
     .        FLAGREMNODE, KREMNODE_EDG(KREMNODE_EDG_SIZ), REMNODE_EDG(REMNODE_EDG_SIZ),
     .        KREMNODE_E2S(KREMNODE_E2S_SIZ), REMNODE_E2S(REMNODE_E2S_SIZ)
C     REAL
      my_real
     .   GAP,PMAX_GAP,VMAXDT,
     .   BMINMA(6),BGAPEMX, MARGE
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   X(3,*), V(3,*), STF(*), STFE(*), GAP_M(*), GAP_M_L(*), GAPE(*), GAP_E_L(*), 
     .   CAND_P(*), CAND_PS(*), CANDE2E_FX(*), CANDE2E_FY(*), CANDE2E_FZ(*),
     .   CANDE2S_FX(4,*) ,CANDE2S_FY(4,*),CANDE2S_FZ(4,*)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IP0, IP1, MAXSIZ,  NSNF, NSNL,
     .        LOC_PROC, N, IEDG, I1, I2
C     REAL
      my_real
     .        XYZM(6), AAA, LX, LY, LZ, MY_LX, MY_LY, MY_LZ,DRAD2
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8   
      SAVE LX, LY, LZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DRAD2 = ZERO
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
      I_MEM(1:2) = 0
C
C=============================================================================
c
c     Si MARGE - DELTA_PMAX_GAP - SOMME(Vrel*dt) < ZERO => RETRI
c        DELTA_PMAX_GAP = MAX((PENEmax(i)-GAP)-(PENEmax_Tri(i)-GAP)
c
C=============================================================================


c      AAA = SQRT(NMN /
c     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
c     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
c     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

c     AAA = 0.75*AAA

      AAA = (NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))))**THIRD

      AAA = TWO*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))

c      LX=ZERO
c      LY=ZERO
c      LZ=ZERO
c
c      CALL MY_BARRIER      
c
c      MY_LX=ZERO
c      MY_LY=ZERO
c      MY_LZ=ZERO
c      DO I=1,NEDGE_T ! no parith on
c        IEDG=SSHIFT+I
c        I1=LEDGE(5,IEDG)
c        I2=LEDGE(6,IEDG)
c        MY_LX=MY_LX+ABS(X(1,I2)-X(1,I1))
c        MY_LY=MY_LY+ABS(X(2,I2)-X(2,I1))
c        MY_LZ=MY_LZ+ABS(X(3,I2)-X(3,I1))
c      END DO
c#include      "lockon.inc"
c      LX=LX+MY_LX
c      LY=LY+MY_LY
c      LZ=LZ+MY_LZ
c#include      "lockoff.inc"
c
c      CALL MY_BARRIER      
c
c!$OMP SINGLE
c      LX=TWO*LX/NEDGE
c      LY=TWO*LY/NEDGE
c      LZ=TWO*LZ/NEDGE
c!$OMP END SINGLE
c
c      NBX = NINT((BMINMA(1)-BMINMA(4))/LX)
c      NBY = NINT((BMINMA(2)-BMINMA(5))/LY)
c      NBZ = NINT((BMINMA(3)-BMINMA(6))/LZ)

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
      
      IF(RES8 > LVOXEL8) stop 678
c      IF(MAX(NBX,NBY,NBZ) > 100) stop 678

C     pas d'initialisation de VOXEL <=> remise a 0 dans i25trivox.
C
      CALL I25TRIVOX_EDG(
     1   I_MEM   ,VMAXDT   ,INACTI  ,IRECT   ,
     2   X        ,V       ,STF     ,STFE    ,XYZM     ,
     3   II_STOK ,CANDS_E2E   ,ESHIFT  ,NEDGE_T ,CANDM_E2E   ,
     4   MULNSNE ,NOINT    ,BGAPEMX ,SSHIFT  ,NRTM_T   ,
     5   VOXEL1  ,NBX      ,NBY     ,NBZ      ,
     6   IGAP    ,GAP_M    ,GAP_M_L ,DRAD2    ,MARGE    ,
     7   ITASK    ,ITAB    ,LL_STOK ,MULNSNS  ,
     8   MBINFLG , EBINFLG ,ILEV     ,CAND_A  ,CAND_P   ,
     9   FLAGREMNODE,KREMNODE_EDG,REMNODE_EDG,KREMNODE_E2S ,
     .                                     REMNODE_E2S   ,
     A   IEDGE   ,NEDGE    ,LEDGE    ,MSEGTYP ,IGAP0    ,
     B   ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C   CANDM_E2S,CANDS_E2S,CAND_B,CAND_PS,GAPE ,
     D   GAP_E_L,NEDGE_LOCAL,IFQ,CANDE2E_FX ,CANDE2E_FY,
     E   CANDE2E_FZ,CANDE2S_FX ,CANDE2S_FY,CANDE2S_FZ,IFPEN_E,IFPEN_E2S,
     F   KREMNODE_EDG_SIZ,REMNODE_EDG_SIZ,KREMNODE_E2S_SIZ,REMNODE_E2S_SIZ, 
     G   DGAPLOAD )
C
C     I_MEM /= 0 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C
      RETURN
      END
