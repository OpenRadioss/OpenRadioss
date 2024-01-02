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
Chd|  I25BUCE_EDG                   source/interfaces/inter3d1/i25buce_edg.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I25TRIVOX_EDG                 source/interfaces/inter3d1/i25trivox_edg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
       SUBROUTINE I25BUCE_EDG(
     1   X        ,IRECT   ,INACTI  ,NSN      ,
     2   NMN      ,CANDM_E2E  ,CANDS_E2E  ,
     3   GAP      ,NOINT   ,II_STOK ,MULNSNE  ,BMINMA   ,
     4   MARGE    ,VMAXDT  ,DRAD    ,ESHIFT   ,NEDGE_T  ,
     5   SSHIFT   ,NRTM_T  ,STFM    ,STFN     ,
     6   NCONT    ,GAP_M   ,ITASK    ,BGAPEMX  ,
     7   I_MEM    ,ITAB    ,MBINFLG  ,EBINFLG  ,LL_STOK  ,
     8   MULNSNS  ,ILEV    ,CAND_A   ,CAND_P   ,IGAP0    ,
     9   FLAGREMNODE,KREMNOD,REMNOD   ,
     A   IGAP    ,GAP_M_L   ,IEDGE    ,NEDGE    ,MSEGTYP, 
     B   LEDGE   ,ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C   CANDM_E2S ,CANDS_E2S,CAND_B ,CAND_PS  ,GAPE    ,
     D   GAP_E_L  ,DGAPLOAD,FLAG_REMOVED_NODE,
     E   S_KREMNODE_E2S,S_REMNODE_E2S,KREMNODE_E2S,REMNODE_E2S)
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
      INTEGER NSN, NMN,NOINT,IDT,INACTI,IGAP0
      INTEGER IRECT(4,*),ITASK,ILEV, IGAP, IEDGE, NEDGE, ESHIFT,NEDGE_T,SSHIFT,NRTM_T 
      INTEGER CANDM_E2E(*),CANDS_E2E(*),CAND_A(*),CANDM_E2S(*),CANDS_E2S(*),CAND_B(*)
      INTEGER MULNSNE,MULNSNS,NCONT,I_MEM(2),
     .        II_STOK, LL_STOK, ITAB(*),MBINFLG(*), EBINFLG(*), LEDGE(NLEDGE,*), ADMSR(*),
     .        MSEGTYP(*),
     .        FLAGREMNODE, KREMNOD(*), REMNOD(*)
      LOGICAL, INTENT(in) :: FLAG_REMOVED_NODE !< flag to remove some S node from the list of candidates
      INTEGER, INTENT(in) :: S_KREMNODE_E2S !< size of KREMNODE_E2S array
      INTEGER, INTENT(in) :: S_REMNODE_E2S !< size of REMNODE_E2S array
      INTEGER, DIMENSION(S_KREMNODE_E2S), INTENT(in) :: KREMNODE_E2S !< adress of forbidden S edge
      INTEGER, DIMENSION(S_REMNODE_E2S), INTENT(in) :: REMNODE_E2S !< list of forbidden S edge
      my_real
     .   GAP,VMAXDT,DRAD,
     .   BMINMA(6),BGAPEMX, MARGE
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .   X(3,*), STFM(*), GAP_M(*), GAP_M_L(*), GAPE(*), GAP_E_L(*), CAND_P(*), CAND_PS(*), STFN(*)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, SOL_EDGE, SH_EDGE, 
     .        N, L, E, IE, I1, I2, N1, N2 
      my_real
     .        XYZM(6), AAA, TZINF, 
     .        XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8   
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     CALCUL DES BORNES DU DOMAINE
C--------------------------------
      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)= EP30
      BMINMA(5)= EP30
      BMINMA(6)= EP30

      XMIN= EP30
      XMAX=-EP30
      YMIN= EP30
      YMAX=-EP30
      ZMIN= EP30
      ZMAX=-EP30 
C
      DO I=ESHIFT+1,ESHIFT+NEDGE_T
          L=LEDGE(1,I)
        E=LEDGE(2,I)
        IF(STFM(L) /= ZERO)THEN      
          J=IRECT(E,L)
          XMIN= MIN(XMIN,X(1,J))
          YMIN= MIN(YMIN,X(2,J))
          ZMIN= MIN(ZMIN,X(3,J))
          XMAX= MAX(XMAX,X(1,J))
          YMAX= MAX(YMAX,X(2,J))
          ZMAX= MAX(ZMAX,X(3,J))
          J=IRECT(MOD(E,4)+1,L)
          XMIN= MIN(XMIN,X(1,J))
          YMIN= MIN(YMIN,X(2,J))
          ZMIN= MIN(ZMIN,X(3,J))
          XMAX= MAX(XMAX,X(1,J))
          YMAX= MAX(YMAX,X(2,J))
          ZMAX= MAX(ZMAX,X(3,J))
        END IF
      END DO

      TZINF = MARGE+TWO*BGAPEMX+VMAXDT+DGAPLOAD

      XMIN=XMIN-TZINF
      YMIN=YMIN-TZINF
      ZMIN=ZMIN-TZINF
      XMAX=XMAX+TZINF
      YMAX=YMAX+TZINF
      ZMAX=ZMAX+TZINF

      BMINMA(1) = MAX(BMINMA(1),XMAX)
      BMINMA(2) = MAX(BMINMA(2),YMAX)
      BMINMA(3) = MAX(BMINMA(3),ZMAX)
      BMINMA(4) = MIN(BMINMA(4),XMIN)
      BMINMA(5) = MIN(BMINMA(5),YMIN)
      BMINMA(6) = MIN(BMINMA(6),ZMIN)

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


      AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

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
      END IF

C     initialisation complete de VOXEL
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
C
      CALL I25TRIVOX_EDG(
     1   I_MEM   ,VMAXDT   ,INACTI  ,
     2   IRECT   ,X        ,STFM    ,XYZM     ,
     3   II_STOK ,CANDS_E2E   ,ESHIFT  ,NEDGE_T  ,CANDM_E2E   ,
     4   MULNSNE ,NOINT    ,BGAPEMX ,SSHIFT   ,NRTM_T   ,
     5   VOXEL1  ,NBX      ,NBY     ,NBZ      ,
     6   IGAP    ,GAP_M    ,GAP_M_L ,DRAD     ,MARGE    ,
     7   ITASK   ,ITAB     ,LL_STOK ,MULNSNS  ,
     8   MBINFLG ,EBINFLG  ,ILEV    ,CAND_A   ,CAND_P   ,
     9   FLAGREMNODE,KREMNOD,REMNOD  ,
     A   IEDGE   ,NEDGE    ,LEDGE    ,MSEGTYP ,IGAP0    ,
     B   ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C   CANDM_E2S,CANDS_E2S,CAND_B,CAND_PS   ,GAPE     ,
     D   GAP_E_L  ,DGAPLOAD,FLAG_REMOVED_NODE,
     E   S_KREMNODE_E2S,S_REMNODE_E2S,KREMNODE_E2S,REMNODE_E2S)
C
C     I_MEM /= 0 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C
      RETURN
      END
