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
C
Chd|====================================================================
Chd|  I22STI3                       source/interfaces/inter3d1/i22sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I22STI3(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3        NTY   ,GAP   ,NOINT ,
     4 STFN  ,NSN   ,MS    ,NSV   ,IXTG  ,
     5 IGAP  ,WA    ,GAP_S ,GAP_M ,GAPMIN,
     6 IXT   ,IXP   ,GAPINF,GAPMAX,
     8 INACTI ,KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D PERCENT_SIZE,GAP_S_L ,GAP_M_L,NOD2EL1D,KNOD2EL1D,
     E IXR    ,ITAB,BGAPSMX ,IXS10  ,IXS16   ,IXS20    ,
     F ID,TITR,IGEO , PM_STACK , IWORKSH)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP, NDDIM,
     .        INACTI
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*), IELES(*), INTTH, IELEC(*),
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*),IPARTC(*),IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*),
     .   ITAB(*), IXS10(6,*), IXS16(*), IXS20(*),IGEO(NPROPGI,*),IWORKSH(3,*)
C     REAL
      my_real
     .   STFAC, GAP,GAPMIN,GAPINF, GAPMAX,PERCENT_SIZE, BGAPSMX 
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),WA(*),GAP_S(*),GAP_M(*),
     .   AREAS(*),THK(*),THK_PART(*),
     .   GAP_S_L(*),GAP_M_L(*),PM_STACK(20,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IGTYP,IPGMAT,IGMAT,
     .   ISUBSTACK
      INTEGER JPERM(4)
C     REAL
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX,GAPS1,GAPS2, GAPM, DDX, 
     .   GAPTMP, GAPSCALE,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   SLSFAC,XL,GAP_S_L_TMP(NUMNOD),ST
      DATA JPERM/2,3,4,1/
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS 
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
      SLSFAC = STFAC
      DXM=0.
      NDX=0
      GAPMX=EP30
      GAPMN=EP30
      GAPS1=ZERO
      GAPS2=ZERO
      GAPSCALE = ONE
      IPGMAT = 700
C------------------------------------
C     GAP STIF FACES MAIN
C------------------------------------
      
      STF(1:NRT)=ONE
      
      RETURN
      END
