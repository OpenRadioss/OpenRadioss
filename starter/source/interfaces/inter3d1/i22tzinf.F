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
Chd|  I22TZINF                      source/interfaces/inter3d1/i22tzinf.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I22TZINF(
     1   X      ,IRECT  ,NSV     ,BUMULT  ,NSEG     ,
     2   NMN    ,NRTM   ,MWA     ,NSN     ,CAND_E   ,
     3   CAND_N ,GAP    ,XYZM    ,NOINT   ,I_STOK   ,
     4   DIST   ,TZINF  ,MAXBOX  ,MINBOX  ,MSR      ,   
     5   STF    ,STFN   ,MULTIMP ,ISTF    ,IDDLEVEL ,
     6   ITAB   ,GAP_S  ,GAP_M   ,IGAP    ,GAPMIN   ,
     7   GAPMAX ,INACTI ,GAP_S_L ,GAP_M_L ,I_MEM    ,
     8   ID     ,TITR   ,BUFBRIC ,NBRIC   ,IXS)
      USE MESSAGE_MOD
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,I_STOK,MULTIMP,ISTF,IGAP,
     .        INACTI,I_MEM,IXS(NIXS,*)
      INTEGER IRECT(4,*),NSV(*),NSEG(*),MWA(*)
      INTEGER CAND_E(*),CAND_N(*),MSR(*),MAXSIZ,IDDLEVEL
      INTEGER ITAB(*)
      my_real
     .   STF(*),STFN(*),X(3,*),XYZM(6,*),GAP_S(*),GAP_M(*),
     .   DIST,BUMULT,GAP,TZINF,MAXBOX,MINBOX,GAPMIN,GAPMAX,
     .   GAP_S_L(*),GAP_M_L(*), XX(8),YY(8), ZZ(8), CoorMIN(3), CoorMAX(3), DDmax
     
      INTEGER ID, BUFBRIC(*), NBRIC
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK, IBID
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,NE
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,MINBOX0,MAXBOX0,GAPSMAX,
     .   BID,TZINF_ST,MARGE,MARGE_ST,GAPV(MVSIZ),
     .   DIAG(4)
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
      IF(NBRIC==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !1-CALCUL TAILLE DES ZONES INFLUENCES
      !DD EST LA DIMENSION DE LA BOULE CIRCONSCRITE (NORME L1)

      
      L = 1
      NE = BUFBRIC(L)
      XX(1:8)    = X(1, IXS(2:9,NE) )
      YY(1:8)    = X(2, IXS(2:9,NE) )
      ZZ(1:8)    = X(3, IXS(2:9,NE) ) 

      DIAG(1)    = SQRT((XX(1)-XX(7))**2 + (YY(1)-YY(7))**2 + (ZZ(1)-ZZ(7))**2) 
      DIAG(2)    = SQRT((XX(3)-XX(5))**2 + (YY(3)-YY(5))**2 + (ZZ(3)-ZZ(5))**2)
      DIAG(3)    = SQRT((XX(2)-XX(8))**2 + (YY(2)-YY(8))**2 + (ZZ(2)-ZZ(8))**2)
      DIAG(4)    = SQRT((XX(4)-XX(6))**2 + (YY(4)-YY(6))**2 + (ZZ(4)-ZZ(6))**2)
      
      DDmax      = MAXVAL(DIAG(1:4))

      DO L=2,NBRIC
        NE = BUFBRIC(L)
        XX(1:8)    = X(1, IXS(2:9,NE) )
        YY(1:8)    = X(2, IXS(2:9,NE) )
        ZZ(1:8)    = X(3, IXS(2:9,NE) ) 
        DIAG(1)    = SQRT((XX(1)-XX(7))**2 + (YY(1)-YY(7))**2 + (ZZ(1)-ZZ(7))**2) 
        DIAG(2)    = SQRT((XX(3)-XX(5))**2 + (YY(3)-YY(5))**2 + (ZZ(3)-ZZ(5))**2)
        DIAG(3)    = SQRT((XX(2)-XX(8))**2 + (YY(2)-YY(8))**2 + (ZZ(2)-ZZ(8))**2)
        DIAG(4)    = SQRT((XX(4)-XX(6))**2 + (YY(4)-YY(6))**2 + (ZZ(4)-ZZ(6))**2)      
        DD         = MAXVAL(DIAG(1:4))      
        IF(DD>DDmax)THEN
          DDmax = DD
          !print *, "TZINF : briqueID=", IXS(11,NE)
          !print *, "          New DD=", DD
        ENDIF
      ENDDO
      
      TZINF = (ONE+FIVE/HUNDRED) * DDmax
           
      RETURN
      END
