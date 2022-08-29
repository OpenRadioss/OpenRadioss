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
Chd|  I22COR3                       source/interfaces/int22/i22cor3.F
Chd|-- called by -----------
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I22COR3(
     1                JLT     ,X         ,IRECT  ,NSV     ,CAND_E ,
     2                CAND_B  ,STF       ,STFN   ,X1      ,X2     ,
     3                X3      ,X4        ,Y1     ,Y2      ,Y3     ,
     4                Y4      ,Z1        ,Z2     ,Z3      ,Z4     ,
     5                XI      ,YI        ,ZI     ,STIF    ,IX1    ,
     6                IX2     ,IX3       ,IX4    ,NSVG    ,IGAP   ,
     7                GAP     ,GAP_S     ,GAP_M  ,GAPV    ,MS     ,
     8                VXI    ,VYI  ,
     A                VZI     ,MSI       ,NSN    ,V       ,KINET  ,
     B                KINI    ,ITY       ,NIN    ,IGSTI   ,KMIN   ,
     C                KMAX    ,GAPMAX    ,GAPMIN ,IADM    ,INDEX  , 
     D                INTTH   ,TEMP      ,CAND__E,CAND__B ,
     E                TEMPI   ,PHI       ,AREAS  ,IELEC   ,AREASI ,
     F                IELECI  ,NODNX_SMS ,NSMS   ,GAP_S_L ,GAP_M_L,
     G                BUFBRIC ,JTASK)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE I22EDGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_B(*),KINET(*),KINI(*),
     .        JLT,IDT, NOINT,IGAP ,NSN, ITY, NIN, IGSTI,
     .        IADM,INTTH, CAND__E(*), CAND__B(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),IELEC(*),IELECI(MVSIZ),  NSMS(MVSIZ), 
     .        NODNX_SMS(*), BUFBRIC(*), JTASK,INDEX(*)
      my_real
     .   GAP, X(3,*), STF(*), STFN(*),GAP_S(*),GAP_M(*),
     .   MS(*), V(3,*), TEMP(*),AREAS(*),PHI(*),
     .   TEMPI(*),AREASI(*),GAP_S_L(*),GAP_M_L(*)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   KMIN, KMAX, GAPMAX, GAPMIN,SCUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, EI, NN, IG,JFT, IX,  BI, NI
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  
C -------------------------------------------------------------
C     DEFINITION OF GLOBAL ARRAY FOR SHELLS (LOCAL + REMOTE ONES)
C -------------------------------------------------------------
C      IRECT_L( 1: 4, :)                         : Four Node ITAB( ID) : global id to manage unicity with XREM
C      IRECT_L( 5: 8, :)                         : X-coordinates
C      IRECT_L( 9:12, :)                         : Y-coordinates
C      IRECT_L(13:16, :)                         : Z-coordinates
C      IRECT_L(17:19, :)                         : Minimum X,Y,Z
C      IRECT_L(20:22, :)                         : Maximum X,Y,Z
C      IRECT_L(23   , :)                         : Stiffness
C      IRECT_L(24:26, :)                         : Lagrangian Velocity (mean)
C      IRECT_L(    :,1+4*ESHIFT:4*ESHIFT+NRTM_T) : element data on current Thread
C      IRECT_L(    :,NRTM+1:NRTM + NSNR)         : remotes elements data
C-----------------------------------------------
C
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

          !candidates retaines from each nodes
          !INDEX is index array for current thread
          DO I=1,JLT
            CAND_E(I) = CAND__E(INDEX(I))
            CAND_B(I) = CAND__B(INDEX(I))      
          ENDDO
          

          DO I=1,JLT
           BI = CAND_B(I)    !up to i22intersect() : cand_b is related to couple I. It corresponds to brick IID = bufbric(cand_b(i))
	                     !after that cand_b(i) is replaced by address in brick_list
           EI = IABS(CAND_E(I))   
           NI=BI
           
           !-------------------------------!
           !RETRIEVING FACES COORDINATES   !
           !-------------------------------! 

           IX=IRECT(1,EI)
           IX1(I)=IX
           X1(I)=X(1,IX)
           Y1(I)=X(2,IX)
           Z1(I)=X(3,IX)

           IX=IRECT(2,EI)
           IX2(I)=IX
           X2(I)=X(1,IX)
           Y2(I)=X(2,IX)
           Z2(I)=X(3,IX)

           IX=IRECT(3,EI)
           IX3(I)=IX
           X3(I)=X(1,IX)
           Y3(I)=X(2,IX)
           Z3(I)=X(3,IX)

           IX=IRECT(4,EI)
           IX4(I)=IX
           X4(I)=X(1,IX)
           Y4(I)=X(2,IX)
           Z4(I)=X(3,IX)
           
           STIF(I) = ZERO

          END DO

      RETURN
      END
C
