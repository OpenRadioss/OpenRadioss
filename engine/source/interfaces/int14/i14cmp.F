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
Chd|  I14CMP                        source/interfaces/int14/i14cmp.F
Chd|-- called by -----------
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|-- calls ---------------
Chd|        I14CAN                        source/interfaces/int14/i14can.F
Chd|        I14DMP                        source/interfaces/int14/i14dmp.F
Chd|        I14ELA                        source/interfaces/int14/i14ela.F
Chd|        I14FRT                        source/interfaces/int14/i14frt.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I14CMP(IPARI ,INTBUF_TAB ,IGRSURF ,BUFSF ,
     2            A ,X ,V ,WA ,FSAV ,
     3            IN ,STIFN ,FCONT , MS ,FSKYI , 
     4            ISKY ,NPC ,PLD   ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI),ISKY(*), NPC(*)
C     REAL
      my_real
     .   BUFSF(*) ,
     .   A(3,*) ,X(3,*) ,V(3,*) ,FSAV(NTHVKI) ,
     .   WA(*)  ,IN(*) ,STIFN(*),FCONT(3,*),MS(*),
     .   FSKYI(LSKY,NFSKYI),
     .   PLD(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K,IGI,IGS,IM,NSN,NSC,NSP
      INTEGER NLO,NFRIC,NDAMP1,NDAMP2
C-----------------------------------------------
C     REAL
      my_real
     .   STFAC, GAPMIN, FRIC, VISC, STF
C-----------------------------------------------
      NSN =IPARI(5)
      GAPMIN=INTBUF_TAB%VARIABLES(2)
      CALL I14CAN(NSN,INTBUF_TAB%NSV,X,INTBUF_TAB%KSURF(1),IGRSURF,
     2      BUFSF,GAPMIN,NSC,WA(1),NSP,
     3      WA(NUMNOD+1),INTBUF_TAB%IMPACT,INTBUF_TAB%CIMP,INTBUF_TAB%NIMP,
     4      WA(2*NUMNOD+1))
C-----------------------------------------------
      STFAC  =INTBUF_TAB%STFAC(1)
      NLO    =IPARI(8)
      FRIC   =INTBUF_TAB%VARIABLES(1)
      NFRIC  =IPARI(9)
      VISC   =INTBUF_TAB%VARIABLES(14)
      NDAMP1 =IPARI(10)
      NDAMP2 =IPARI(11)
C-----------------------------------------------
      CALL I14ELA(X	,INTBUF_TAB%KSURF ,IGRSURF  ,BUFSF ,NSC    ,
     2      WA(1) ,NSP ,WA(NUMNOD+1) ,INTBUF_TAB%NSV,INTBUF_TAB%IMPACT,
     3      INTBUF_TAB%CIMP ,INTBUF_TAB%NIMP   ,STFAC ,NLO ,GAPMIN   ,
     4      NPC ,PLD   ,WA(2*NUMNOD+1) ,STF  )
C-----------------------------------------------
      CALL I14DMP(X    ,V     ,INTBUF_TAB%KSURF   ,IGRSURF  ,BUFSF ,
     2      NSC ,WA(1) ,NSP   ,WA(NUMNOD+1) ,INTBUF_TAB%NSV    ,
     3      INTBUF_TAB%IMPACT,INTBUF_TAB%CIMP,INTBUF_TAB%NIMP,VISC,NDAMP1,
     4      NDAMP2     ,GAPMIN     ,NPC        ,PLD   ,MS     ,
     5      WA(2*NUMNOD+1) ,WA(3*NUMNOD+1) ,STF )
C-----------------------------------------------
C     Frottement calcule apres amortissement.
      CALL I14FRT(A	,X     ,V     ,INTBUF_TAB%KSURF ,IGRSURF  ,
     2      BUFSF ,NSC  ,WA(1) ,NSP   ,WA(NUMNOD+1) ,
     3      INTBUF_TAB%NSV,INTBUF_TAB%IMPACT,INTBUF_TAB%CIMP,INTBUF_TAB%NIMP,
     4      FRIC   ,NFRIC ,NPC    ,PLD  ,GAPMIN ,STF	,
     5      WA(2*NUMNOD+1),WA(3*NUMNOD+1),INTBUF_TAB%VARIABLES(9),MS,STIFN ,
     6      FSAV   ,FCONT  ,FSKYI ,ISKY  ,H3D_DATA)
C-----------------------------------------------
9999  CONTINUE
      RETURN
      END
