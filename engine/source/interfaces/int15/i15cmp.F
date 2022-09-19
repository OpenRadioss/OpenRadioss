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
Chd|  I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- called by -----------
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I15ASS                        source/interfaces/int15/i15ass.F
Chd|        I15CAN                        source/interfaces/int15/i15can.F
Chd|        I15FOR1                       source/interfaces/int15/i15for1.F
Chd|        I15FORT1                      source/interfaces/int15/i15fort1.F
Chd|        I15MARQ                       source/interfaces/int15/i15marq.F
Chd|        I15TOT1                       source/interfaces/int15/i15tot1.F
Chd|        I15TOTT1                      source/interfaces/int15/i15tott1.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I15CMP(IPARI,INTBUF_TAB,IGRSURF,BUFSF,
     2            A, X, V, WA, FSAV,
     3            IN, STIFN, FCONT, MS, FSKYI,
     4            ISKY, NPC  , PLD , DT2T, NELTST,
     5            ITYPTST, ITAB,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI),
     .        ISKY(*), NPC(*),
     .        NELTST, ITYPTST,
     .        ITAB(*)
C     REAL
      my_real
     .   BUFSF(*) ,A(3,*) ,X(3,*) ,V(3,*) ,
     .   FSAV(NTHVKI) ,WA(*)  ,IN(*) ,STIFN(*) ,FCONT(3,*) ,
     .   MS(*) ,FSKYI(LSKY,NFSKYI) ,PLD(*),
     .   DT2T

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NOINT,N,K,NRTS,NSN,INOD
      INTEGER I,NDEB,NREST, NSC, NTC, NNC, MAXFCT
      INTEGER KSC, KTC, KNC, KMARQND, KWNF, KWTF, KWNS, KWLENG
C-----------------------------------------------
C     REAL
      my_real
     .  STFAC, FRIC, STF,
     .  XP1(3,MVSIZ) ,XP2(3,MVSIZ) ,XP3(3,MVSIZ) , XP4(3,MVSIZ),
     .  GX(3,MVSIZ)  ,XTK(4,MVSIZ) ,YTK(4,MVSIZ) ,ZTK(4,MVSIZ) ,
     .  NTX(4,MVSIZ) ,NTY(4,MVSIZ) ,NTZ(4,MVSIZ) ,
     .  ANS(4,MVSIZ) ,DEPTH(4,MVSIZ),
     .  XI(4,MVSIZ)  ,YI(4,MVSIZ)  ,ZI(4,MVSIZ)  ,
     .  NXI(4,MVSIZ) ,NYI(4,MVSIZ) ,NZI(4,MVSIZ) ,
     .  DE, ANSMX, FTOT,
     .  FNORMX,FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ
C-----------------------------------------------
        NOINT=IPARI(15)
C-----------------------------------------------
C       Vecteurs de travail (nbre maximum de facettes).
        MAXFCT=6*NUMELS+NUMELC+NUMELTG
        KSC    =1
        KTC    =MAXFCT +KSC
        KNC    =MAXFCT +KTC
        KMARQND=KNC    +NUMNOD
        KWNF   =KMARQND+NUMNOD
        KWTF   =KWNF   +3*NUMNOD
        KWNS   =KWTF   +3*NUMNOD
C       Longueur totale des vecteurs de travail.
        KWLENG=KWNS+NUMNOD-1
        IF (KWLENG>LENWA) THEN
          CALL ANCMSG(MSGID=83,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C-----------------------------------------------
      NRTS =IPARI(3)
      NSN  =IPARI(5)
C-----------------------------------------------
C     ELIMINATION (HEURISTIQUE).
C-----------------------------------------------
      CALL I15CAN(NRTS,INTBUF_TAB%IRECTS,X,INTBUF_TAB%KSURF(1),IGRSURF,
     2  	  BUFSF,NSC,WA(KSC),NTC,WA(KTC),
     3  	  INTBUF_TAB%IMPACT)
C-----------------------------------------------
      STFAC  =INTBUF_TAB%STFAC(1)
      FRIC   =INTBUF_TAB%VARIABLES(1)
C-----------------------------------------------
      ANSMX =ZERO
      DE    =ZERO
C-----
      FTANGX=ZERO
      FTANGY=ZERO
      FTANGZ=ZERO
      FNORMX=ZERO
      FNORMY=ZERO
      FNORMZ=ZERO
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,NSN
        INOD=INTBUF_TAB%NSV(I)
        WA(KWNF+3*(INOD-1))  =ZERO
        WA(KWNF+3*(INOD-1)+1)=ZERO
        WA(KWNF+3*(INOD-1)+2)=ZERO
        WA(KWTF+3*(INOD-1))  =ZERO
        WA(KWTF+3*(INOD-1)+1)=ZERO
        WA(KWTF+3*(INOD-1)+2)=ZERO
        WA(KWNS+INOD-1)=ZERO	
      ENDDO
C-----------------------------------------------
C      QUADRANGLES.
C-----------------------------------------------
       NDEB =0
 10    CONTINUE
       CALL I15TOT1(NOINT ,NDEB, NSC,X  	,INTBUF_TAB%KSURF(1) ,
     2         IGRSURF  ,BUFSF   ,WA(KSC) ,INTBUF_TAB%IRECTS ,INTBUF_TAB%NOLD,
     3         XP1   ,XP2   ,XP3   ,XP4   ,GX	  ,
     4         XTK   ,YTK   ,ZTK   ,NTX   ,NTY    ,
     5         NTZ   ,ANS   ,DEPTH ,XI    ,YI	  ,
     6         ZI    ,NXI   ,NYI   ,NZI   ,ANSMX  ,
     7         INTBUF_TAB%HOLD ,INTBUF_TAB%IMPACT ,ITAB )
       CALL I15FOR1(NDEB, NSC, STFAC,X     ,V	   ,
     2       INTBUF_TAB%KSURF ,IGRSURF ,BUFSF ,WA(KSC) ,INTBUF_TAB%IRECTS,
     3       INTBUF_TAB%IMPACT,INTBUF_TAB%IOLD ,INTBUF_TAB%HOLD ,INTBUF_TAB%NOLD ,
     4  	 INTBUF_TAB%DOLD ,XP1   ,XP2    ,XP3	,XP4   ,GX     ,
     5  	 XTK   ,YTK   ,ZTK   ,NTX   ,NTY    ,
     6  	 NTZ   ,ANS   ,DEPTH ,XI    ,YI    ,
     7  	 ZI	,NXI   ,NYI   ,NZI   ,MS   ,
     8  	 DE    ,NPC   ,PLD    ,WA(KWNF) ,WA(KWTF) ,
     9  	 WA(KWNS) ,FNORMX,FNORMY,FNORMZ,FTANGX,
     A  	 FTANGY,FTANGZ  ,DT2T  ,NOINT ,NELTST ,
     B  	 ITYPTST ,FRIC )
       NDEB=NDEB+MVSIZ
       IF (NDEB<NSC) GOTO 10
C-----------------------------------------------
C      TRIANGLES.
C-----------------------------------------------
       NDEB =0
 20    CONTINUE
       CALL I15TOTT1(NOINT ,NDEB, NTC,X     ,INTBUF_TAB%KSURF(1) ,
     2         IGRSURF  ,BUFSF ,WA(KTC) ,INTBUF_TAB%IRECTS ,INTBUF_TAB%NOLD ,
     3         XP1    ,XP2   ,XP3     ,XTK   ,YTK   ,
     4         ZTK    ,NTX   ,NTY     ,NTZ   ,ANS   ,
     5         DEPTH  ,XI     ,YI     ,ZI    ,NXI   ,
     6         NYI   ,NZI    ,ANSMX   ,INTBUF_TAB%HOLD ,INTBUF_TAB%IMPACT ,
     7         ITAB )
       CALL I15FORT1(NDEB, NTC, STFAC,X     ,V      ,
     2       INTBUF_TAB%KSURF(1) ,IGRSURF ,BUFSF ,WA(KTC) ,INTBUF_TAB%IRECTS,
     3       INTBUF_TAB%IMPACT,INTBUF_TAB%IOLD ,INTBUF_TAB%HOLD ,INTBUF_TAB%NOLD,
     4           INTBUF_TAB%DOLD ,XP1   ,XP2      ,XP3    ,XTK   ,YTK   ,
     5           ZTK   ,NTX   ,NTY   ,NTZ   ,ANS   ,
     6           DEPTH ,XI    ,YI    ,ZI     ,NXI   ,
     7           NYI   ,NZI   ,MS   ,DE    ,NPC   ,
     8           PLD    ,WA(KWNF) ,WA(KWTF) ,WA(KWNS) ,FNORMX,
     9           FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ  ,
     A           DT2T  ,NOINT , NELTST ,ITYPTST ,FRIC  )
       NDEB=NDEB+MVSIZ
       IF (NDEB<NTC) GOTO 20
C------
C      MARQUE LES NOEUDS / F<>0.
C------
       CALL I15MARQ(INTBUF_TAB%IRECTS,NSC,WA(KSC),NTC,WA(KTC),
     2  	  INTBUF_TAB%IMPACT,NSN,INTBUF_TAB%NSV,WA(KMARQND),NNC,
     3  	  WA(KNC))
C-----------------------------------------------
C      RETURN TO GLOBAL VECTORS.
C-----------------------------------------------
       CALL I15ASS(A   ,X     ,V    ,INTBUF_TAB%KSURF ,IGRSURF  ,
     2           BUFSF ,STIFN ,FSAV ,FCONT      ,FSKYI  ,
     3           ISKY  ,DE    ,WA(KWNF) ,WA(KWTF) ,WA(KWNS) ,
     4           FNORMX ,FNORMY ,FNORMZ ,FTANGX ,FTANGY   ,
     5           FTANGZ ,NNC    ,WA(KNC),H3D_DATA )
C-----------------------------------------------
      INTBUF_TAB%VARIABLES(9)=DE
C-----------------------------------------------
9999  CONTINUE
      RETURN
      END
