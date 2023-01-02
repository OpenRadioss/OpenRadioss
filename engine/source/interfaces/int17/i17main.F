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
Chd|  I17MAIN                       source/interfaces/int17/i17main.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        I17BUCE                       source/interfaces/int17/i17buce.F
Chd|        I17CRIT                       source/interfaces/int17/i17crit.F
Chd|        I17LAGM                       source/interfaces/int17/i17lagm.F
Chd|        I21LAGM                       source/interfaces/int17/i21lagm.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I17MAIN(
     1          NIN    ,IPARI  ,INTBUF_TAB     ,X       ,
     2          V      ,A      ,ITASK  ,IGRBRIC ,
     3          EMINX  ,MS     ,NC     ,IADLL  ,
     4          LLL    ,JLL    ,SLL    ,XLL    ,N_MUL_MX,
     5          IXS    ,IXS16  ,IXS20  ,NKMAX  ,COMNTAG )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C                  
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C                  
C     X     3,NUMNOD                 COORDONNEES                        E
C                  
C     V     3,NUMNOD                 VITESSES                           E
C                  
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C                  
C    WAT    4*(NME+100)+4*(NMES+100) TABLEAUX DE TRI                  TMP_TASK
C                  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
      COMMON /I17TRIC/SIZE
      my_real
     .        SIZE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK ,NC,N_MUL_MX ,NKMAX   
      INTEGER IPARI(NPARI,NINTER),
     .        IADLL(*) ,LLL(*) ,JLL(*) ,SLL(*) , COMNTAG(*),
     .        IXS(NIXS,*)  ,IXS16(8,*)  ,IXS20(12,*)
C     REAL
      my_real 
     .   X(3,*), V(3,*), A(3,*), MS(*),EMINX(6,*),
     .   XLL(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NME,NAD,EAD,LWAT,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NMES,
     .        IDUM1, IDUM2, IDUM3, NMESR 
C     REAL
      my_real
     .   STARTT, STOPT,XX,XY,XZ,TZINF,MINBOX,XMSR(6),SIZE_T
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NOINT       = IPARI(15,NIN)
      MULTIMP     = IPARI(23,NIN)
      IGN         = IPARI(36,NIN)
      IGE         = IPARI(34,NIN)
      NMES  =IGRBRIC(IGN)%NENTITY
      NME   =IGRBRIC(IGE)%NENTITY
      LWAT = 4*(NME+100)+4*(NMES+100)
      MX_CAND=MULTIMP*NMES
      STARTT = INTBUF_TAB%VARIABLES(3)
      STOPT  = INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C -------------------------------------------------------------
       NME_T=NME/NTHREAD
       ESH_T = ITASK*NME_T
       IF(ITASK==NTHREAD-1)NME_T=NME-(NTHREAD-1)*(NME/NTHREAD)
       SIZE   = ZERO
       XSLVG(1) = -EP30
       XSLVG(2) = -EP30
       XSLVG(3) = -EP30
       XSLVG(4) =  EP30
       XSLVG(5) =  EP30
       XSLVG(6) =  EP30
       XMSRG(1) = -EP30
       XMSRG(2) = -EP30
       XMSRG(3) = -EP30
       XMSRG(4) =  EP30
       XMSRG(5) =  EP30
       XMSRG(6) =  EP30 
       XMSR(1) = -EP30
       XMSR(2) = -EP30
       XMSR(3) = -EP30
       XMSR(4) =  EP30
       XMSR(5) =  EP30
       XMSR(6) =  EP30       
C -------------------------------------------------------------
       CALL MY_BARRIER
C=======================================================================
C      TEST pour savoir s'il faut retrier + initialisation
C=======================================================================
       SIZE_T = ZERO
       CALL I17CRIT(
     1   X        ,V           ,A         ,IGRBRIC(IGE)%ENTITY ,EMINX,
     2   NME      ,ITASK       ,INTBUF_TAB%XSAV,IXS           ,IXS16,
     3   IXS20    ,XMSR        ,SIZE_T)
C -------------------------------------------------------------
#include "lockon.inc"
         XMSRG(1)=MAX(XMSRG(1),XMSR(1))
         XMSRG(2)=MAX(XMSRG(2),XMSR(2))
         XMSRG(3)=MAX(XMSRG(3),XMSR(3))
         XMSRG(4)=MIN(XMSRG(4),XMSR(4))
         XMSRG(5)=MIN(XMSRG(5),XMSR(5))
         XMSRG(6)=MIN(XMSRG(6),XMSR(6))
#include "lockoff.inc"
       CALL MY_BARRIER
C -------------------------------------------------------------
       CALL I17CRIT(
     1   X	  ,V	       ,A	,IGRBRIC(IGN)%ENTITY,EMINX(1,NME+1),
     2   NMES	  ,ITASK       ,INTBUF_TAB%XSAV,IXS	   ,IXS16         ,
     3   IXS20    ,XSLVG       ,SIZE_T  )
C -------------------------------------------------------------
#include "lockon.inc"
         XSLVG(1)=MAX(XSLVG(1),XMSR(1))
         XSLVG(2)=MAX(XSLVG(2),XMSR(2))
         XSLVG(3)=MAX(XSLVG(3),XMSR(3))
         XSLVG(4)=MIN(XSLVG(4),XMSR(4))
         XSLVG(5)=MIN(XSLVG(5),XMSR(5))
         XSLVG(6)=MIN(XSLVG(6),XMSR(6))
         SIZE = SIZE + SIZE_T
#include "lockoff.inc"
       CALL MY_BARRIER
C -------------------------------------------------------------
C       TZINF  = BUFIN(JD(11)+3) * SIZE / NME / 18
C       MINBOX = BUFIN(JD(11)+4) * SIZE / NME / 18
C error in the estimation formula
       TZINF  = INTBUF_TAB%VARIABLES(4) * SIZE / (NME+NMES) / 6
       MINBOX = INTBUF_TAB%VARIABLES(5) * SIZE / (NME+NMES) / 6
       XX = MAX(XSLVG(1)-XMSRG(4),XMSRG(1)-XSLVG(4))
       XY = MAX(XSLVG(2)-XMSRG(5),XMSRG(2)-XSLVG(5))
       XZ = MAX(XSLVG(3)-XMSRG(6),XMSRG(3)-XSLVG(6))
C -------------------------------------------------------------
       IF(XX**2 +  XY**2 + XZ**2 >= TZINF**2) THEN
C=======================================================================
C        TRI
C=======================================================================
        IPARI(35,NIN) = 0
C -------------------------------------------------------------
         CALL MY_BARRIER
C -------------------------------------------------------------
	 CALL I17BUCE(
     1   IGRBRIC(IGN)%ENTITY,IXS	 ,IXS16   ,IXS20	,IGRBRIC(IGE)%ENTITY ,
     2   NME_T      ,LWAT	 ,NMES    ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3   NOINT      ,IPARI(35,NIN) ,TZINF ,MINBOX	,EMINX        ,
     4   INTBUF_TAB%XSAV,ITASK	 ,X	  ,V		,A	      ,
     5   MX_CAND    ,EMINX(1,NME+1),ESH_T ,INTBUF_TAB%FROTS,INTBUF_TAB%KS,
     6   IDUM1      ,IDUM2	 ,IDUM3   ,NIN  	,NMESR        ,
     7   V	    )
C IDUM1 & IDUM2 replace ISENDTO, IRCVFROM useful for SPMD i17 pena
C -------------------------------------------------------------
         CALL MY_BARRIER
C -------------------------------------------------------------
       ENDIF
C=======================================================================
C      CONTACT
C=======================================================================
       CALL I17LAGM(
     1   X	 ,V	       ,LLL	  ,JLL  	 ,SLL	      ,
     2   XLL	 ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,IPARI(35,NIN),IXS,
     3   IXS16   ,IADLL        ,EMINX	  ,IGRBRIC(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   NC	 ,N_MUL_MX     ,ITASK	  ,A		,IPARI(30,NIN),
     5   NIN	 ,NKMAX        ,EMINX(1,NME+1),COMNTAG  )
       CALL I21LAGM(
     1   X       ,V            ,LLL       ,JLL           ,SLL         ,
     2   XLL     ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,IPARI(35,NIN),IXS,
     3   IXS20   ,IADLL        ,EMINX     ,IGRBRIC(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   NC      ,N_MUL_MX     ,ITASK     ,A            ,IPARI(30,NIN),
     5   NIN     ,NKMAX        ,EMINX(1,NME+1),COMNTAG  )
C
       CALL MY_BARRIER
C
      RETURN
      END
C
