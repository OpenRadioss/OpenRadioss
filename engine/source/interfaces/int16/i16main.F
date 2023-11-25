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
      MODULE I16MAIN_MOD
#include "my_real.inc"
        my_real :: XSLVG(7), XMSRG(7)
      END MODULE
Chd|====================================================================
Chd|  I16MAIN                       source/interfaces/int16/i16main.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        I10LAGM                       source/interfaces/int16/i10lagm.F
Chd|        I16BUCE                       source/interfaces/int16/i16buce.F
Chd|        I16CRIT                       source/interfaces/int16/i16crit.F
Chd|        I16LAGM                       source/interfaces/int16/i16lagm.F
Chd|        I20LAGM                       source/interfaces/int16/i20lagm.F
Chd|        I8LAGM                        source/interfaces/int16/i8lagm.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I16MAIN(
     1          NIN    ,IPARI    ,INTBUF_TAB ,X      ,V      ,
     2          A      ,ITASK    ,IGRNOD     ,EMINX  ,WAT    ,
     3          MS     ,IADLL    ,LLL        ,JLL    ,SLL    ,
     4          XLL    ,N_MUL_MX ,IXS        ,IXS16  ,IXS20  ,
     5          NKMAX  ,IXS10    ,COMNTAG    ,IGRBRIC)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE I16MAIN_MOD
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
C                  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"

      COMMON /i16tmp/size
      my_real
     .   SIZE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK ,N_MUL_MX ,NKMAX   
      INTEGER IPARI(NPARI,NINTER),
     .        IADLL(*) ,LLL(*) ,JLL(*) ,SLL(*) ,
     .        IXS(NIXS,*)  ,IXS16(8,*)  ,IXS20(12,*) ,IXS10(6,*),
     .        COMNTAG(*)
C     REAL
      my_real 
     .   X(3,*), V(3,*), A(3,*), MS(*),EMINX(*),WAT(*),
     .   XLL(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,itri 
C     REAL
      my_real
     .   STARTT, STOPT,XX,XY,XZ,TZINF,MINBOX
      SAVE itri
      data itri/0/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NOINT       = IPARI(15,NIN)
      MULTIMP     = IPARI(23,NIN)
      IGN         = IPARI(36,NIN)
      IGE         = IPARI(34,NIN)
      NSN   =IGRNOD(IGN)%NENTITY
      NME   =IGRBRIC(IGE)%NENTITY
C
      MX_CAND=MULTIMP*NSN
      STARTT = INTBUF_TAB(NIN)%VARIABLES(3)
      STOPT  = INTBUF_TAB(NIN)%VARIABLES(11)
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
C -------------------------------------------------------------
       CALL MY_BARRIER
C=======================================================================
C      TEST pour savoir s'il faut retrier + initialisation
C=======================================================================
       CALL I16CRIT(
     1   X    ,IGRNOD(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY ,NSN  ,EMINX      ,
     2   NME    ,ITASK       ,INTBUF_TAB(NIN)%XSAV ,IXS  ,IXS16      ,
     3   IXS20    ,IXS10       ,V       ,A   )

C -------------------------------------------------------------
       CALL MY_BARRIER
C -------------------------------------------------------------
C error in the estimation formula
       TZINF  = INTBUF_TAB(NIN)%VARIABLES(4) * SIZE / NME / 6
       MINBOX = INTBUF_TAB(NIN)%VARIABLES(5) * SIZE / NME / 6
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
           CALL I16BUCE(
     1   IGRNOD(IGN)%ENTITY,IXS   ,IXS16   ,IXS20  ,IGRBRIC(IGE)%ENTITY ,
     2   NME_T      ,WAT   ,NSN    ,INTBUF_TAB(NIN)%CAND_E,INTBUF_TAB(NIN)%CAND_N,
     3   NOINT      ,IPARI(35,NIN) ,TZINF   ,MINBOX    ,EMINX      ,
     4   INTBUF_TAB(NIN)%XSAV,ITASK   ,X    ,V    ,A        ,
     5   MX_CAND    ,IXS10   ,ESH_T)
C -------------------------------------------------------------
         CALL MY_BARRIER
C -------------------------------------------------------------
       ENDIF
C=======================================================================
C      CONTACT
C=======================================================================
       CALL I8LAGM(
     1   X   ,V         ,LLL    ,JLL     ,SLL        ,
     2   XLL   ,INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%CAND_E,IPARI(35,NIN),IXS  ,
     3   IADLL   ,EMINX        ,IGRNOD(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   N_MUL_MX     ,ITASK   ,A         ,IPARI(30,NIN),
     5   NIN   ,NKMAX        ,COMNTAG   )
       CALL I10LAGM(
     1   X       ,V            ,LLL       ,JLL           ,SLL         ,
     2   XLL     ,INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%CAND_E,IPARI(35,NIN),IXS       ,
     3   IXS10   ,IADLL        ,EMINX     ,IGRNOD(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   N_MUL_MX     ,ITASK     ,A            ,IPARI(30,NIN),
     5   NIN     ,NKMAX        ,COMNTAG   )
       CALL I16LAGM(
     1   X   ,V         ,LLL    ,JLL     ,SLL        ,
     2   XLL   ,INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%CAND_E,IPARI(35,NIN),IXS ,
     3   IXS16   ,IADLL        ,EMINX    ,IGRNOD(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   N_MUL_MX     ,ITASK   ,A         ,IPARI(30,NIN),
     5   NIN   ,NKMAX        ,COMNTAG   )
       CALL I20LAGM(
     1   X   ,V         ,LLL    ,JLL     ,SLL        ,
     2   XLL   ,INTBUF_TAB(NIN)%CAND_N,INTBUF_TAB(NIN)%CAND_E,IPARI(35,NIN),IXS       ,
     3   IXS20   ,IADLL        ,EMINX    ,IGRNOD(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     4   N_MUL_MX     ,ITASK   ,A         ,IPARI(30,NIN),
     5   NIN   ,NKMAX        ,COMNTAG   )
!
      CALL MY_BARRIER
!
      RETURN
      END
