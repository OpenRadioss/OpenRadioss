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
Chd|  ANIMX                         source/output/anim/generate/animx.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SAV_BUF_POINT                 source/user_interface/eng_callback_c.c
Chd|        XANIM28                       source/elements/xelem/xanim28.F
Chd|        XANIM29                       source/output/anim/generate/xanim29.F
Chd|        XANIM30                       source/output/anim/generate/xanim30.F
Chd|        XANIM31                       source/output/anim/generate/xanim31.F
Chd|        XCOOR3                        source/elements/xelem/xcoor3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ANIMX(ELBUF_TAB,
     .           IPARG    ,ITAB     ,X        ,KXX      ,IXX    ,
     .           IPARTX   ,PM       ,GEO      ,BUFMAT   ,BUFGEO ,
     .           UIX      ,XUSR     ,NFACPTX  ,IXEDGE   ,IXFACET,
     .           IXSOLID  ,INUMX1   ,INUMX2   ,INUMX3   ,IOFFX1 ,
     .           IOFFX2   ,IOFFX3   ,XMASS1   ,XMASS2   ,XMASS3 ,
     .           XFUNC1   ,XFUNC2   ,XFUNC3   ,NANIM1D_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX,*),IXX(*),
     .   IPARTX(*), UIX(*), NFACPTX(3,*),
     .   IPARG(NPARG,*), ITAB(*),
     .   INUMX1(*), INUMX2(*), INUMX3(*),
     .   IOFFX1(*), IOFFX2(*), IOFFX3(*),
     .   IXEDGE(2,*), IXFACET(4,*), IXSOLID(8,*),
     .   NANIM1D_L
      my_real
     .   X(3,*), PM(NPROPM,*), GEO(NPROPG,*),
     .   BUFMAT(*) ,BUFGEO(*) ,
     .   XUSR(3,*) ,
     .   XMASS1(*), XMASS2(*), XMASS3(*),
     .   XFUNC1(10,*), XFUNC2(10,*), XFUNC3(10,*)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real 
     .     OFF, MASSELE, EINTELE
      INTEGER IPRT, NG, MYNEL, MYNFT, MYIAD, MYITY,
     .   I, J, K, IPROP, IMAT, NX,
     .   NB1, NB2, NB3, NB4, NB5, NBFI, UID,
     .   IGTYP,NUVAR,NUVARN,NUPARAM,
     .   L, NAX1D, NAX2D, NAX3D, NEDGE, NFACET, NSOLID,
     .   IADNOD, NUSR, NOSYS,KVAR,KVARN
      CHARACTER*40 MESS
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C     REAL
      my_real
     .   SYSFUS
C-----------------------------------------------
      DATA MESS/'MULTI-PURPOSE ELEMENT DISCRETIZATION    '/
C-----------------------------------------------
      NAX1D=0
      NAX2D=0
      NAX3D=0
C
      DO IPRT=1,NPART
        DO NG=1,NGROUP
          MYNEL   =IPARG(2,NG)
          MYNFT   =IPARG(3,NG)
          MYIAD   =IPARG(4,NG)
          MYITY   =IPARG(5,NG)
C
          GBUF => ELBUF_TAB(NG)%GBUF
C
          IF (MYITY == 100) THEN
            DO 150 I=1,MYNEL
            J=I+MYNFT
            IF (IPARTX(J) /= IPRT) GOTO 150
C
            IMAT =KXX(1,J)
            IPROP=KXX(2,J)
            NX   =KXX(3,J)
C
            IGTYP =  NINT(GEO(12,IPROP))
            NUVAR =  NINT(GEO(25,IPROP))
            NUVARN=  NINT(GEO(35,IPROP))
C
cc            NB1 =(I-1)*(3+NUVAR+NUVARN*NX)+MYIAD
cc            NB2 =NB1 +1
cc            NB3 =NB2 +1
cc            NB4 =NB3 +1
cc            NB5 =NB4 +NUVAR
cc            NBFI=NB5 +NUVARN*NX
            KVAR  = NUVAR*(I-1)+1
            KVARN = NUVARN*NX*(I-1)+1
C-------
C           FILL COORDINATES.
            CALL XCOOR3(X   ,KXX(1,J) ,IXX  ,ITAB ,NX ,
     2                  UID ,UIX ,XUSR )
            IADNOD=KXX(4,J)
            DO K=1,NX
             UIX(NX+K)=IXX(IADNOD+K-1)
            ENDDO
C-------
            CALL SAV_BUF_POINT(PM,1)
            CALL SAV_BUF_POINT(BUFMAT,2)
            CALL SAV_BUF_POINT(GEO,3)
            CALL SAV_BUF_POINT(BUFGEO,4)
C            CALL SAV_BUF_POINT(NPC,5)
C            CALL SAV_BUF_POINT(PLD,6)
C-------
            NEDGE  =0
            NFACET =0
            NSOLID =0
            OFF    = GBUF%OFF(I)
            EINTELE= GBUF%EINT(I)
            MASSELE= GBUF%MASS(I)
cc            OFF    =ELBUF(NB1)
cc            EINTELE=ELBUF(NB2)
cc            MASSELE=ELBUF(NB3)
            IF (IGTYP == 28) THEN
              CALL XANIM28(NX   ,UIX ,UID    ,XUSR  ,
     2        IOUT    ,IPROP ,IMAT   ,
     3        OFF     ,MASSELE ,EINTELE ,
     4        NEDGE          , NFACET          , NSOLID         , 
     5        IXEDGE(1,NAX1D+1), IXFACET(1,NAX2D+1),IXSOLID(1,NAX3D+1),
     6        XMASS1(NAX1D+1) , XMASS2(NAX2D+1) , XMASS3(NAX3D+1), 
     7        XFUNC1(1,NAX1D+1) ,XFUNC2(1,NAX2D+1) ,XFUNC3(1,NAX3D+1) ,
     8        NUVAR   ,GBUF%VAR(KVAR) ,NUVARN  ,GBUF%VARN(KVARN))
cc     8        NUVAR   ,ELBUF(NB4) ,NUVARN  ,ELBUF(NB5) )
            ELSEIF (IGTYP == 29) THEN
              CALL XANIM29(NX   ,UIX ,UID    ,XUSR  ,
     2        IOUT    ,IPROP ,IMAT   ,
     3        OFF     ,MASSELE ,EINTELE ,
     4        NEDGE          , NFACET          , NSOLID         , 
     5        IXEDGE(1,NAX1D+1), IXFACET(1,NAX2D+1),IXSOLID(1,NAX3D+1),
     6        XMASS1(NAX1D+1) , XMASS2(NAX2D+1) , XMASS3(NAX3D+1), 
     7        XFUNC1(1,NAX1D+1) ,XFUNC2(1,NAX2D+1) ,XFUNC3(1,NAX3D+1) ,
     8        NUVAR   ,GBUF%VAR(KVAR) ,NUVARN  ,GBUF%VARN(KVARN))
cc     8        NUVAR   ,ELBUF(NB4) ,NUVARN  ,ELBUF(NB5) )
            ELSEIF (IGTYP == 30) THEN
              CALL XANIM30(NX   ,UIX ,UID    ,XUSR  ,
     2        IOUT    ,IPROP ,IMAT   ,
     3        OFF     ,MASSELE ,EINTELE ,
     4        NEDGE          , NFACET          , NSOLID         , 
     5        IXEDGE(1,NAX1D+1), IXFACET(1,NAX2D+1),IXSOLID(1,NAX3D+1),
     6        XMASS1(NAX1D+1) , XMASS2(NAX2D+1) , XMASS3(NAX3D+1), 
     7        XFUNC1(1,NAX1D+1) ,XFUNC2(1,NAX2D+1) ,XFUNC3(1,NAX3D+1) ,
     8        NUVAR   ,GBUF%VAR(KVAR),NUVARN  ,GBUF%VARN(KVARN))
cc     8        NUVAR   ,ELBUF(NB4) ,NUVARN  ,ELBUF(NB5) )
            ELSEIF (IGTYP == 31) THEN
              CALL XANIM31(NX   ,UIX ,UID    ,XUSR  ,
     2        IOUT    ,IPROP ,IMAT   ,
     3        OFF     ,MASSELE ,EINTELE ,
     4        NEDGE          , NFACET          , NSOLID         , 
     5        IXEDGE(1,NAX1D+1), IXFACET(1,NAX2D+1),IXSOLID(1,NAX3D+1),
     6        XMASS1(NAX1D+1) , XMASS2(NAX2D+1) , XMASS3(NAX3D+1), 
     7        XFUNC1(1,NAX1D+1) ,XFUNC2(1,NAX2D+1) ,XFUNC3(1,NAX3D+1) ,
     8        NUVAR   ,GBUF%VAR(KVAR),NUVARN  ,GBUF%VARN(KVARN))
cc     8        NUVAR   ,ELBUF(NB4) ,NUVARN  ,ELBUF(NB5) )
            ENDIF
C--------
            NFACPTX(1,IPRT)=NFACPTX(1,IPRT)+NEDGE
            NFACPTX(2,IPRT)=NFACPTX(2,IPRT)+NFACET
            NFACPTX(3,IPRT)=NFACPTX(3,IPRT)+NSOLID
C--------
            DO L=1,NEDGE
              IOFFX1(NAX1D+L)=NINT(MIN(GBUF%OFF(I),ONE))
cc              IOFFX1(NAX1D+L)=NINT(MIN(ELBUF(NB1),ONE))
              INUMX1(NAX1D+L)=KXX(NIXX,J)
              IXEDGE(1,NAX1D+L)=IXX(IADNOD+IXEDGE(1,NAX1D+L)-1)
              IXEDGE(2,NAX1D+L)=IXX(IADNOD+IXEDGE(2,NAX1D+L)-1)
            ENDDO
            DO L=1,NFACET
              IOFFX2(NAX2D+L)=NINT(MIN(GBUF%OFF(I),ONE))
cc              IOFFX2(NAX2D+L)=NINT(MIN(ELBUF(NB1),ONE))
              INUMX2(NAX2D+L)=KXX(NIXX,J)
              IXFACET(1,NAX2D+L)=IXX(IADNOD+IXFACET(1,NAX2D+L)-1)
              IXFACET(2,NAX2D+L)=IXX(IADNOD+IXFACET(2,NAX2D+L)-1)
              IXFACET(3,NAX2D+L)=IXX(IADNOD+IXFACET(3,NAX2D+L)-1)
C             if 3 nodes facet : node 4 should be equal to node 3.
              IXFACET(4,NAX2D+L)=IXX(IADNOD+IXFACET(4,NAX2D+L)-1)
            ENDDO
            DO L=1,NSOLID
              IOFFX3(NAX3D+L)=NINT(MIN(GBUF%OFF(I),ONE))
cc              IOFFX3(NAX3D+L)=NINT(MIN(ELBUF(NB1),ONE))
              INUMX3(NAX3D+L)=KXX(NIXX,J)
              IXSOLID(1,NAX3D+L)=IXX(IADNOD+IXSOLID(1,NAX3D+L)-1)
              IXSOLID(2,NAX3D+L)=IXX(IADNOD+IXSOLID(2,NAX3D+L)-1)
              IXSOLID(3,NAX3D+L)=IXX(IADNOD+IXSOLID(3,NAX3D+L)-1)
              IXSOLID(4,NAX3D+L)=IXX(IADNOD+IXSOLID(4,NAX3D+L)-1)
              IXSOLID(5,NAX3D+L)=IXX(IADNOD+IXSOLID(5,NAX3D+L)-1)
              IXSOLID(6,NAX3D+L)=IXX(IADNOD+IXSOLID(6,NAX3D+L)-1)
              IXSOLID(7,NAX3D+L)=IXX(IADNOD+IXSOLID(7,NAX3D+L)-1)
              IXSOLID(8,NAX3D+L)=IXX(IADNOD+IXSOLID(8,NAX3D+L)-1)
            ENDDO
C--------
            NAX1D=NAX1D+NEDGE
            NAX2D=NAX2D+NFACET
            NAX3D=NAX3D+NSOLID
            NANIM1D_L = NAX1D
C123456789C123456789C123456789C123456789C123456789C123456789C123456789C1
            IF (NAX1D > NANIM1D .OR. NAX2D > NANIM2D .OR.
     .          NAX3D > NANIM3D) THEN
              CALL ANCMSG(MSGID=28,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
 150        CONTINUE
          ENDIF ! IF (MYITY == 100)
        ENDDO ! DO NG=1,NGROUP
      ENDDO ! DO IPRT=1,NPART
C----------------------------------
C      IF (IERR>0) THEN
C        WRITE(ISTDO,*)
C     .  ' ** ERROR DISCRETIZATION OF MULTI-PURPOSE ELEMENTS.'
C        WRITE(IOUT,*)
C     .  ' ** ERROR DISCRETIZATION OF MULTI-PURPOSE ELEMENTS.'
C        CALL ARRET(2)
C      ENDIF
C----------------------------------
      RETURN
      END

