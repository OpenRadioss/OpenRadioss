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
Chd|  I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I11COQ                        source/interfaces/inter3d1/i11coq.F
Chd|        I11FIL                        source/interfaces/inter3d1/i11coq.F
Chd|        I11GMX3                       source/interfaces/inter3d1/i11gmx3.F
Chd|        I11SOL                        source/interfaces/inter3d1/i11sol.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F
Chd|====================================================================
      SUBROUTINE I11STI3(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINTR ,SLSFAC,
     3 NTY   ,GAPMAX,NOINT ,GAP_SM,
     4 MS    ,IXTG  ,IXT   ,IXP   ,IXR   ,
     5 IGAP  ,GAPMIN,GAP0  ,GAPINF,IPARTC,
     6 IPARTTG,THK  ,THK_PART,PERCENT_SIZE,GAP_L,
     7 NOD2EL1D,KNOD2EL1D,ITAB,IXS10,ID,TITR,
     8 KXX  ,IXX,IGEO, KNOD2ELS,KNOD2ELC ,
     9 KNOD2ELTG,NOD2ELS,NOD2ELC,NOD2ELTG,LELX,
     A FILLSOL  ,INTTH  ,DRAD   ,AREA  ,IELEC,
     B PM_STACK ,IWORKSH,IT19,BGAPSMX,INTFRIC,
     C IPARTS ,TAGPRT_FRIC,IPARTFRIC,INTBUF_FRIC_TAB,
     D IPARTT   ,IPARTP   ,IPARTX    ,IPARTR , IREM_GAP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE INTBUF_FRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr08_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "warn_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINTR, NTY, NOINT,IGAP,INTTH,INTFRIC,IREM_GAP
C     REAL
      my_real
     .   SLSFAC, GAPMAX,GAPMIN,GAP0,PERCENT_SIZE,DRAD,BGAPSMX
      INTEGER IRECT(2,*), IXS(NIXS,*), IXC(NIXC,*),
     .   IXTG(NIXTG,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .   IPARTC(*), IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*),ITAB(*),
     .   IXS10(6,*),KXX(NIXX,*),IXX(*),IGEO(NPROPGI,*),
     .    KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*),
     .    NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),IELEC(*),IWORKSH(3,*),
     .    TAGPRT_FRIC(*),IPARTFRIC(*),IPARTS(*),
     .    IPARTT(*) ,IPARTP(*) ,IPARTX(*) ,IPARTR(*)
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*),
     .   MS(*),GAP_SM(*),XL2, GAPINF,THK(*),THK_PART(*),
     .   GAP_L(*),LELX(*), FILLSOL(*),AREA(*),PM_STACK(20,*)
      INTEGER ID,IT19
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, INRT, NELS, MT, JJ, JJJ, NELC, J,
     .   MG, NUM, NPT, LL, L, NN, NELTG,NELT,NELP,NELR,
     .   IGTYP, IP,N1,N2,K,T,P,R,NELX,IPGMAT,IGMAT,IE,
     .   JJ1,JJ2,IEC,I1,I2,I3,K1,K2,IPFMAX,IPL,IPC,
     .   IE1(50,2),IE2(50,2),ISUBSTACK,IPG,N3,N4,N5,N6,N7,N8
      my_real
     .   DXM, GAPMX, GAPMN, AREASS, VOL, DX,GAP1,GAPS1,GAPTMP,XL,
     .   GET_U_GEO,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,X6,Y6,Z6,X7,Y7,Z7,X8,Y8,Z8,
     .   XX1(4),XX2(4), XX3(4) ,FACE(6)
C-----------------------------------------------
      EXTERNAL GET_U_GEO
C     Stiffness: stiffness of the shell if the segment belongs
C     to a shell and a solid (except if the stiffness of the shell is 0
      DXM=ZERO
      NDX=0
      GAPS1=ZERO
      GAPMX=EP30
      GAPMN=EP30
      IPGMAT = 700
      IF(IGAP==3)THEN
        DO I=1,NRT
          GAP_L(I)=EP30
        ENDDO
      ENDIF

C
      DO 500 I=1,NRT
        STF(I)=ZERO
        GAP_SM(I)=ZERO
        INRT=I
        CALL I11GMX3(X,IRECT,INRT,GAPMX,XL2)
C----------------------
C     Solids
C----------------------
        CALL I11SOL(X,IRECT,IXS,NINTR,NELS,INRT,
     .             AREASS,NOINT,KNOD2ELS,NOD2ELS,IXS10)
        IF(NELS/=0) THEN
          MT=IXS(1,NELS)
          IF(MT>0)THEN
            DO 100 JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
  100       CONTINUE
            CALL VOLINT(VOL)
            IF(XL2>0.0)THEN
              STF(I)=SLSFAC*FILLSOL(NELS)*VOL*PM(32,MT)/XL2
            ELSE
              STF(I)=ZERO
            ENDIF
          ELSE
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF
          ENDIF

C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTS(NELS)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ENDIF
        CALL I11COQ(IRECT   ,IXC      ,IXTG   ,NINTR   ,NELC    ,
     .              NELTG   ,INRT     ,GEO    ,PM      ,THK     ,
     .              IGEO    ,KNOD2ELC,KNOD2ELTG,NOD2ELC,NOD2ELTG,
     .              PM_STACK,IWORKSH)
        IF(NELTG/=0) THEN
C
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IP = IPARTTG(NELTG)
          IGMAT = IGEO(98,MG)
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)
          ELSEIF (THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK ==0)THEN
            DX=THK(NUMELC+NELTG)
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            DX=THK(NUMELC+NELTG)
          ELSE
            DX=GEO(1,MG)
          ENDIF
          GAP_SM(I)=HALF*DX
          GAPS1=MAX(GAPS1,GAP_SM(I))
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
              STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
            ELSEIF(IGTYP == 52 .OR.
     .         ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              STF(I)=SLSFAC*DX*PM_STACK( 2 ,ISUBSTACK)
            ELSE
              STF(I)=SLSFAC*DX*PM(20,MT)
            ENDIF
          ELSE
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF

          ENDIF

C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTTG(NELTG)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------

        ELSEIF(NELC/=0) THEN
C
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IGTYP = IGEO(11,MG)
          IP = IPARTC(NELC)
          IGMAT = IGEO(98,MG)
          IF (THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)
          ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0 ) THEN
            DX=THK(NELC)
          ELSEIF(IGTYP ==17 .OR. IGTYP == 51 .OR. IGTYP == 52)THEN
            DX=THK(NELC)
          ELSE
            DX=GEO(1,MG)
          ENDIF
          GAP_SM(I)=HALF*DX
          GAPS1=MAX(GAPS1,GAP_SM(I))
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
              STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
            ELSEIF(IGTYP == 52 .OR.
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
              ISUBSTACK = IWORKSH(3,NELC)
              STF(I)=SLSFAC*DX*PM_STACK( 2 ,ISUBSTACK)
            ELSE
              STF(I)=SLSFAC*DX*PM(20,MT)
            ENDIF
          ELSE
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
            ENDIF
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
            ENDIF
          ENDIF

C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTC(NELC)

            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ENDIF
        CALL I11FIL(IRECT,IXT ,IXP,IXR,NINTR,NELT ,
     .              NELP,NELR,NELX,INRT,NOD2EL1D,
     .              KNOD2EL1D,KXX,IXX)
        IF(NELT/=0) THEN
C
          MT=IXT(1,NELT)
          MG=IXT(4,NELT)
          DX=SQRT(GEO(1,MG))
          GAP_SM(I)=MAX(GAP_SM(I),HALF*DX)
          GAPS1=MAX(GAPS1,GAP_SM(I))
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            STF(I)=SLSFAC*DX*PM(20,MT)
            IF (NSUBDOM>0) STF(I)=SLSFAC*DX*PM_R2R(MT)
          ELSE
C         IF(NINTR>=0)WRITE (IOUT,1700) IXT(NIXT,NELT),I, NOINT
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
            ENDIF
C         IF(NINTR<0)WRITE (IOUT,1800) IXT(NIXT,NELT),I, NOINT
C         IWARN=IWARN+1
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
            ENDIF
          ENDIF
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTT(NELT)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ELSEIF(NELP/=0) THEN
C
          MT=IXP(1,NELP)
          MG=IXP(5,NELP)
          DX=SQRT(GEO(1,MG))
          GAP_SM(I)=MAX(GAP_SM(I),HALF*DX)
          GAPS1=MAX(GAPS1,GAP_SM(I))
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            STF(I)=SLSFAC*DX*PM(20,MT)
            IF (NSUBDOM>0) STF(I)=SLSFAC*DX*PM_R2R(MT)
          ELSE
C         IF(NINTR>=0)WRITE (IOUT,1700) IXP(NIXP,NELP),I, NOINT
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXP(NIXP,NELP),
     .                    C2='BEAM',
     .                    I3=I)
            ENDIF
C         IF(NINTR<0)WRITE (IOUT,1800) IXP(NIXP,NELP),I, NOINT
C         IWARN=IWARN+1
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXP(NIXP,NELP),
     .                    C2='BEAM',
     .                    I3=I)
            ENDIF
          ENDIF
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTP(NELP)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ELSEIF(NELR/=0) THEN
C
          MG=IXR(1,NELR)
          MT = IXR(5,NELR)
          IF(MG>0)THEN
            IGTYP=NINT(GEO(12,MG))
            IF(IGTYP==4.OR.IGTYP==12)THEN
              STF(I)=SLSFAC*GEO(2,MG)
            ELSEIF(IGTYP==8.OR.IGTYP==13)THEN
              STF(I)=SLSFAC*MAX(GEO(3,MG),GEO(10,MG),GEO(15,MG))
            ELSEIF(IGTYP == 23)THEN
              STF(I)=SLSFAC*MAX(PM(191,MT),PM(192,MT),PM(193,MT))
            ELSEIF(IGTYP==25)THEN
              STF(I)=SLSFAC*GEO(10,MG)
            ELSEIF(IGTYP>=29)THEN
              STF(I)=SLSFAC*GEO(3,MG)
            ELSE
              WRITE(6,'(A)') 'INTERNAL ERROR 987'
              CALL MY_EXIT(2)
C           STOP 987
            ENDIF
          ELSE
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
            ENDIF
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
            ENDIF
          ENDIF
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTR(NELR)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ELSEIF(NELX/=0) THEN
C
          MG=KXX(2,NELX)
          IF(MG>0)THEN
            STF(I)=SLSFAC*GET_U_GEO(4,MG)*(KXX(3,NELX)-1)/LELX(NELX)
          ELSE
            IF(NINTR>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
            ENDIF
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
            ENDIF
          ENDIF
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTX(NELX)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRIC(INRT) = IPL
            ENDIF
          ENDIF
C------------------------------------
        ENDIF
C
        IF (IMACH/=3) THEN
          IF(NELS+NELC+NELTG+NELT+NELP+NELR+NUMELX==0.AND.SLSFAC>0.)
     .                                                               THEN
C       IF(NINTR>0) WRITE (IOUT,1100) I, NOINT
            IF(NINTR>0) THEN
              CALL ANCMSG(MSGID=92,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
            ENDIF
C       IF(NINTR<0) WRITE (IOUT,1200) I, NOINT
C       IWARN=IWARN+1
            IF(NINTR<0) THEN
              CALL ANCMSG(MSGID=93,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
            ENDIF
          ENDIF
        ELSEIF(NELS+NELC+NELTG+NELT+NELP+NELR+NUMELX==0.)THEN
          IF(NINTR>0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
          ENDIF
          IF(NINTR<0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
          ENDIF
        ENDIF
  500 CONTINUE
C---------------------------------------------
C
C Igap == 3
C
      IF(IGAP==3)THEN
        DO I=1,NRT
          XL = EP30
          N1=IRECT(1,I)
          N2=IRECT(2,I)
          IF(N1 /= N2 .AND. N1 /= 0)
     .        XL=MIN(XL,SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+
     .                 (X(3,N1)-X(3,N2))**2))

          DO J=1,2
            N1=IRECT(J,I)
            DO K=KNOD2EL1D(N1)+1,KNOD2EL1D(N1+1)
              IF (NOD2EL1D(K) <= NUMELT
     .                       .AND. NOD2EL1D(K) /= ZERO) THEN
                T = NOD2EL1D(K)
                XL=MIN(XL,SQRT((X(1,IXT(2,T))-X(1,IXT(3,T)))**2+
     .                   (X(2,IXT(2,T))-X(2,IXT(3,T)))**2+
     .                   (X(3,IXT(2,T))-X(3,IXT(3,T)))**2))
              ELSEIF (NOD2EL1D(K) <= NUMELT+NUMELP
     .                       .AND. NOD2EL1D(K) /= ZERO) THEN
                P = NOD2EL1D(K) - NUMELT
                XL=MIN(XL,SQRT((X(1,IXP(2,P))-X(1,IXP(3,P)))**2+
     .                   (X(2,IXP(2,P))-X(2,IXP(3,P)))**2+
     .                   (X(3,IXP(2,P))-X(3,IXP(3,P)))**2))
              ELSEIF (NOD2EL1D(K) <= NUMELT+NUMELP+NUMELR
     .                       .AND. NOD2EL1D(K) /= ZERO) THEN
                R = NOD2EL1D(K) - NUMELT - NUMELP
                XL=MIN(XL,SQRT((X(1,IXR(2,R))-X(1,IXR(3,R)))**2+
     .                   (X(2,IXR(2,R))-X(2,IXR(3,R)))**2+
     .                   (X(3,IXR(2,R))-X(3,IXR(3,R)))**2))
              ENDIF
            ENDDO
          ENDDO
          DO J=1,2
            GAP_L(I)=
     .        MIN(GAP_L(I),PERCENT_SIZE*XL)
          ENDDO
        ENDDO
      ENDIF
C---------------------------
C     GAP
C---------------------------
      GAPMX=SQRT(GAPMX)
      IF(IGAP==0)THEN
C---------------------------
C     GAP  FIXE
C---------------------------
        IF(GAP0>ZERO)THEN
          GAP1 = GAP0
        ELSE
          IF(NDX/=0)THEN
            GAP1 = MIN(HALF*GAPMX,DXM/NDX)
          ELSE
            GAP1 = EM01* GAPMX
          ENDIF
          IF ((NINTR<0).AND.(IT19==0)) WRITE(IOUT,1300)HALF*(GAPMIN+GAP1)
        ENDIF
C
        IF(NINTR<0) GAP1 = HALF*(GAPMIN+GAP1)
        GAPMIN = GAP1
        GAPMAX = GAP1
C
        IF ((GAP1>HALF*GAPMX).AND.(IREM_GAP/=2)) THEN
C          WRITE(IOUT,1400)GAP1,0.5*GAPMX
C          IWARN=IWARN+1
          GAPTMP = HALF*GAPMX
          CALL ANCMSG(MSGID=94,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAP1,
     .                R2=GAPTMP)
        ENDIF
      ELSE
C---------------------------
C GAP VARIABLE
C---------------------------
        BGAPSMX = ZERO
        IF(GAP0>ZERO)THEN
          GAP1 = GAP0
        ELSE
          IF(NDX/=0)THEN
            GAP1 = MIN(HALF*GAPMX,GAPMN)
          ELSE
            GAP1 = EM01 * GAPMX
          ENDIF
          IF ((NINTR<0).AND.(IT19==0)) WRITE(IOUT,1300)HALF*(GAPMIN+GAP1)
        ENDIF
C GAP MINI ET SUP DES GAPS VARIABLES
        IF(NINTR>0)THEN
          GAPMIN = GAP1
          GAPMAX = GAPS1
        ELSE
          GAPMIN = HALF*(GAPMIN+GAP1)
          GAPMAX = MAX(GAPMAX+GAPS1,GAPMIN)
          BGAPSMX = MAX(BGAPSMX,GAPS1)
        ENDIF
C
        IF ((GAPMAX>HALF*GAPMX) .AND. (IGAP/=3).AND.(IREM_GAP/=2))THEN
C          WRITE(IOUT,1400)GAPMAX,0.5*GAPMX
C          IWARN=IWARN+1
          GAPTMP = HALF*GAPMX
          CALL ANCMSG(MSGID=94,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAPMAX,
     .                R2=GAPTMP)
        ENDIF
      ENDIF
C---------------------------
C     STIF GLOBAL
C---------------------------
      IF(SLSFAC<ZERO)THEN
        DO I=1,NRT
          STF(I)=-SLSFAC
        ENDDO
      ENDIF
C---------------------------------------------
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      IF (IGAP==0) THEN
        GAPINF=GAPMAX
      ELSEIF(IGAP==1 .OR. IGAP==2) THEN
        DO I = 1, NRT
          GAPINF = MIN(GAPINF,GAP_SM(I))
        ENDDO
      ELSEIF(IGAP==3) THEN
        DO I = 1, NRT
          GAPINF = MIN(GAPINF,MIN(GAP_SM(I),GAP_L(I)))
        ENDDO
      ENDIF
C
      IF(INTTH/=0)THEN
        IF(DRAD==ZERO)THEN
C by default Drad = max( sup des gaps , largeur des elts )
          DRAD=MAX(GAP1,GAPMX)
        ELSEIF(DRAD<GAP1)THEN
C Drad  > gap
          DRAD=GAP1
        END IF
        WRITE(IOUT,2001)DRAD

C Performance warning
        IF(DRAD>GAPMX)THEN
          CALL ANCMSG(MSGID=918,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=DRAD ,
     .                R2=GAPMX,
     .                I2=ID)
        END IF
      END IF

C second. surface--
      IF(INTTH > 0 ) THEN
        IF(NELC/=0.OR.NELTG/=0) THEN
          DO I=1,NRT
            AREA(I) = ZERO
            JJ1 = 0
            DO J=KNOD2ELC(IRECT(1,I))+1,KNOD2ELC(IRECT(1,I)+1)
              JJ1 = JJ1 +1
              IE1(JJ1,1) = NOD2ELC(J)
              IE1(JJ1,2) = 4
            ENDDO
            DO J= KNOD2ELTG(IRECT(1,I))+1,KNOD2ELTG(IRECT(1,I)+1)
              JJ1 = JJ1 +1
              IE1(JJ1,1) = NOD2ELTG(J)
              IE1(JJ1,2) = 3
            ENDDO
            JJ2 = 0
            DO J=KNOD2ELC(IRECT(2,I))+1,KNOD2ELC(IRECT(2,I)+1)
              JJ2 = JJ2 +1
              IE2(JJ2,1) = NOD2ELC(J)
              IE2(JJ2,2) = 4
            ENDDO
            DO J= KNOD2ELTG(IRECT(2,I))+1,KNOD2ELTG(IRECT(2,I)+1)
              JJ2 = JJ2 +1
              IE2(JJ2,1) = NOD2ELTG(J)
              IE2(JJ2,2) = 3
            ENDDO
            IEC = 0
            DO J=1,JJ1
              DO K=1,JJ2
                IF(IE1(J,1)==IE2(K,1)) THEN
                  IE = IE1(J,1)
                  IEC = IEC +1
                  IF(IE1(J,2)==4) THEN
                    SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
                    SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
                    SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
                    SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
                    SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
                    SZ2 = X(3,IXC(5,IE)) - X(3,IXc(3,IE))
                    SX3  = SY1*SZ2 - SZ1*SY2
                    SY3  = SZ1*SX2 - SX1*SZ2
                    SZ3  = SX1*SY2 - SY1*SX2
                    AREA(I) = AREA(I)
     .                   + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                    IELEC(I) = IXC(1,IE)
                    MG = IXC(6,IE)
                  ENDIF
                  IF(IE1(J,2)==3) THEN
                    SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                    SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                    SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                    SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                    SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                    SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                    SX3  = SY1*SZ2 - SZ1*SY2
                    SY3  = SZ1*SX2 - SX1*SZ2
                    SZ3  = SX1*SY2 - SY1*SX2
                    AREA(I) = AREA(I)
     .                   + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                    IELEC(I) = IXTG(1,IE)
                    MG = IXTG(5,IE)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
            AREA(I) = HALF*SQRT(AREA(I))
            IF(IEC == 1) THEN
              DX=GEO(1,MG)
              AREA(I) = AREA(I)+HALF*DX
            ENDIF
          ENDDO

        ELSEIF(NELS/=0) THEN
          DO I=1,NRT
            AREA(I) = ZERO
            JJ1 = 0
            DO J=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
              JJ1 = JJ1 +1
              IE1(JJ1,1) = NOD2ELS(J)
            ENDDO
            JJ2 = 0
            DO J=KNOD2ELS(IRECT(2,I))+1,KNOD2ELS(IRECT(2,I)+1)
              JJ2 = JJ2 +1
              IE2(JJ2,1) = NOD2ELS(J)
            ENDDO
            IEC = 0
            DO J=1,JJ1
              DO K=1,JJ2
                IF(IE1(J,1)==IE2(K,1)) THEN
                  IE = IE1(J,1)
                  IEC= IEC +1
                  IELEC(I) = IXS(1,IE)
C
                  N1=IXS(2,IE)
                  N2=IXS(3,IE)
                  N3=IXS(4,IE)
                  N4=IXS(5,IE)
                  N5=IXS(6,IE)
                  N6=IXS(7,IE)
                  N7=IXS(8,IE)
                  N8=IXS(9,IE)
C
                  X1=X(1,N1)
                  Y1=X(2,N1)
                  Z1=X(3,N1)
                  X2=X(1,N2)
                  Y2=X(2,N2)
                  Z2=X(3,N2)
                  X3=X(1,N3)
                  Y3=X(2,N3)
                  Z3=X(3,N3)
                  X4=X(1,N4)
                  Y4=X(2,N4)
                  Z4=X(3,N4)
                  X5=X(1,N5)
                  Y5=X(2,N5)
                  Z5=X(3,N5)
                  X6=X(1,N6)
                  Y6=X(2,N6)
                  Z6=X(3,N6)
                  X7=X(1,N7)
                  Y7=X(2,N7)
                  Z7=X(3,N7)
                  X8=X(1,N8)
                  Y8=X(2,N8)
                  Z8=X(3,N8)
C----
c           face 1234
                  XX1(1)=X1
                  XX2(1)=Y1
                  XX3(1)=Z1
                  XX1(2)=X2
                  XX2(2)=Y2
                  XX3(2)=Z2
                  XX1(3)=X3
                  XX2(3)=Y3
                  XX3(3)=Z3
                  XX1(4)=X4
                  XX2(4)=Y4
                  XX3(4)=Z4
                  CALL NORMA1(I1,I2,I3,FACE(1))
                  IF(    N4/=N3
     .               .AND.N3/=N2
     .               .AND.N2/=N1
     .               .AND.N1/=N4)THEN
                    FACE(1) =FOURTH*FACE(1)
                  ELSE
                    FACE(1) =THIRD*FACE(1)
                  ENDIF
c           face 5678
                  XX1(1)=X5
                  XX2(1)=Y5
                  XX3(1)=Z5
                  XX1(2)=X6
                  XX2(2)=Y6
                  XX3(2)=Z6
                  XX1(3)=X7
                  XX2(3)=Y7
                  XX3(3)=Z7
                  XX1(4)=X8
                  XX2(4)=Y8
                  XX3(4)=Z8
                  CALL NORMA1(I1,I2,I3,FACE(2))
                  IF(    N8/=N7
     .               .AND.N7/=N6
     .               .AND.N6/=N5
     .               .AND.N5/=N8)THEN
                    FACE(2) =FOURTH*FACE(2)
                  ELSE
                    FACE(2) =THIRD*FACE(2)
                  ENDIF
c           face 2376
                  XX1(1)=X2
                  XX2(1)=Y2
                  XX3(1)=Z2
                  XX1(2)=X3
                  XX2(2)=Y3
                  XX3(2)=Z3
                  XX1(3)=X7
                  XX2(3)=Y7
                  XX3(3)=Z7
                  XX1(4)=X6
                  XX2(4)=Y6
                  XX3(4)=Z6
                  CALL NORMA1(I1,I2,I3,FACE(3))
                  IF(    N6/=N7
     .               .AND.N7/=N3
     .               .AND.N3/=N2
     .               .AND.N2/=N6)THEN
                    FACE(3) =FOURTH*FACE(3)
                  ELSE
                    FACE(3) =THIRD*FACE(3)
                  ENDIF
c           face 1485
                  XX1(1)=X1
                  XX2(1)=Y1
                  XX3(1)=Z1
                  XX1(2)=X4
                  XX2(2)=Y4
                  XX3(2)=Z4
                  XX1(3)=X8
                  XX2(3)=Y8
                  XX3(3)=Z8
                  XX1(4)=X5
                  XX2(4)=Y5
                  XX3(4)=Z5
                  CALL NORMA1(I1,I2,I3,FACE(4))
                  IF(    N5/=N8
     .               .AND.N8/=N4
     .               .AND.N4/=N1
     .               .AND.N1/=N5)THEN
                    FACE(4) =FOURTH*FACE(4)
                  ELSE
                    FACE(4) =THIRD*FACE(4)
                  ENDIF
c           face 1265
                  XX1(1)=X1
                  XX2(1)=Y1
                  XX3(1)=Z1
                  XX1(2)=X2
                  XX2(2)=Y2
                  XX3(2)=Z2
                  XX1(3)=X6
                  XX2(3)=Y6
                  XX3(3)=Z6
                  XX1(4)=X5
                  XX2(4)=Y5
                  XX3(4)=Z5
                  CALL NORMA1(I1,I2,I3,FACE(5))
                  IF(    N5/=N6
     .               .AND.N6/=N2
     .               .AND.N2/=N1
     .               .AND.N1/=N5)THEN
                    FACE(5) =FOURTH*FACE(5)
                  ELSE
                    FACE(5) =THIRD*FACE(5)
                  ENDIF
c           face 4378
                  XX1(1)=X4
                  XX2(1)=Y4
                  XX3(1)=Z4
                  XX1(2)=X3
                  XX2(2)=Y3
                  XX3(2)=Z3
                  XX1(3)=X7
                  XX2(3)=Y7
                  XX3(3)=Z7
                  XX1(4)=X8
                  XX2(4)=Y8
                  XX3(4)=Z8
                  CALL NORMA1(I1,I2,I3,FACE(6))
                  IF(    N8/=N7
     .               .AND.N7/=N3
     .               .AND.N3/=N4
     .               .AND.N4/=N8)THEN
                    FACE(6) =FOURTH*FACE(6)
                  ELSE
                    FACE(6) =THIRD*FACE(6)
                  ENDIF
C----
                  DO K1=1,8
                    N1 = IXS(K1+1,IE)
                    IF (N1 == IRECT(1,I).AND.N2 == IRECT(2,I)) THEN
                      DO K2=1,8
                        N2 = IXS(K2+1,IE)
                        IF (K1 == 1 .AND. K2 == 2) THEN
                          AREA(I) = AREA(I) + (FACE(1)+FACE(5))
                        ELSEIF (K1 == 1 .AND. K2 == 4) THEN
                          AREA(I) = AREA(I) + (FACE(1)+FACE(4))
                        ELSEIF (K1 == 1 .AND. K2 == 5) THEN
                          AREA(I) = AREA(I) + (FACE(4)+FACE(5))
                        ELSEIF  (K1 == 2 .AND. K2 == 3) THEN
                          AREA(I) = AREA(I) + (FACE(1)+FACE(3))
                        ELSEIF  (K1 == 2 .AND. K2 == 6) THEN
                          AREA(I) = AREA(I) + (FACE(5)+FACE(3))
                        ELSEIF  (K1 == 3 .AND. K2 == 4) THEN
                          AREA(I) = AREA(I) + (FACE(1)+FACE(6))
                        ELSEIF  (K1 == 3 .AND. K2 == 7) THEN
                          AREA(I) = AREA(I) + (FACE(3)+FACE(6))
                        ELSEIF  (K1 == 4 .AND. K2 == 1) THEN
                          AREA(I) = AREA(I) + (FACE(1)+FACE(4))
                        ELSEIF  (K1 == 4 .AND. K2 == 8) THEN
                          AREA(I) = AREA(I) + (FACE(6)+FACE(4))
                        ELSEIF  (K1 == 5 .AND. K2 == 6) THEN
                          AREA(I) = AREA(I) + (FACE(2)+FACE(5))
                        ELSEIF  (K1 == 6 .AND. K2 == 7) THEN
                          AREA(I) = AREA(I) + (FACE(2)+FACE(5))
                        ELSEIF  (K1 == 7 .AND. K2 == 8) THEN
                          AREA(I) = AREA(I) + (FACE(2)+FACE(6))
                        ELSEIF  (K1 == 8.AND. K2 == 5) THEN
                          AREA(I) = AREA(I) + (FACE(2)+FACE(4))
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
C
          ENDDO
        ELSEIF(NELP/=0) THEN
C
          DO I=1,NRT
            AREA(I) = ZERO
            JJ1 = 0
            DO J=KNOD2EL1D(IRECT(1,I))+1,KNOD2EL1D(IRECT(1,I)+1)
              IE = NOD2EL1D(J)
              JJ1 = JJ1+1
              MG = IXP(5,IE)
              DX = SQRT(GEO(1,MG))
              AREA(I) = AREA(I)+DX
              IELEC(I) = IXP(1,IE)
            ENDDO
            DO J=KNOD2EL1D(IRECT(2,I))+1,KNOD2EL1D(IRECT(2,I)+1)
              IE = NOD2EL1D(J)
              JJ1 = JJ1+1
              MG = IXP(5,IE)
              DX = SQRT(GEO(1,MG))
              AREA(I) = AREA(I)+DX
            ENDDO
            AREA(I) = SQRT(AREA(I)/JJ1)
          ENDDO
        ELSEIF(NELT/=0) THEN
          DO I=1,NRT
            AREA(I) = ZERO
            JJ1 = 0
            DO J=KNOD2EL1D(IRECT(1,I))+1,KNOD2EL1D(IRECT(1,I)+1)
              IE = NOD2EL1D(J)
              JJ1 = JJ1+1
              MG=IXT(4,IE)
              DX=SQRT(GEO(1,MG))
              AREA(I) = AREA(I)+DX
              IELEC(I) = IXT(1,IE)
            ENDDO
            DO J=KNOD2EL1D(IRECT(2,I))+1,KNOD2EL1D(IRECT(2,I)+1)
              IE = NOD2EL1D(J)
              JJ1 = JJ1+1
              MG = IXT(4,IE)
              DX = SQRT(GEO(1,MG))
              AREA(I) = AREA(I)+DX
            ENDDO
            AREA(I) = SQRT(AREA(I)/JJ1)
          ENDDO
        ENDIF
C
      ENDIF
c       IF(NINTR < 0 .AND. IGAP /= 0) THEN
c         DO I = 1, NRT
c           BGAPSMX = MAX(BGAPSMX,GAP_SM(I))
c         ENDDO
c       ENDIF
C----------------------------------

      RETURN

 1300 FORMAT(2X,'COMPUTED GAP = ',1PG20.13)
 2001 FORMAT(2X,'Maximum distance for radiation computation = ',
     .                                                    1PG20.13)

      END
