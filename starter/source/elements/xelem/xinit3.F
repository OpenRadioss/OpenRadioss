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
Chd|  XINIT3                        source/elements/xelem/xinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        XINI1U                        source/elements/xelem/xinit3.F
Chd|        XINI28                        source/elements/xelem/xini28.F
Chd|        XINI29                        source/elements/xelem/xini29.F
Chd|        XINI2U                        source/elements/xelem/xinit3.F
Chd|        XINI30                        source/elements/xelem/xini30.F
Chd|        XINI31                        source/elements/xelem/xini31.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE XINIT3(ELBUF_STR,KXX    ,IXX    ,X      ,V      ,
     2                  VR       ,XMAS   ,XIN    ,
     3                  SKEW     ,DTELEM ,NEL    ,STIFN  ,STIFR  ,
     4                  PARTSAV  ,IPARTX ,GEO    ,
     5                  ITAB     ,UIX    ,XUSR   ,VUSR   ,
     6                  VRUSR    ,UMASS  ,UINER  ,USTIFM ,USTIFR ,
     7                  UVISM    ,UVISR  ,IGEO   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX,*), IXX(*), IPARTX(*),ITAB(*),
     .        NEL, UIX(*),IGEO(NPROPGI,*)
C
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(*), VR(*), XMAS(*), XIN(*),
     .   SKEW(LSKEW,*), DTELEM(*),STIFN(*),STIFR(*),PARTSAV(20,*),
     .   GEO(NPROPG,*),XUSR(3,*), VUSR(3,*),VRUSR(3,*),
     .   UMASS(*) ,UINER(*) ,USTIFM(*) ,USTIFR(*) ,UVISM(*) ,UVISR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IGTYP, NDEPAR,
     .   NB1, NB2, NB3, NB4, NB5, NBFI, I1,
     .   NUVAR, NUVARN, NUPARAM, IADBUF, NFUNC, IADFUN,
     .   NMAT,IADMAT,NJPID,IADPID,
     .   IADNOD, IMAT, IPROP, NX, K, UID,
     .   NAX1D, NAX2D, NAX3D,IPID1,KVAR,KVARN
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C     REAL ou REAL*8
      my_real
     .   DT, DTC, XK, XC, XKR, XCR, XM, XINE, A,
     .   DTE, MASSELE
C
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
C---
      NDEPAR=NUMELC+NUMELS+NUMELT+NUMELQ+NUMELP+NUMELR+NUMELTG
     .         +NFT
C-------
      IPID1=KXX(2,NFT+1)
      ID=IGEO(1,IPID1)
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
      DO I=LFT,LLT
        J=I+NFT
C
        IMAT =KXX(1,J)
        IPROP=KXX(2,J)
        NX   =KXX(3,J)
C
        IGTYP =  NINT(GEO(12,IPROP))
        NUVAR =  NINT(GEO(25,IPROP))
        NUVARN=  NINT(GEO(35,IPROP))
C
cc        NB1 =(I-1)*(3+NUVAR+NUVARN*NX)+1
cc        NB2 =NB1 +1
cc        NB3 =NB2 +1
cc        NB4 =NB3 +1
cc        NB5 =NB4 +NUVAR
cc        NBFI=NB5 +NUVARN*NX
C
        KVAR  = NUVAR*(I-1)+1
        KVARN = NUVARN*NX*(I-1)+1
C-------
        CALL XINI1U(X ,V       ,VR   ,
     2        GBUF%OFF(I) ,KXX(1,J),IXX  ,ITAB   ,NX    ,
     3        UID     ,UIX     ,XUSR ,VUSR   ,VRUSR )
C-------
        NUPARAM =  NINT(GEO(26,IPROP))
        IADBUF  =  NINT(GEO(27,IPROP))
C
        IF (IGTYP == 28) THEN
          CALL XINI28 (NX     ,NAX1D ,NAX2D ,NAX3D ,
     1         XUSR    ,VUSR ,VRUSR  ,
     3         IOUT    ,IPROP  ,IMAT   ,
     4         UIX     ,UID    ,UMASS  ,
     5         UINER   ,USTIFM ,USTIFR ,UVISM ,UVISR  ,
     6         GBUF%VAR(KVAR),NUVAR  ,GBUF%VARN(KVARN),NUVARN ,DTE  )
        ELSEIF (IGTYP == 29) THEN
          DTE=EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
          ENDDO
          NAX1D=0
          NAX2D=0
          NAX3D=0
          CALL XINI29 (NX     ,NAX1D ,NAX2D ,NAX3D ,
     1         XUSR    ,VUSR ,VRUSR  ,
     3         IOUT    ,IPROP  ,IMAT   ,
     4         UIX     ,UID    ,UMASS  ,
     5         UINER   ,USTIFM ,USTIFR ,UVISM ,UVISR  ,
     6         GBUF%VAR(KVAR),NUVAR  ,GBUF%VARN(KVARN) ,NUVARN ,DTE  )
        ELSEIF (IGTYP == 30) THEN
          DTE=EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
          ENDDO
          NAX1D=0
          NAX2D=0
          NAX3D=0
          CALL XINI30 (NX     ,NAX1D ,NAX2D ,NAX3D ,
     1         XUSR    ,VUSR ,VRUSR  ,
     3         IOUT    ,IPROP  ,IMAT   ,
     4         UIX     ,UID    ,UMASS  ,
     5         UINER   ,USTIFM ,USTIFR ,UVISM ,UVISR  ,
     6         GBUF%VAR(KVAR) ,NUVAR  ,GBUF%VARN(KVARN) ,NUVARN ,DTE  )
        ELSEIF (IGTYP == 31) THEN
          DTE=EP20
          DO K=1,NX
            UMASS(K)=ZERO
            UINER(K)=ZERO
            USTIFM(K)=ZERO
            USTIFR(K)=ZERO
            UVISM(K)=ZERO
            UVISR(K)=ZERO
          ENDDO
          NAX1D=0
          NAX2D=0
          NAX3D=0
          CALL XINI31 (NX     ,NAX1D ,NAX2D ,NAX3D ,
     1         XUSR    ,VUSR ,VRUSR  ,
     3         IOUT    ,IPROP  ,IMAT   ,
     4         UIX     ,UID    ,UMASS  ,
     5         UINER   ,USTIFM ,USTIFR ,UVISM ,UVISR  ,
     6         GBUF%VAR(KVAR) ,NUVAR  ,GBUF%VARN(KVARN) ,NUVARN ,DTE  )
        ELSE
          CALL ANCMSG(MSGID=413,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=KXX(5,K),
     .                C1='PROPERTY',
     .                I2=IGEO(1,IPROP),
     .                C2='PROPERTY',
     .                I3=IGTYP)
        ENDIF
C-------
C     MASS ELEMENT = SUM OF NODAL MASS.
        MASSELE=ZERO
        DO K=1,NX
         MASSELE=MASSELE+UMASS(K)
        ENDDO
        GBUF%MASS(I) = MASSELE
C
        NANIM1D=NANIM1D+NAX1D
        NANIM2D=NANIM2D+NAX2D
        NANIM3D=NANIM3D+NAX3D
C-------
C       CALCUL DE DT ELEMENTAIRE.
        DTELEM(NDEPAR+I)= DTE
C-------
C       NODAL TIME STEP IS ESTIMATED BASED UPON STIFFNESS ONLY (NO DAMPING).
        IADNOD=KXX(4,J)
        DO K=1,NX
         I1=IXX(IADNOD+K-1)
         STIFN(I1)=STIFN(I1)+USTIFM(K)
         STIFR(I1)=STIFR(I1)+USTIFR(K)
        ENDDO
C-------
C       UPDATE DES MASSES NODALES & VARIABLES GLOBALES TH.
        CALL XINI2U(KXX(1,J),IXX,UMASS,UINER,XMAS,
     .               XIN,PARTSAV,X,V,IPARTX(J))
      ENDDO ! DO I=LFT,LLT
C------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  XINI1U                        source/elements/xelem/xinit3.F
Chd|-- called by -----------
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XINI1U(X    ,V    ,VR    ,
     2                  OFF  ,KXX  ,IXX   ,ITAB   ,NX    ,
     3                  UID  ,UIX  ,XUSR  ,VUSR   ,VRUSR )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX),IXX(*),NX,ITAB(*),UIX(NX),UID
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),VR(3,*),OFF,
     .   XUSR(3,NX),VUSR(3,NX),VRUSR(3,NX)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, I1, IADNOD
C     REAL ou REAL*8
C-----------------------------------------------
      OFF=ONE
C
C     NX    =KXX(3)
      IADNOD=KXX(4)
      DO K=1,NX
         I1=IXX(IADNOD+K-1)
         UIX(K)   =ITAB(I1)
         XUSR(1,K)=X(1,I1)
         XUSR(2,K)=X(2,I1)
         XUSR(3,K)=X(3,I1)
         VUSR(1,K)=V(1,I1)
         VUSR(2,K)=V(2,I1)
         VUSR(3,K)=V(3,I1)
         VRUSR(1,K)=VR(1,I1)
         VRUSR(2,K)=VR(2,I1)
         VRUSR(3,K)=VR(3,I1)
      ENDDO
      UID=KXX(5)
C
      RETURN
      END
Chd|====================================================================
Chd|  XINI2U                        source/elements/xelem/xinit3.F
Chd|-- called by -----------
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XINI2U(KXX,IXX,UMASS,UINER,MS,
     .                  XIN,PARTSAV,X,V,IPART)
C----------------------------------------------
C     UPDATE DES MASSES NODALES
C----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX),IXX(*),IPART
C     REAL ou REAL*8
      my_real
     .   UMASS(*), UINER(*), MS(*), XIN(*),X(3,*),V(3,*),
     .   PARTSAV(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, IP, I1, NX, IADNOD
C     REAL ou REAL*8
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
      my_real
     .   EMS, XI
C----------------------------------------------
      NX    =KXX(3)
      IADNOD=KXX(4)
C----------------------------------------------
      DO K=1,NX
        I1 = IXX(IADNOD+K-1)
C
        EMS=UMASS(K)
        MS(I1)=MS(I1)+EMS
        XI =UINER(K)
        XIN(I1)=XIN(I1)+XI
C
        IP=IPART
        PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*X(1,I1)
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*X(2,I1)
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*X(3,I1)
        XX = X(1,I1)*X(1,I1)
        XY = X(1,I1)*X(2,I1)
        YY = X(2,I1)*X(2,I1)
        YZ = X(2,I1)*X(3,I1)
        ZZ = X(3,I1)*X(3,I1)
        ZX = X(3,I1)*X(1,I1)
        PARTSAV(5,IP) =PARTSAV(5,IP) + XI + EMS * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + XI + EMS * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + XI + EMS * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY * HALF
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ * HALF
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX * HALF
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*V(1,I1)
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*V(2,I1)
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*V(3,I1)
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
      ENDDO
C----------------------------------------------
      RETURN
      END
