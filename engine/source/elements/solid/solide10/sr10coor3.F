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
Chd|====================================================================
Chd|  SR10COOR3                     source/elements/solid/solide10/sr10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S10RROTA10                    source/elements/solid/solide10/s10rrota10.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|====================================================================
      SUBROUTINE SR10COOR3(
     1   X,       IXS,     IXS10,   V,
     2   W,       XX,      YY,      ZZ,
     3   VX,      VY,      VZ,      VDXX,
     4   VDYY,    VDZZ,    VDX,     VDY,
     5   VDZ,     VD2,     VIS,     OFFG,
     6   OFF,     SAV,     NC,      NGL,
     7   MXT,     NGEO,    FX,      FY,
     8   FZ,      STIG,    SIGG,    EINTG,
     9   RHOG,    QG,      EPLASM,  EPSDG,
     A   VR,      DR,      D,       WXXG,
     B   WYYG,    WZZG,    G_PLA,   XDP,
     C   R11,     R12,     R13,     R21,
     D   R22,     R23,     R31,     R32,
     E   R33,     NEL,     G_EPSD,  JALE,
     F   ISMSTR,  JEUL,    JLAG,    ISRAT,
     G   ISROT)
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JEUL
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: ISRAT
      INTEGER, INTENT(IN) :: ISROT
      INTEGER, INTENT(IN) :: G_PLA,NEL,G_EPSD
      INTEGER NC(MVSIZ,10), MXT(*), NGL(*), NGEO(*),
     .        IXS(NIXS,*), IXS10(6,*)
     
      DOUBLE PRECISION
     .  XDP(3,*),XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .  XDL,YDL,ZDL,SAV(NEL,30)
     
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*)
C     REAL
      my_real
     .  X(3,*),V(3,*),W(3,*), VIS(*),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  VDXX(MVSIZ,10), VDYY(MVSIZ,10), VDZZ(MVSIZ,10),
     .  VDX(*), VDY(*), VDZ(*),VD2(*),OFFG(*),OFF(*),
     .  FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10),EPSDG(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),STIG(*),EPLASM(*),
     .  VR(3,*),DR(3,*),D(3,*), 
     .  WXXG(MVSIZ),WYYG(MVSIZ),WZZG(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN,MXT_1
C     REAL
      my_real
     .   OFF_L,DVX,DVY,DVZ,DX,DY,DZ
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
      IUN=1
      OFF_L  = ZERO
C
      MXT_1 = IXS(1,1)
      DO I=1,NEL
        VIS(I)=ZERO
        VD2(I)=ZERO
        NGEO(I)=IXS(10,I)
        NGL(I) =IXS(11,I)
        MXT(I) =MXT_1
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
        EINTG(I)=ZERO
        RHOG(I)=ZERO
        QG(I)=ZERO
        SIGG(I,1)=ZERO
        SIGG(I,2)=ZERO
        SIGG(I,3)=ZERO
        SIGG(I,4)=ZERO
        SIGG(I,5)=ZERO
        SIGG(I,6)=ZERO
        STIG(I)=ZERO
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
      
      IF ((ISRAT /= 0).OR.(G_EPSD > 0)) THEN
        DO I=1,NEL
          EPSDG(I)=ZERO
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN
        DO I=1,NEL
          EPLASM(I)=ZERO
        ENDDO
      ENDIF

      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
        DO I=1,NEL
          WXXG(I)=ZERO
          WYYG(I)=ZERO
          WZZG(I)=ZERO
        ENDDO
      END IF

      IF(ISROT /= 1)THEN
        DO I=1,NEL
          NC(I,5) =IXS10(1,I)
          NC(I,6) =IXS10(2,I)
          NC(I,7) =IXS10(3,I)
          NC(I,8) =IXS10(4,I)
          NC(I,9) =IXS10(5,I)
          NC(I,10)=IXS10(6,I)
        ENDDO
      ELSE
        DO I=1,NEL
          NC(I,5) =0
          NC(I,6) =0
          NC(I,7) =0
          NC(I,8) =0
          NC(I,9) =0
          NC(I,10)=0
        ENDDO
      ENDIF
C
      IF (JLAG==0)THEN
        DO I=1,NEL
          VDX(I)=ZERO
          VDY(I)=ZERO
          VDZ(I)=ZERO  
        ENDDO
      ENDIF
      DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)           
            IF(IRESP==1)THEN
              XX(I,N)=XDP(1,NN)
              YY(I,N)=XDP(2,NN)
              ZZ(I,N)=XDP(3,NN)        
              ELSE
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)       
            ENDIF      
          ENDDO
      END DO
C
      DO N=5,10
        IF(ISROT==0.OR.ISROT==2)THEN
C
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
               IF(IRESP==1)THEN
                 XX(I,N)=XDP(1,NN)
                 YY(I,N)=XDP(2,NN)
                 ZZ(I,N)=XDP(3,NN)        
                 ELSE
                 XX(I,N)=X(1,NN)
                 YY(I,N)=X(2,NN)
                 ZZ(I,N)=X(3,NN)       
               ENDIF
            ELSE
              N1=IPERM1(N)
              N2=IPERM2(N)
              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
            END IF
          ENDDO
C
        ELSEIF(ISROT==1)THEN
C
          DO I=1,NEL
            N1=IPERM1(N)
            N2=IPERM2(N)
            DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .         - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
            DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .         - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
            DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .         - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
          ENDDO
C
        ENDIF
      END DO
C
      DO I=1,NEL
        RX(I) = XX(I,1) - XX(I,4)
        RY(I) = YY(I,1) - YY(I,4)
        RZ(I) = ZZ(I,1) - ZZ(I,4)
        SX(I) = XX(I,2) - XX(I,4)
        SY(I) = YY(I,2) - YY(I,4)
        SZ(I) = ZZ(I,2) - ZZ(I,4)
        TX(I) = XX(I,3) - XX(I,4)
        TY(I) = YY(I,3) - YY(I,4)
        TZ(I) = ZZ(I,3) - ZZ(I,4)
      END DO 
      CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,4
        IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C--
           DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+10)
              ZZ(I,N)=SAV(I,N+20)
              OFF(I) = ABS(OFFG(I))-ONE
            ELSE
             XDL=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
             YDL=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
             ZDL=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
             XX(I,N)=XDL
             YY(I,N)=YDL
             ZZ(I,N)=ZDL
            ENDIF
           ENDDO
        ELSE
C--
          DO I=1,NEL
             XDL=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
             YDL=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
             ZDL=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
             XX(I,N)=XDL
             YY(I,N)=YDL
             ZZ(I,N)=ZDL
          ENDDO
        ENDIF
      END DO
C
      DO N=5,10
        IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C
          IF(ISROT==0.OR.ISROT==2)THEN
C--
           DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+10)
              ZZ(I,N)=SAV(I,N+20)
              OFF(I) = ABS(OFFG(I))-ONE
            ELSE
              IF(NN/=0)THEN
               XDL=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
               YDL=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
               ZDL=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
               XX(I,N)=XDL
               YY(I,N)=YDL
               ZZ(I,N)=ZDL
              ELSE
                N1=IPERM1(N)
                N2=IPERM2(N)
                XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
                YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
                ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
              END IF
            ENDIF
           ENDDO
          ELSEIF(ISROT==1)THEN
c
C--        
           DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+10)
              OFF(I) = ABS(OFFG(I))-ONE
              ZZ(I,N)=SAV(I,N+20)
            ELSE
              N1=IPERM1(N)
              N2=IPERM2(N)
C-------------DR-> local first        
              DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .           - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
              DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .           - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
              DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .           - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
            ENDIF
           ENDDO
          END IF
C
        ELSEIF(ISROT==0.OR.ISROT==2)THEN
C
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
               XDL=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
               YDL=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
               ZDL=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
               XX(I,N)=XDL
               YY(I,N)=YDL
               ZZ(I,N)=ZDL
            ELSE
              N1=IPERM1(N)
              N2=IPERM2(N)
              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
            END IF
          ENDDO
C
        ELSEIF(ISROT==1)THEN
C
          DO I=1,NEL
            N1=IPERM1(N)
            N2=IPERM2(N)
            DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .         - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
            DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .         - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
            DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .         - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
          ENDDO
C
        ENDIF
      END DO
C
      DO N=1,10
        DO I=1,NEL
            NN = MAX(IUN,NC(I,N))
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)
            FX(I,N)=ZERO
            FY(I,N)=ZERO
            FZ(I,N)=ZERO    
        ENDDO
      ENDDO
      CALL S10RROTA10(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX,      VY,      VZ,
     4   NEL)
      DO N=1,10
        IF(OFF_L<ZERO)THEN
          DO I=1,NEL
            IF(OFFG(I)<ZERO)THEN
              VX(I,N)=ZERO
              VY(I,N)=ZERO
              VZ(I,N)=ZERO
            ENDIF
          ENDDO
        ENDIF
C
        IF (JLAG==0)THEN
C
          IF(JALE/=0)THEN
            DO I=1,NEL
              NN = MAX(IUN,NC(I,N))
              VDXX(I,N)=VX(I,N)-W(1,NN)
              VDYY(I,N)=VY(I,N)-W(2,NN)
              VDZZ(I,N)=VZ(I,N)-W(3,NN)
            ENDDO
          ELSEIF(JEUL/=0)THEN
            DO I=1,NEL
              VDXX(I,N)=VX(I,N)
              VDYY(I,N)=VY(I,N)
              VDZZ(I,N)=VZ(I,N)
            ENDDO
          ENDIF
C
          DO I=1,NEL
            VDX(I)=VDX(I)+VDXX(I,N)
            VDY(I)=VDY(I)+VDYY(I,N)
            VDZ(I)=VDZ(I)+VDZZ(I,N)
          ENDDO
        ENDIF
      ENDDO
C
      IF (JLAG==0)THEN
        DO I=1,NEL
          VDX(I)=FOURTH*VDX(I)
          VDY(I)=FOURTH*VDY(I)
          VDZ(I)=FOURTH*VDZ(I)  
          VD2(I)=(VDX(I)**2+VDY(I)**2+VDZ(I)**2)  
        ENDDO
      ENDIF
C
      IF(ISROT == 0.OR.ISROT==2)THEN
        DO N=5,10
          N1=IPERM1(N)
          N2=IPERM2(N)
          DO I=1,NEL
            IF(NC(I,N)==0)THEN
              VX(I,N) = HALF*(VX(I,N1)+VX(I,N2))
              VY(I,N) = HALF*(VY(I,N1)+VY(I,N2))
              VZ(I,N) = HALF*(VZ(I,N1)+VZ(I,N2))    
            ENDIF
          ENDDO
        ENDDO
      ELSEIF(ISROT == 1)THEN
        DO N=5,10
          N1=IPERM1(N)
          N2=IPERM2(N)
          DO I=1,NEL
                     DVX = (YY(I,N2)-YY(I,N1))*(VR(3,NC(I,N2))-VR(3,NC(I,N1)))
     .           - (ZZ(I,N2)-ZZ(I,N1))*(VR(2,NC(I,N2))-VR(2,NC(I,N1)))
                     DVY = (ZZ(I,N2)-ZZ(I,N1))*(VR(1,NC(I,N2))-VR(1,NC(I,N1)))
     .           - (XX(I,N2)-XX(I,N1))*(VR(3,NC(I,N2))-VR(3,NC(I,N1)))
                     DVZ = (XX(I,N2)-XX(I,N1))*(VR(2,NC(I,N2))-VR(2,NC(I,N1)))
     .           - (YY(I,N2)-YY(I,N1))*(VR(1,NC(I,N2))-VR(1,NC(I,N1)))
            VX(I,N) = HALF*(VX(I,N1)+VX(I,N2)) + ONE_OVER_8 * DVX
            VY(I,N) = HALF*(VY(I,N1)+VY(I,N2)) + ONE_OVER_8 * DVY
            VZ(I,N) = HALF*(VZ(I,N1)+VZ(I,N2)) + ONE_OVER_8 * DVZ   
          ENDDO
        ENDDO
      ENDIF
C-----------
      RETURN
      END
