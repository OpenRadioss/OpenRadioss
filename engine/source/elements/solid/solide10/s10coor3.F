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
Chd|  S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10COOR3(
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
     C   NEL,     CONDEG,  G_EPSD,  JALE,
     D   ISMSTR,  JEUL,    JLAG,    ISRAT,
     E   ISROT)
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
     .  XDP(3,*),XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),SAV(NEL,30)
     
C     REAL
      my_real
     .  X(3,*),V(3,*),W(3,*), VIS(*),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  VDXX(MVSIZ,10), VDYY(MVSIZ,10), VDZZ(MVSIZ,10),
     .  VDX(*), VDY(*), VDZ(*),VD2(*),OFFG(*),OFF(*),
     .  FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10),EPSDG(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),STIG(*),EPLASM(*),
     .  VR(3,*),DR(3,*),D(3,*), 
     .  WXXG(MVSIZ),WYYG(MVSIZ),WZZG(MVSIZ),CONDEG(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN,MXT_1
C     REAL
      my_real
     .   OFF_L,DVX,DVY,DVZ,DX,DY,DZ
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
      IUN=1
      OFF_L  = ZERO
C
      MXT_1 = IXS(1,1)

      VIS(1:NEL)=ZERO
      VD2(1:NEL)=ZERO
      NGEO(1:NEL)=IXS(10,1:NEL)
      NGL(1:NEL) =IXS(11,1:NEL)
      MXT(1:NEL) =MXT_1
      NC(1:NEL,1)=IXS(2,1:NEL)
      NC(1:NEL,2)=IXS(4,1:NEL)
      NC(1:NEL,3)=IXS(7,1:NEL)
      NC(1:NEL,4)=IXS(6,1:NEL)
      EINTG(1:NEL)=ZERO
      RHOG(1:NEL)=ZERO
      QG(1:NEL)=ZERO
      SIGG(1:NEL,1)=ZERO
      SIGG(1:NEL,2)=ZERO
      SIGG(1:NEL,3)=ZERO
      SIGG(1:NEL,4)=ZERO
      SIGG(1:NEL,5)=ZERO
      SIGG(1:NEL,6)=ZERO
      STIG(1:NEL)=ZERO
      CONDEG(1:NEL)=ZERO
      
      IF ((ISRAT /= 0).OR.(G_EPSD > 0)) THEN
          EPSDG(1:NEL)=ZERO
      ENDIF
      IF (G_PLA > 0) THEN
          EPLASM(1:NEL)=ZERO
      ENDIF

      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3).OR.ISMSTR==11)THEN
          WXXG(1:NEL)=ZERO
          WYYG(1:NEL)=ZERO
          WZZG(1:NEL)=ZERO
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
          NC(1:NEL,5) =0
          NC(1:NEL,6) =0
          NC(1:NEL,7) =0
          NC(1:NEL,8) =0
          NC(1:NEL,9) =0
          NC(1:NEL,10)=0
      ENDIF
C
      IF (JLAG==0)THEN
          VDX(1:NEL)=ZERO
          VDY(1:NEL)=ZERO
          VDZ(1:NEL)=ZERO  
      ENDIF
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,4
        IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C--
           DO I=1,NEL
            NN = NC(I,N)
            IF(ABS(OFFG(I))>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+10)
              ZZ(I,N)=SAV(I,N+20)
              OFF(I) = ABS(OFFG(I))-ONE
              OFF_L  = MIN(OFF_L,OFFG(I))
            ELSE
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
C              SAV(I,N)=XX(I,N)
C              SAV(I,N+10)=YY(I,N)
C              SAV(I,N+20)=ZZ(I,N)
              OFF(I) = ABS(OFFG(I))
              OFF_L  = MIN(OFF_L,OFFG(I))
            ENDIF
           ENDDO
        ELSE
C--
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
            OFF(I) = MIN(ONE,ABS(OFFG(I)))
            OFF_L  = MIN(OFF_L,OFFG(I))
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
              OFF_L  = MIN(OFF_L,OFFG(I))
            ELSE
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
C             SAV(I,N)=XX(I,N)
C             SAV(I,N+10)=YY(I,N)
C             SAV(I,N+20)=ZZ(I,N)
              OFF(I) = ABS(OFFG(I))
              OFF_L  = MIN(OFF_L,OFFG(I))
            ENDIF
           ENDDO
          ELSEIF(ISROT==1)THEN
c
C--        
           DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+10)
              ZZ(I,N)=SAV(I,N+20)
              OFF(I) = ABS(OFFG(I))-ONE
              OFF_L  = MIN(OFF_L,OFFG(I))
            ELSE
              N1=IPERM1(N)
              N2=IPERM2(N)
c                XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
c                YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
c                ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
              DX = (YY(I,N2)-YY(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
     .           - (ZZ(I,N2)-ZZ(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
              DY = (ZZ(I,N2)-ZZ(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))
     .           - (XX(I,N2)-XX(I,N1))*(DR(3,NC(I,N2))-DR(3,NC(I,N1)))
              DZ = (XX(I,N2)-XX(I,N1))*(DR(2,NC(I,N2))-DR(2,NC(I,N1)))
     .           - (YY(I,N2)-YY(I,N1))*(DR(1,NC(I,N2))-DR(1,NC(I,N1)))

              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2)) + ONE_OVER_8 * DX
              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2)) + ONE_OVER_8 * DY
              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2)) + ONE_OVER_8 * DZ
C             SAV(I,N)=XX(I,N)
C             SAV(I,N+10)=YY(I,N)
C             SAV(I,N+20)=ZZ(I,N)
c              SAV(I,N)= HALF*(XX(I,N1)+XX(I,N2))
c              SAV(I,N+10)= HALF*(YY(I,N1)+YY(I,N2))
c              SAV(I,N+20)= HALF*(ZZ(I,N1)+ZZ(I,N2))
              OFF(I) = ABS(OFFG(I))
              OFF_L  = MIN(OFF_L,OFFG(I))
            ENDIF
           ENDDO
          END IF
C
        ELSEIF(ISROT==0.OR.ISROT==2)THEN
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
            OFF(I) = MIN(ONE,ABS(OFFG(I)))
            OFF_L  = MIN(OFF_L,OFFG(I))
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
            OFF(I) = MIN(ONE,ABS(OFFG(I)))
            OFF_L  = MIN(OFF_L,OFFG(I))
          ENDDO
C
c        ELSEIF(ISROT==2)THEN
C
c          DO I=1,NEL
c            NN = NC(I,N)
c            N1=IPERM1(N)
c            N2=IPERM2(N)
c            IF(NN==0)THEN
c              XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
c              YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
c              ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
c            ELSE
c --------------will be done in resol	    
c              XX(I,N) = D(1,NC(I,N)) + HALF*(XX(I,N1)+XX(I,N2))
c              YY(I,N) = D(2,NC(I,N)) + HALF*(YY(I,N1)+YY(I,N2))
c              ZZ(I,N) = D(3,NC(I,N)) + HALF*(ZZ(I,N1)+ZZ(I,N2))
c             en entr   X est faux, il manque 1/2(V1+V2)*dt
c              X(1,NC(I,N)) = XX(I,N)
c              X(2,NC(I,N)) = YY(I,N)
c              X(3,NC(I,N)) = ZZ(I,N)
c	      IF(IRESP==1)THEN
c                XDP(1,NC(I,N)) = XX(I,N)
c                XDP(2,NC(I,N)) = YY(I,N)
c                XDP(3,NC(I,N)) = ZZ(I,N)	        
c              ENDIF
c            END IF
c            OFF(I) = MIN(ONE,ABS(OFFG(I)))
c            OFF_L  = MIN(OFF_L,OFFG(I))
c          ENDDO
C
        ENDIF
      END DO
C
      ! Initialization of VX, VY and VZ tables
      VX(1:MVSIZ,1:10) = ZERO
      VY(1:MVSIZ,1:10) = ZERO
      VZ(1:MVSIZ,1:10) = ZERO
      IF(ISROT/=1) THEN
       DO N=1,10
        DO I=1,NEL
            NN = NC(I,N)
            ! Add a test on NN to avoid check bounds issue when NN = 0 (degenerated tetra)
            IF (NN /= 0) THEN
              VX(I,N)=V(1,NN)
              VY(I,N)=V(2,NN)
              VZ(I,N)=V(3,NN)  
            ENDIF
        ENDDO
       ENDDO
      ELSE 
       DO N=1,4
        DO I=1,NEL
            NN = NC(I,N)
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)  
        ENDDO
       ENDDO
       DO N=5,10
        NN = 1
        DO I=1,NEL
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)  
        ENDDO
       ENDDO
      ENDIF

      DO N=1,10
        DO I=1,NEL
            FX(I,N)=ZERO
            FY(I,N)=ZERO
            FZ(I,N)=ZERO    
        ENDDO
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
      IF(ISROT == 0.OR.ISROT == 2)THEN
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
     .         	- (ZZ(I,N2)-ZZ(I,N1))*(VR(2,NC(I,N2))-VR(2,NC(I,N1)))
	           DVY = (ZZ(I,N2)-ZZ(I,N1))*(VR(1,NC(I,N2))-VR(1,NC(I,N1)))
     .         	- (XX(I,N2)-XX(I,N1))*(VR(3,NC(I,N2))-VR(3,NC(I,N1)))
	           DVZ = (XX(I,N2)-XX(I,N1))*(VR(2,NC(I,N2))-VR(2,NC(I,N1)))
     .         	- (YY(I,N2)-YY(I,N1))*(VR(1,NC(I,N2))-VR(1,NC(I,N1)))
            VX(I,N) = HALF*(VX(I,N1)+VX(I,N2)) + ONE_OVER_8 * DVX
            VY(I,N) = HALF*(VY(I,N1)+VY(I,N2)) + ONE_OVER_8 * DVY
            VZ(I,N) = HALF*(VZ(I,N1)+VZ(I,N2)) + ONE_OVER_8 * DVZ   
          ENDDO
        ENDDO
c      ELSE IF(ISROT == 2)THEN
c        DO N=5,10
c          N1=IPERM1(N)
c          N2=IPERM2(N)
c          DO I=1,NEL
c            IF(NC(I,N) == 0)THEN
c              VX(I,N) = HALF*(VX(I,N1)+VX(I,N2))
c              VY(I,N) = HALF*(VY(I,N1)+VY(I,N2))
c              VZ(I,N) = HALF*(VZ(I,N1)+VZ(I,N2)) 
c            ELSE
c              VX(I,N) = VX(I,N) + HALF*(VX(I,N1)+VX(I,N2))
c              VY(I,N) = VY(I,N) + HALF*(VY(I,N1)+VY(I,N2))
c              VZ(I,N) = VZ(I,N) + HALF*(VZ(I,N1)+VZ(I,N2))
c            ENDIF
c          ENDDO
c        ENDDO
      ENDIF
C-----------
      RETURN
      END
