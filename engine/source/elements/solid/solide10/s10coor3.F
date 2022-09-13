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
Chd|  S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10COOR3(
     1   X    ,IXS  ,IXS10,V   ,W    ,
     2   XX   ,YY   ,ZZ   ,VX  ,VY   , 
     3   VZ   ,VDXX ,VDYY ,VDZZ,VDX  , 
     4   VDY  ,VDZ  ,VD2  ,VIS ,OFFG ,
     5   OFF  ,SAV  ,NC   ,NGL ,MXT  ,
     6   NGEO ,FX   ,FY   ,FZ  ,STIG ,
     7   SIGG ,EINTG,RHOG ,QG  ,EPLASM,
     8   EPSDG,VR   ,DR   ,D   ,WXXG ,
     9   WYYG,WZZG  ,G_PLA,XDP ,NEL  ,
     A   CONDEG)
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,NEL
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
      MXT_1 = IXS(1,LFT)

      VIS(LFT:LLT)=ZERO
      VD2(LFT:LLT)=ZERO
      NGEO(LFT:LLT)=IXS(10,LFT:LLT)
      NGL(LFT:LLT) =IXS(11,LFT:LLT)
      MXT(LFT:LLT) =MXT_1
      NC(LFT:LLT,1)=IXS(2,LFT:LLT)
      NC(LFT:LLT,2)=IXS(4,LFT:LLT)
      NC(LFT:LLT,3)=IXS(7,LFT:LLT)
      NC(LFT:LLT,4)=IXS(6,LFT:LLT)
      EINTG(LFT:LLT)=ZERO
      RHOG(LFT:LLT)=ZERO
      QG(LFT:LLT)=ZERO
      SIGG(LFT:LLT,1)=ZERO
      SIGG(LFT:LLT,2)=ZERO
      SIGG(LFT:LLT,3)=ZERO
      SIGG(LFT:LLT,4)=ZERO
      SIGG(LFT:LLT,5)=ZERO
      SIGG(LFT:LLT,6)=ZERO
      STIG(LFT:LLT)=ZERO
      CONDEG(LFT:LLT)=ZERO
      
      IF (ISRAT /= 0) THEN
          EPSDG(LFT:LLT)=ZERO
      ENDIF
      IF (G_PLA > 0) THEN
          EPLASM(LFT:LLT)=ZERO
      ENDIF

      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3).OR.ISMSTR==11)THEN
          WXXG(LFT:LLT)=ZERO
          WYYG(LFT:LLT)=ZERO
          WZZG(LFT:LLT)=ZERO
      END IF

      IF(ISROT /= 1)THEN
        DO I=LFT,LLT
          NC(I,5) =IXS10(1,I)
          NC(I,6) =IXS10(2,I)
          NC(I,7) =IXS10(3,I)
          NC(I,8) =IXS10(4,I)
          NC(I,9) =IXS10(5,I)
          NC(I,10)=IXS10(6,I)
        ENDDO
      ELSE
          NC(LFT:LLT,5) =0
          NC(LFT:LLT,6) =0
          NC(LFT:LLT,7) =0
          NC(LFT:LLT,8) =0
          NC(LFT:LLT,9) =0
          NC(LFT:LLT,10)=0
      ENDIF
C
      IF (JLAG==0)THEN
          VDX(LFT:LLT)=ZERO
          VDY(LFT:LLT)=ZERO
          VDZ(LFT:LLT)=ZERO  
      ENDIF
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,4
        IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C--
           DO I=LFT,LLT
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
          DO I=LFT,LLT
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
           DO I=LFT,LLT
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
           DO I=LFT,LLT
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
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
c          DO I=LFT,LLT
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
        DO I=LFT,LLT
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
        DO I=LFT,LLT
            NN = NC(I,N)
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)  
        ENDDO
       ENDDO
       DO N=5,10
        NN = 1
        DO I=LFT,LLT
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)  
        ENDDO
       ENDDO
      ENDIF

      DO N=1,10
        DO I=LFT,LLT
            FX(I,N)=ZERO
            FY(I,N)=ZERO
            FZ(I,N)=ZERO    
        ENDDO
        IF(OFF_L<ZERO)THEN
          DO I=LFT,LLT
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
            DO I=LFT,LLT
              NN = MAX(IUN,NC(I,N))
              VDXX(I,N)=VX(I,N)-W(1,NN)
              VDYY(I,N)=VY(I,N)-W(2,NN)
              VDZZ(I,N)=VZ(I,N)-W(3,NN)
            ENDDO
          ELSEIF(JEUL/=0)THEN
            DO I=LFT,LLT
              VDXX(I,N)=VX(I,N)
              VDYY(I,N)=VY(I,N)
              VDZZ(I,N)=VZ(I,N)
            ENDDO
          ENDIF
C
          DO I=LFT,LLT
            VDX(I)=VDX(I)+VDXX(I,N)
            VDY(I)=VDY(I)+VDYY(I,N)
            VDZ(I)=VDZ(I)+VDZZ(I,N)
          ENDDO
        ENDIF
      ENDDO
C
      IF (JLAG==0)THEN
        DO I=LFT,LLT
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
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
c          DO I=LFT,LLT
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
Chd|====================================================================
Chd|  S10SAV3                       source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10SAV3(OFFG ,SAV  ,NC   ,
     +                   XX   ,YY   ,ZZ   ,NEL)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,10),NEL
      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),SAV(NEL,30)
     
C     REAL
      my_real
     .  OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,NN
C     REAL
C-----------------------------------------------
       DO N=1,10
           DO I=LFT,LLT
            NN = NC(I,N)
            IF(ABS(OFFG(I))<=ONE)THEN
              SAV(I,N)   =XX(I,N)
              SAV(I,N+10)=YY(I,N)
              SAV(I,N+20)=ZZ(I,N)
            ENDIF
           ENDDO
       END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SGCOOR10                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGCOOR10(XX,YY,ZZ,X  ,XDP   ,
     1   X0, Y0, Z0, VX0,VY0, VZ0, 
     2   SAV,NC,D ,OFF,OFF0,NEL)
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
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,10),NEL
C     REAL
      my_real
     .  VX0(MVSIZ,10),VY0(MVSIZ,10),VZ0(MVSIZ,10),
     .  D(3,*) ,OFF(*),OFF0(*),X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),XDP(3,*),
     .   X0(MVSIZ,10), Y0(MVSIZ,10), Z0(MVSIZ,10),SAV(NEL,30)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE,N,N2,N3
C     REAL
C-----------------------------------------------
C----------------------------
C     NODAL COORDINATES INITIALES    |
C----------------------------
      NPE = 10
      DO N=1,NPE
       DO I=LFT,LLT
        X0(I,N)=SAV(I,N)
        Y0(I,N)=SAV(I,N+NPE)
        Z0(I,N)=SAV(I,N+2*NPE)
       ENDDO
      ENDDO 
C----------------------------
C     DISPLACEMENT     |
C----------------------------
        N = NPE
        DO I=LFT,LLT
        VX0(I,N)=XX(I,N)-X0(I,N)
        VY0(I,N)=YY(I,N)-Y0(I,N)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)
        ENDDO
       DO N=1,NPE
        DO I=LFT,LLT
        VX0(I,N)=XX(I,N)-X0(I,N)-VX0(I,NPE)
        VY0(I,N)=YY(I,N)-Y0(I,N)-VY0(I,NPE)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)-VZ0(I,NPE)
        ENDDO
       ENDDO 
C---   w/ /DT/BRICK/CST    
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        DO I=LFT,LLT
         OFF0(I)=OFF(I)
        ENDDO
       IF (MTN==1) THEN
         IF(IRESP==1)THEN
          DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            N3 = NC(I,NPE)
            VX0(I,NPE)=XDP(1,N3)-X0(I,NPE)
            VY0(I,NPE)=XDP(2,N3)-Y0(I,NPE)
            VZ0(I,NPE)=XDP(3,N3)-Z0(I,NPE)
          ENDDO 
          DO N=1,NPE
           DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            N2 = NC(I,N)
            VX0(I,N) =XDP(1,N2)-X0(I,N)-VX0(I,NPE)
            VY0(I,N) =XDP(2,N2)-Y0(I,N)-VY0(I,NPE)
            VZ0(I,N) =XDP(3,N2)-Z0(I,N)-VZ0(I,NPE)
           ENDDO
          ENDDO 
         ELSE      
          DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            N3 = NC(I,NPE)
            VX0(I,NPE)=X(1,N3)-X0(I,NPE)
            VY0(I,NPE)=X(2,N3)-Y0(I,NPE)
            VZ0(I,NPE)=X(3,N3)-Z0(I,NPE)
          ENDDO 
          DO N=1,NPE
           DO I=LFT,LLT
            IF (OFF(I) <= ONE ) CYCLE 
            N2 = NC(I,N)
            VX0(I,N) =X(1,N2)-X0(I,N)-VX0(I,NPE)
            VY0(I,N) =X(2,N2)-Y0(I,N)-VY0(I,NPE)
            VZ0(I,N) =X(3,N2)-Z0(I,N)-VZ0(I,NPE)
           ENDDO
          ENDDO 
         END IF !(IRESP==1)THEN
       ELSE
         DO N=1,NPE
          DO I=LFT,LLT
           IF (OFF(I) <= ONE ) CYCLE 
            VX0(I,N)=D(1,NC(I,N))-D(1,NC(I,NPE))  
            VY0(I,N)=D(2,NC(I,N))-D(2,NC(I,NPE))
            VZ0(I,N)=D(3,NC(I,N))-D(3,NC(I,NPE))
          ENDDO
         ENDDO
       END IF !(MTN==1) THEN
      END IF !(ISMSTR == 10.AND.IDTMIN(1)==3) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  S10RCOOR12                    source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        SMORTHO3                      source/elements/solid/solide4/s4coor3.F
Chd|====================================================================
      SUBROUTINE S10RCOOR12(OFF,X ,XDP ,NC ,
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ) 
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,10)
     
      DOUBLE PRECISION
     .  XDP(3,*)
C     REAL
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),OFF(*),
     .   X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,4), YY(MVSIZ,4), ZZ(MVSIZ,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N ,NN   
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C=======================================================================
         DO N=1,4
          DO I=LFT,LLT
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
          END DO 
         END DO 
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=LFT,LLT
        IF (OFF(I) <= ONE ) CYCLE 
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
      CALL SMORTHO3(OFF,
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S10RROTA3                     source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10RROTA3(OFF,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   XX, YY, ZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),OFF(*),
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
      my_real
     .   X,Y,Z
C-----------------------------------------------
C   X=RX' <=> X'=t(R)X chgt de base.
C-  to do X'=RtX, just input Rt instead of R
      DO I=LFT,LLT
       DO N =1,10
       IF (OFF(I) <= ONE) CYCLE
       X=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
       Y=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
       Z=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
       XX(I,N)=X
       YY(I,N)=Y
       ZZ(I,N)=Z
       END DO !I=LFT,LLT
      END DO !I=LFT,LLT
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  S10RCOOR10                    source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|====================================================================
      SUBROUTINE S10RCOOR10(
     .   XX    ,YY    ,ZZ    , 
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ) 
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C=======================================================================
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=LFT,LLT
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
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S10RROTA10                    source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        SR10COOR3                     source/elements/solid/solide10/s10coor3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10RROTA10(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   XX, YY, ZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
      my_real
     .   X,Y,Z
C-----------------------------------------------
C   X=RX' <=> X'=t(R)X chgt de base.
C-  to do X'=RtX, just input Rt instead of R
      DO I=LFT,LLT
       DO N =1,10
       X=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
       Y=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
       Z=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
       XX(I,N)=X
       YY(I,N)=Y
       ZZ(I,N)=Z
       END DO !I=LFT,LLT
      END DO !I=LFT,LLT
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
Chd|====================================================================
Chd|  SR10COOR3                     source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S10RROTA10                    source/elements/solid/solide10/s10coor3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|====================================================================
      SUBROUTINE SR10COOR3(
     1   X    ,IXS  ,IXS10,V   ,W    ,
     2   XX   ,YY   ,ZZ   ,VX  ,VY   , 
     3   VZ   ,VDXX ,VDYY ,VDZZ,VDX  , 
     4   VDY  ,VDZ  ,VD2  ,VIS ,OFFG ,
     5   OFF  ,SAV  ,NC   ,NGL ,MXT  ,
     6   NGEO ,FX   ,FY   ,FZ  ,STIG ,
     7   SIGG ,EINTG,RHOG ,QG  ,EPLASM,
     8   EPSDG,VR   ,DR   ,D   ,WXXG ,
     9   WYYG,WZZG  ,G_PLA,XDP ,
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ,
     .   NEL  ) 
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,NEL
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
      MXT_1 = IXS(1,LFT)
      DO I=LFT,LLT
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
      
      IF (ISRAT /= 0) THEN
        DO I=LFT,LLT
          EPSDG(I)=ZERO
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN
        DO I=LFT,LLT
          EPLASM(I)=ZERO
        ENDDO
      ENDIF

      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
        DO I=LFT,LLT
          WXXG(I)=ZERO
          WYYG(I)=ZERO
          WZZG(I)=ZERO
        ENDDO
      END IF

      IF(ISROT /= 1)THEN
        DO I=LFT,LLT
          NC(I,5) =IXS10(1,I)
          NC(I,6) =IXS10(2,I)
          NC(I,7) =IXS10(3,I)
          NC(I,8) =IXS10(4,I)
          NC(I,9) =IXS10(5,I)
          NC(I,10)=IXS10(6,I)
        ENDDO
      ELSE
        DO I=LFT,LLT
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
        DO I=LFT,LLT
          VDX(I)=ZERO
          VDY(I)=ZERO
          VDZ(I)=ZERO  
        ENDDO
      ENDIF
      DO N=1,4
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
      DO I=LFT,LLT
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
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33)
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,4
        IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C--
           DO I=LFT,LLT
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
          DO I=LFT,LLT
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
           DO I=LFT,LLT
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
           DO I=LFT,LLT
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
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
        DO I=LFT,LLT
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
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33  ,
     .   VX, VY, VZ)
      DO N=1,10
        IF(OFF_L<ZERO)THEN
          DO I=LFT,LLT
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
            DO I=LFT,LLT
              NN = MAX(IUN,NC(I,N))
              VDXX(I,N)=VX(I,N)-W(1,NN)
              VDYY(I,N)=VY(I,N)-W(2,NN)
              VDZZ(I,N)=VZ(I,N)-W(3,NN)
            ENDDO
          ELSEIF(JEUL/=0)THEN
            DO I=LFT,LLT
              VDXX(I,N)=VX(I,N)
              VDYY(I,N)=VY(I,N)
              VDZZ(I,N)=VZ(I,N)
            ENDDO
          ENDIF
C
          DO I=LFT,LLT
            VDX(I)=VDX(I)+VDXX(I,N)
            VDY(I)=VDY(I)+VDYY(I,N)
            VDZ(I)=VDZ(I)+VDZZ(I,N)
          ENDDO
        ENDIF
      ENDDO
C
      IF (JLAG==0)THEN
        DO I=LFT,LLT
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
          DO I=LFT,LLT
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
          DO I=LFT,LLT
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
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S10LOC_S                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|====================================================================
      SUBROUTINE S10LOC_S(NC, X,
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ) 
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
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER NC(MVSIZ,10)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NN
      
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C=======================================================================
      DO N=1,4
          DO I=LFT,LLT
            NN = NC(I,N)           
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)			 
          ENDDO
      END DO
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=LFT,LLT
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
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S10SAV12                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10SAV12(OFFG ,OFFG0,SAV  ,NC   ,
     +                   XX   ,YY   ,ZZ   ,NEL)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,10),NEL
      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),SAV(NEL,30)
     
C     REAL
      my_real
     .  OFFG(*),OFFG0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,NN
C     REAL
C-----------------------------------------------
       DO N=1,10
           DO I=LFT,LLT
            NN = NC(I,N)
            IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I))>ONE)THEN
              SAV(I,N)   =XX(I,N)
              SAV(I,N+10)=YY(I,N)
              SAV(I,N+20)=ZZ(I,N)
            ENDIF
           ENDDO
       END DO
C-----------
      RETURN
      END
