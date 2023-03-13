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
Chd|  S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20COOR3(X,IXS,IXSP,V,W,IPERM1,IPERM2,NPE,
     2   XX, YY, ZZ, VX,VY, VZ,VDXX, VDYY, VDZZ, 
     3   VDX,VDY,VDZ,VD2,VIS,OFFG,OFF,SAV,
     4   NC,NGL,MXT,NGEO,FX,FY,FZ,STIG,
     5   SIGG,EINTG,RHOG,QG,EPLASM,EPSDG,G_PLA,G_EPSD,NEL,CONDEN)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,G_PLA,G_EPSD,NEL,
     .        NC(MVSIZ,NPE), MXT(*), NGL(*),NGEO(*),
     .        IXS(NIXS,*),IXSP(NPE-8,*), IPERM1(NPE),IPERM2(NPE)
C     REAL
      my_real
     .  X(3,*),V(3,*),W(3,*), VIS(*),
     .  XX(MVSIZ,NPE), YY(MVSIZ,NPE), ZZ(MVSIZ,NPE),
     .  VX(MVSIZ,NPE),VY(MVSIZ,NPE),VZ(MVSIZ,NPE),
     .  VDXX(MVSIZ,NPE), VDYY(MVSIZ,NPE), VDZZ(MVSIZ,NPE),
     .  VDX(*), VDY(*), VDZ(*),VD2(*),OFFG(*),OFF(*),
     .  FX(MVSIZ,NPE), FY(MVSIZ,NPE), FZ(MVSIZ,NPE), EPSDG(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),STIG(MVSIZ,NPE),EPLASM(*),
     .  CONDEN(MVSIZ,NPE)
      DOUBLE PRECISION 
     .   SAV(NEL,3*NPE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,N1,N2,NN,IUN,MXT_1
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L  = ZERO
      IUN=1
C
      MXT_1 = IXS(1,LFT)
      DO I=LFT,LLT
        VIS(I)=ZERO
        VD2(I)=ZERO
        NGEO(I)=IXS(10,I)
        NGL(I) =IXS(11,I)
        MXT(I) =MXT_1
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(3,I)
        NC(I,3)=IXS(4,I)
        NC(I,4)=IXS(5,I)
        NC(I,5)=IXS(6,I)
        NC(I,6)=IXS(7,I)
        NC(I,7)=IXS(8,I)
        NC(I,8)=IXS(9,I)
        EINTG(I)=ZERO
        RHOG(I)=ZERO
        QG(I)=ZERO
        SIGG(I,1)=ZERO
        SIGG(I,2)=ZERO
        SIGG(I,3)=ZERO
        SIGG(I,4)=ZERO
        SIGG(I,5)=ZERO
        SIGG(I,6)=ZERO
        STIG(I,1)=ZERO
        STIG(I,2)=ZERO
        STIG(I,3)=ZERO
        STIG(I,4)=ZERO
        STIG(I,5)=ZERO
        STIG(I,6)=ZERO
        STIG(I,7)=ZERO
        STIG(I,8)=ZERO
        CONDEN(I,1:8)=ZERO
      ENDDO
      IF (G_PLA > 0) THEN
        DO I=LFT,LLT
          EPLASM(I)=ZERO  
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=LFT,LLT
          EPSDG(I)=ZERO
        ENDDO
      ENDIF
C
      DO N=9,NPE
        DO I=LFT,LLT
          NC(I,N) =IXSP(N-8,I)
          STIG(I,N)=ZERO
          CONDEN(I,N)=ZERO
        ENDDO
      ENDDO
C
      IF (JLAG==0)THEN
        DO I=LFT,LLT
          VDX(I)=ZERO
          VDY(I)=ZERO
          VDZ(I)=ZERO  
        ENDDO
      ENDIF
C
      DO N=1,NPE
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
        IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
C
          DO I=LFT,LLT
            IF(OFFG(I)>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+NPE)
              ZZ(I,N)=SAV(I,N+2*NPE)
              OFF(I) = OFFG(I)-ONE
            ELSE
              NN = MAX(IUN,NC(I,N))
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)
              SAV(I,N)      =XX(I,N)
              SAV(I,N+NPE)  =YY(I,N)
              SAV(I,N+NPE*2)=ZZ(I,N)
              OFF(I) = OFFG(I)
            ENDIF
          ENDDO
C
        ELSE
C
          DO I=LFT,LLT
              NN = MAX(IUN,NC(I,N))
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)
              OFF(I) = MIN(ONE,ABS(OFFG(I)))
              OFF_L  = MIN(OFF_L,OFFG(I))
          ENDDO
C
        ENDIF
C
        DO I=LFT,LLT
            NN = MAX(IUN,NC(I,N))
            VX(I,N)=V(1,NN)
            VY(I,N)=V(2,NN)
            VZ(I,N)=V(3,NN)
            FX(I,N)=ZERO
            FY(I,N)=ZERO
            FZ(I,N)=ZERO    
        ENDDO
        IF(OFF_L<0.)THEN
          DO I=LFT,LLT
            IF(OFFG(I)<0.)THEN
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
      DO N=9,NPE
        N1=IPERM1(N)
        N2=IPERM2(N)
        DO I=LFT,LLT
          IF(NC(I,N)==0)THEN
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
            VX(I,N) = HALF*(VX(I,N1)+VX(I,N2))
            VY(I,N) = HALF*(VY(I,N1)+VY(I,N2))
            VZ(I,N) = HALF*(VZ(I,N1)+VZ(I,N2))    
          ENDIF
        ENDDO
      ENDDO
C-----------
      RETURN
      END
