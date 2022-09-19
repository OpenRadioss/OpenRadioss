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
Chd|  S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|-- called by -----------
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S20COOR3(
     1   X      ,V     ,IXS  ,IXS20 ,XX  ,
     2   YY     ,ZZ    ,VX   ,VY    ,VZ  ,
     3   NC     ,NGL   ,MXT  ,NGEO  ,MASS,
     4   DTELEM ,STI   ,SIGG ,EINTG ,RHOG,
     5   QG     ,TEMP0 ,TEMP ,NEL   )
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER NC(MVSIZ,20),MXT(*),NGL(*),NGEO(*),IXS(NIXS,*),IXS20(12,*)
C     REAL
      my_real
     .   X(3,*), V(3,*), XX(MVSIZ,20), YY(MVSIZ,20), ZZ(MVSIZ,20),
     .  VX(MVSIZ,20), VY(MVSIZ,20), VZ(MVSIZ,20),
     .  MASS(MVSIZ),DTELEM(*),STI(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),TEMP(*), TEMP0(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(20),IPERM2(20),N,N1,N2,NN,IUN
C     REAL
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,5,6,7,8,6,7,8,5/
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .    CHECKVOLUME_8N
C=======================================================================
      IUN=1
C
      DO I=LFT,LLT
        NGEO(I)=IXS(10,I)
        NGL(I) =IXS(11,I)
        MXT(I) =IXS(1,I)
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(3,I)
        NC(I,3)=IXS(4,I)
        NC(I,4)=IXS(5,I)
        NC(I,5)=IXS(6,I)
        NC(I,6)=IXS(7,I)
        NC(I,7)=IXS(8,I)
        NC(I,8)=IXS(9,I)
        NC(I,9)  =IXS20(1,I)
        NC(I,10) =IXS20(2,I)
        NC(I,11) =IXS20(3,I)
        NC(I,12) =IXS20(4,I)
        NC(I,13) =IXS20(5,I)
        NC(I,14) =IXS20(6,I)
        NC(I,15) =IXS20(7,I)
        NC(I,16) =IXS20(8,I)
        NC(I,17) =IXS20(9,I)
        NC(I,18) =IXS20(10,I)
        NC(I,19) =IXS20(11,I)
        NC(I,20) =IXS20(12,I)
        DTELEM(I)=EP30
        STI(I)=ZERO
        EINTG(I)=ZERO
        RHOG(I)=ZERO
        QG(I)=ZERO
        SIGG(I,1)=ZERO
        SIGG(I,2)=ZERO
        SIGG(I,3)=ZERO
        SIGG(I,4)=ZERO
        SIGG(I,5)=ZERO
        SIGG(I,6)=ZERO
        IF (CHECKVOLUME_8N(X ,IXS(1,I)) < ZERO) THEN
C         renumber connectivity
          NC(I,1) =IXS(6,I)
          NC(I,2) =IXS(7,I)
          NC(I,3) =IXS(8,I)
          NC(I,4) =IXS(9,I)
          NC(I,5) =IXS(2,I)
          NC(I,6) =IXS(3,I)
          NC(I,7) =IXS(4,I)
          NC(I,8) =IXS(5,I)
          NC(I,9) =IXS20(9,I)
          NC(I,10)=IXS20(10,I)
          NC(I,11)=IXS20(11,I)
          NC(I,12)=IXS20(12,I)
          NC(I,17)=IXS20(1,I)
          NC(I,18)=IXS20(2,I)
          NC(I,19)=IXS20(3,I)
          NC(I,20)=IXS20(4,I)
          IXS(2,I) = NC(I,1)
          IXS(3,I) = NC(I,2)
          IXS(4,I) = NC(I,3)
          IXS(5,I) = NC(I,4)
          IXS(6,I) = NC(I,5)
          IXS(7,I) = NC(I,6)
          IXS(8,I) = NC(I,7)
          IXS(9,I) = NC(I,8)
          IXS20(1,I)  = NC(I,9)
          IXS20(2,I)  = NC(I,10)
          IXS20(3,I)  = NC(I,11)
          IXS20(4,I)  = NC(I,12)
          IXS20(5,I)  = NC(I,13)
          IXS20(6,I)  = NC(I,14)
          IXS20(7,I)  = NC(I,15)
          IXS20(8,I)  = NC(I,16)
          IXS20(9,I)  = NC(I,17)
          IXS20(10,I) = NC(I,18)
          IXS20(11,I) = NC(I,19)
          IXS20(12,I) = NC(I,20)
        ENDIF
      ENDDO
C
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,20
        DO I=LFT,LLT
          NN = MAX(IUN,NC(I,N))
          XX(I,N)=X(1,NN)
          YY(I,N)=X(2,NN)
          ZZ(I,N)=X(3,NN)
          VX(I,N)=V(1,NN)
          VY(I,N)=V(2,NN)
          VZ(I,N)=V(3,NN)
        ENDDO
      ENDDO
C
      DO I=LFT,LLT
        MASS(I)=ZERO
      ENDDO
C
      DO N=9,20
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
C
       IF(JTHE < 0 ) THEN
         IF(NINTEMP > 0 ) THEN
          DO N =1,20
            DO I=LFT,LLT
              NN = MAX(IUN,NC(I,N))
              IF(TEMP(NN)== ZERO) TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ELSE
           DO N =1,20
            DO I=LFT,LLT
               NN = MAX(IUN,NC(I,N))
               TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ENDIF
        ENDIF
C------------
      RETURN
      END
