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
Chd|  S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10COOR3(
     1                    X      ,V     ,IXS   ,IXS10 ,XX  ,
     2                    YY     ,ZZ    ,VX    ,VY    ,VZ  ,
     3                    NC     ,NGL   ,MXT   ,NGEO  ,MASS,
     4                    DTELEM ,STI   ,SIGG  ,EINTG ,RHOG,
     5                    QG     ,TEMP0 ,TEMP  ,SAV   ,NEL )
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),SAV(NEL,30)

      my_real
     .  X(3,*), V(3,*),
     .  VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10),
     .  MASS(MVSIZ),DTELEM(*),STI(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),TEMP0(*), TEMP(*)
      INTEGER NC(MVSIZ,10), MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*),IXS10(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN
C     REAL
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .    CHECKVOLUME_4N
C=======================================================================
      IUN = 1
C
      DO I=LFT,LLT
        NGEO(I) =IXS(10,I)
        NGL(I)  =IXS(11,I)
        MXT(I)  =IXS(1,I)
        NC(I,1) =IXS(2,I)
        NC(I,2) =IXS(4,I)
        NC(I,3) =IXS(7,I)
        NC(I,4) =IXS(6,I)
        IF(ISROT /= 1)THEN
         NC(I,5) =IXS10(1,I)
         NC(I,6) =IXS10(2,I)
         NC(I,7) =IXS10(3,I)
         NC(I,8) =IXS10(4,I)
         NC(I,9) =IXS10(5,I)
         NC(I,10)=IXS10(6,I)
        ELSE
         NC(I,5) =ZERO
         NC(I,6) =ZERO
         NC(I,7) =ZERO
         NC(I,8) =ZERO
         NC(I,9) =ZERO
         NC(I,10)=ZERO
        ENDIF
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
        IF (CHECKVOLUME_4N(X ,IXS(1,I)) < ZERO) THEN
C         renumber connectivity
          NC(I,1) =IXS(2,I)
          NC(I,2) =IXS(6,I)
          NC(I,3) =IXS(7,I)
          NC(I,4) =IXS(4,I)
          IXS(2,I) = NC(I,1)
          IXS(4,I) = NC(I,2)
          IXS(7,I) = NC(I,3)
          IXS(6,I) = NC(I,4)
          IF(ISROT /= 1)THEN
            NC(I,5) =IXS10(4,I)
            NC(I,6) =IXS10(6,I)
            NC(I,7) =IXS10(3,I)
            NC(I,8) =IXS10(1,I)
            NC(I,9) =IXS10(5,I)
            NC(I,10)=IXS10(2,I)
            IXS10(1,I) = NC(I,5) 
            IXS10(2,I) = NC(I,6) 
            IXS10(3,I) = NC(I,7) 
            IXS10(4,I) = NC(I,8) 
            IXS10(5,I) = NC(I,9) 
            IXS10(6,I) = NC(I,10)
          ENDIF
        ENDIF
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,10
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
      DO N=5,10
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
C   initial nodal temperature
C
        IF(JTHE < 0 ) THEN
         IF(NINTEMP > 0 ) THEN
          DO N =1,10
            DO I=LFT,LLT  
              NN = MAX(IUN,NC(I,N))
              IF(TEMP(NN)== ZERO) TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ELSE
           DO N =1,10
            DO I=LFT,LLT
               NN = MAX(IUN,NC(I,N))
               TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ENDIF   
        ENDIF
C
        IF(ISMSTR>=10.AND.ISMSTR<=12)THEN
          DO N=1,10
           DO I=LFT,LLT
            NN = NC(I,N)
            SAV(I,N)   =XX(I,N)
            SAV(I,N+10)=YY(I,N)
            SAV(I,N+20)=ZZ(I,N)
           ENDDO
          END DO 
        END IF
CC        
      RETURN
      END
Chd|====================================================================
Chd|  S10COOR3_OLD                  source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10COOR3_OLD(X,V, IXS,IXS10,
     .   XX, YY, ZZ, VX, VY, VZ,NC,
     .   NGL,MXT,NGEO,MASS,DTELEM,STI,
     .   SIGG,EINTG,RHOG,QG, TEMP0,
     .   TEMP,NEL)
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)

      my_real
     .  X(3,*), V(3,*),
     .  VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10),
     .  MASS(MVSIZ),DTELEM(*),STI(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),TEMP0(*), TEMP(*)
      INTEGER NC(MVSIZ,10), MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*),IXS10(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(10),IPERM2(10),N,N1,N2,NN,IUN
C     REAL
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .    CHECKVOLUME_4N
C=======================================================================
      IUN = 1
C
      DO I=LFT,LLT
        NGEO(I) =IXS(10,I)
        NGL(I)  =IXS(11,I)
        MXT(I)  =IXS(1,I)
        NC(I,1) =IXS(2,I)
        NC(I,2) =IXS(4,I)
        NC(I,3) =IXS(7,I)
        NC(I,4) =IXS(6,I)
        IF(ISROT /= 1)THEN
         NC(I,5) =IXS10(1,I)
         NC(I,6) =IXS10(2,I)
         NC(I,7) =IXS10(3,I)
         NC(I,8) =IXS10(4,I)
         NC(I,9) =IXS10(5,I)
         NC(I,10)=IXS10(6,I)
        ELSE
         NC(I,5) =ZERO
         NC(I,6) =ZERO
         NC(I,7) =ZERO
         NC(I,8) =ZERO
         NC(I,9) =ZERO
         NC(I,10)=ZERO
        ENDIF
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
        IF (CHECKVOLUME_4N(X ,IXS(1,I)) < ZERO) THEN
C         renumber connectivity
          NC(I,1) =IXS(2,I)
          NC(I,2) =IXS(6,I)
          NC(I,3) =IXS(7,I)
          NC(I,4) =IXS(4,I)
          IXS(2,I) = NC(I,1)
          IXS(4,I) = NC(I,2)
          IXS(7,I) = NC(I,3)
          IXS(6,I) = NC(I,4)
          IF(ISROT /= 1)THEN
            NC(I,5) =IXS10(4,I)
            NC(I,6) =IXS10(6,I)
            NC(I,7) =IXS10(3,I)
            NC(I,8) =IXS10(1,I)
            NC(I,9) =IXS10(5,I)
            NC(I,10)=IXS10(2,I)
            IXS10(1,I) = NC(I,5) 
            IXS10(2,I) = NC(I,6) 
            IXS10(3,I) = NC(I,7) 
            IXS10(4,I) = NC(I,8) 
            IXS10(5,I) = NC(I,9) 
            IXS10(6,I) = NC(I,10)
          ENDIF
        ENDIF
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,10
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
      DO N=5,10
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
C   initial nodal temperature
C
        IF(JTHE < 0 ) THEN
         IF(NINTEMP > 0 ) THEN
          DO N =1,10
            DO I=LFT,LLT  
              NN = MAX(IUN,NC(I,N))
              IF(TEMP(NN)== ZERO) TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ELSE
           DO N =1,10
            DO I=LFT,LLT
               NN = MAX(IUN,NC(I,N))
               TEMP(NN) = TEMP0(I)
            ENDDO
          ENDDO
         ENDIF   
        ENDIF
CC        
      RETURN
      END

