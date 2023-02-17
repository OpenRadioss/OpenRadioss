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
C-----------------------------------------------
C     Build buckets from segments & particles.
C-----------------------------------------------
Chd|====================================================================
Chd|  SPHREQS                       source/elements/sph/sphreq.F  
Chd|-- called by -----------
Chd|        SPONOF1                       source/elements/sph/sponof1.F 
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|-- calls ---------------
Chd|        SPH_NODSEG                    source/elements/sph/sph_nodseg.F
Chd|====================================================================
      SUBROUTINE SPHREQS(NSEG ,ISEG ,X    ,NCEL ,IXP     ,
     2                LONFSPH ,KXSP ,NSX  ,ISX  ,NSY     ,
     3                ISY     ,NSZ  ,ISZ  ,NPX  ,IPX     ,
     4                NPY     ,IPY  ,NPZ  ,IPZ  ,DPS     ,
     5                WNORMAL ,DT_OLD,MASS_CUM,V,SPBUF   ,
     6                ITYPE)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,ISEG(NIBSPH,*),NCEL,IXP(*),
     .        LONFSPH(*),KXSP(NISP,*),
     .        NSX(0:*),ISX(*),NSY(0:*),ISY(*),NSZ(0:*),ISZ(*),
     .        NPX(0:*),IPX(*),NPY(0:*),IPY(*),NPZ(0:*),IPZ(*),ITYPE
C     REAL
      my_real
     .   X(3,*),DPS(*),WNORMAL(3,*),DT_OLD,MASS_CUM,V(3,*),SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IN,
     .        NBX,NBY,NBZ,NBAUX,NBAUY,NBAUZ,
     .        NBX1,NBY1,NBZ1,NBX2,NBY2,NBZ2,
     .        NBX3,NBY3,NBZ3,NBX4,NBY4,NBZ4,
     .        NP,NS,KP,KS,IB,IBX,IBY,IBZ,TFLAG,J
      my_real
     .       DBOITEX,DBOITEY,DBOITEZ,XI,YI,ZI,NN,XX(12),DPS_OLD
C-----------------------------------------------
C     if more than 1 box, each box size >= DBUCS
C     NBOX=MAX(IUN,INT((XBMAX-XBMIN)/DBUCS))
C     NBOY=MAX(IUN,INT((YBMAX-YBMIN)/DBUCS))
C     NBOZ=MAX(IUN,INT((ZBMAX-ZBMIN)/DBUCS))  
      DBOITEX=MAX(EM20,(XBMAX-XBMIN)/NBOX)
      DBOITEY=MAX(EM20,(YBMAX-YBMIN)/NBOY)
      DBOITEZ=MAX(EM20,(ZBMAX-ZBMIN)/NBOZ)
C-----
      DO IB=0,NBOX+1
       NPX(IB)=0
      ENDDO
C     all points inside (0<=NBX<=NBOX,0<=NBY<=NBOY,0<=NBZ<=NBOZ)
      DO N=1,NCEL
       IN=KXSP(3,LONFSPH(IXP(N)))
       XI =X(1,IN)
       NBX=INT((XI-XBMIN)/DBOITEX)+1
C      bande NBX uniquement.
       IF(NBX>=1.AND.NBX<=NBOX+1)THEN
         NPX(NBX)=NPX(NBX)+1
       ENDIF
      ENDDO
      DO IB=1,NBOX+1
       NPX(IB)=NPX(IB)+NPX(IB-1)
      ENDDO
      DO IB=NBOX+1,1,-1
       NPX(IB)=NPX(IB-1)
      ENDDO
      DO N=1,NCEL
       IN=KXSP(3,LONFSPH(IXP(N)))
       XI =X(1,IN)
       NBX=INT((XI-XBMIN)/DBOITEX)+1
C      bande NBX uniquement.
       IF(NBX>=1.AND.NBX<=NBOX+1)THEN
        NPX(NBX)=NPX(NBX)+1
        IPX(NPX(NBX))=N
       ENDIF
      ENDDO
C-----
      DO IB=0,NBOX+1
       NSX(IB)=0
      ENDDO
C     all points inside (0<=NBX<=NBOX,0<=NBY<=NBOY,0<=NBZ<=NBOZ)
      DO N=1,NSEG
       IN=ISEG(1,N)
       XI =X(1,IN)
       NBX1=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(2,N)
       XI =X(1,IN)
       NBX2=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(3,N)
       XI =X(1,IN)
       NBX3=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(4,N)
       XI =X(1,IN)
       NBX4=INT((XI-XBMIN)/DBOITEX)+1
C      bandes NBX1,NBX2,NBX3,NBX4 & bandes adjacentes.
       DO NBAUX=MIN(NBX1,NBX2,NBX3,NBX4)-1,
     .          MAX(NBX1,NBX2,NBX3,NBX4)+1
        IF(NBAUX>=1.AND.NBAUX<=NBOX+1)THEN
         NSX(NBAUX)=NSX(NBAUX)+1
        ENDIF
       ENDDO
      ENDDO
      DO IB=1,NBOX+1
       NSX(IB)=NSX(IB)+NSX(IB-1)
      ENDDO
      DO IB=NBOX+1,1,-1
       NSX(IB)=NSX(IB-1)
      ENDDO
      DO N=1,NSEG
       IN=ISEG(1,N)
       XI =X(1,IN)
       NBX1=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(2,N)
       XI =X(1,IN)
       NBX2=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(3,N)
       XI =X(1,IN)
       NBX3=INT((XI-XBMIN)/DBOITEX)+1
       IN=ISEG(4,N)
       XI =X(1,IN)
       NBX4=INT((XI-XBMIN)/DBOITEX)+1
C      bandes NBX1,NBX2,NBX3,NBX4 & bandes adjacentes.
       DO NBAUX=MIN(NBX1,NBX2,NBX3,NBX4)-1,
     .          MAX(NBX1,NBX2,NBX3,NBX4)+1
        IF(NBAUX>=1.AND.NBAUX<=NBOX+1)THEN
         NSX(NBAUX)=NSX(NBAUX)+1
         ISX(NSX(NBAUX))=N
        ENDIF
       ENDDO
      ENDDO
C-----
      DO IBX=1,NBOX+1
       DO IBY=0,NBOY+1
        NPY(IBY)=0
       ENDDO
       DO KS=NPX(IBX-1)+1,NPX(IBX)
        N  =IPX(KS)
        IN=KXSP(3,LONFSPH(IXP(N)))
        YI =X(2,IN)
        NBY=INT((YI-YBMIN)/DBOITEY)+1
C       bande NBY uniquement.
        IF(NBY>=1.AND.NBY<=NBOY+1)THEN
          NPY(NBY)=NPY(NBY)+1
        ENDIF
       ENDDO
       DO IBY=1,NBOY+1
        NPY(IBY)=NPY(IBY)+NPY(IBY-1)
       ENDDO
       DO IBY=NBOY+1,1,-1
        NPY(IBY)=NPY(IBY-1)
       ENDDO
       DO KS=NPX(IBX-1)+1,NPX(IBX)
        N  =IPX(KS)
        IN=KXSP(3,LONFSPH(IXP(N)))
        YI =X(2,IN)
        NBY=INT((YI-YBMIN)/DBOITEY)+1
C       bande NBY uniquement.
        IF(NBY>=1.AND.NBY<=NBOY+1)THEN
         NPY(NBY)=NPY(NBY)+1
         IPY(NPY(NBY))=N
        ENDIF
       ENDDO
C------
       DO IBY=0,NBOY+1
        NSY(IBY)=0
       ENDDO
       DO KS=NSX(IBX-1)+1,NSX(IBX)
        N=ISX(KS)
        IN=ISEG(1,N)
        YI =X(2,IN)
        NBY1=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(2,N)
        YI =X(2,IN)
        NBY2=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(3,N)
        YI =X(2,IN)
        NBY3=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(4,N)
        YI =X(2,IN)
        NBY4=INT((YI-YBMIN)/DBOITEY)+1
C       bande NBY & bandes adjacentes.
        DO NBAUY=MIN(NBY1,NBY2,NBY3,NBY4)-1,
     .           MAX(NBY1,NBY2,NBY3,NBY4)+1
         IF(NBAUY>=1.AND.NBAUY<=NBOY+1)THEN
          NSY(NBAUY)=NSY(NBAUY)+1
         ENDIF
        ENDDO
       ENDDO
C------
       DO IBY=1,NBOY+1
        NSY(IBY)=NSY(IBY)+NSY(IBY-1)
       ENDDO
       DO IBY=NBOY+1,1,-1
        NSY(IBY)=NSY(IBY-1)
       ENDDO
C------
       DO KS=NSX(IBX-1)+1,NSX(IBX)
        N=ISX(KS)
        IN=ISEG(1,N)
        YI =X(2,IN)
        NBY1=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(2,N)
        YI =X(2,IN)
        NBY2=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(3,N)
        YI =X(2,IN)
        NBY3=INT((YI-YBMIN)/DBOITEY)+1
        IN=ISEG(4,N)
        YI =X(2,IN)
        NBY4=INT((YI-YBMIN)/DBOITEY)+1
C       bande NBY & bandes adjacentes.
        DO NBAUY=MIN(NBY1,NBY2,NBY3,NBY4)-1,
     .           MAX(NBY1,NBY2,NBY3,NBY4)+1
         IF(NBAUY>=1.AND.NBAUY<=NBOY+1)THEN
          NSY(NBAUY)=NSY(NBAUY)+1
          ISY(NSY(NBAUY))=N
         ENDIF
        ENDDO
       ENDDO
C------
       DO IBY=1,NBOY+1
C-------
        DO IBZ=0,NBOZ+1
         NPZ(IBZ)=0
        ENDDO
        DO KS=NPY(IBY-1)+1,NPY(IBY)
         N  =IPY(KS)
         IN=KXSP(3,LONFSPH(IXP(N)))
         ZI =X(3,IN)
         NBZ=INT((ZI-ZBMIN)/DBOITEZ)+1
C        bande NBZ uniquement.
         IF(NBZ>=1.AND.NBZ<=NBOZ+1)THEN
           NPZ(NBZ)=NPZ(NBZ)+1
         ENDIF
        ENDDO
        DO IBZ=1,NBOZ+1
         NPZ(IBZ)=NPZ(IBZ)+NPZ(IBZ-1)
        ENDDO
        DO IBZ=NBOZ+1,1,-1
         NPZ(IBZ)=NPZ(IBZ-1)
        ENDDO
        DO KS=NPY(IBY-1)+1,NPY(IBY)
         N  =IPY(KS)
         IN=KXSP(3,LONFSPH(IXP(N)))
         ZI =X(3,IN)
         NBZ=INT((ZI-ZBMIN)/DBOITEZ)+1
C        bande NBZ uniquement.
         IF(NBZ>=1.AND.NBZ<=NBOZ+1)THEN
          NPZ(NBZ)=NPZ(NBZ)+1
          IPZ(NPZ(NBZ))=N
         ENDIF
        ENDDO
C-------
        DO IBZ=0,NBOZ+1
         NSZ(IBZ)=0
        ENDDO
        DO KS=NSY(IBY-1)+1,NSY(IBY)
         N=ISY(KS)
         IN=ISEG(1,N)
         ZI =X(3,IN)
         NBZ1=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(2,N)
         ZI =X(3,IN)
         NBZ2=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(3,N)
         ZI =X(3,IN)
         NBZ3=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(4,N)
         ZI =X(3,IN)
         NBZ4=INT((ZI-ZBMIN)/DBOITEZ)+1
C        bande NBZ & bandes adjacentes.
         DO NBAUZ=MIN(NBZ1,NBZ2,NBZ3,NBZ4)-1,
     .            MAX(NBZ1,NBZ2,NBZ3,NBZ4)+1
          IF(NBAUZ>=1.AND.NBAUZ<=NBOZ+1)THEN
           NSZ(NBAUZ)=NSZ(NBAUZ)+1
          ENDIF
         ENDDO
        ENDDO
        DO IBZ=1,NBOZ+1
         NSZ(IBZ)=NSZ(IBZ)+NSZ(IBZ-1)
        ENDDO
        DO IBZ=NBOZ+1,1,-1
         NSZ(IBZ)=NSZ(IBZ-1)
        ENDDO
        DO KS=NSY(IBY-1)+1,NSY(IBY)
         N=ISY(KS)
         IN=ISEG(1,N)
         ZI =X(3,IN)
         NBZ1=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(2,N)
         ZI =X(3,IN)
         NBZ2=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(3,N)
         ZI =X(3,IN)
         NBZ3=INT((ZI-ZBMIN)/DBOITEZ)+1
         IN=ISEG(4,N)
         ZI =X(3,IN)
         NBZ4=INT((ZI-ZBMIN)/DBOITEZ)+1
C        bande NBZ & bandes adjacentes.
         DO NBAUZ=MIN(NBZ1,NBZ2,NBZ3,NBZ4)-1,
     .            MAX(NBZ1,NBZ2,NBZ3,NBZ4)+1
          IF(NBAUZ>=1.AND.NBAUZ<=NBOZ+1)THEN
           NSZ(NBAUZ)=NSZ(NBAUZ)+1
           ISZ(NSZ(NBAUZ))=N
          ENDIF
         ENDDO
        ENDDO
C-------
        DO IBZ=1,NBOZ+1
         DO KP=NPZ(IBZ-1)+1,NPZ(IBZ)
          NP =IPZ(KP)
C
          IF (ITYPE>1) THEN
            DPS(NP)= 1.E+20
            WNORMAL(1,LONFSPH(IXP(NP)))=ZERO
            WNORMAL(2,LONFSPH(IXP(NP)))=ZERO
            WNORMAL(3,LONFSPH(IXP(NP)))=ZERO
            IN=KXSP(3,LONFSPH(IXP(NP)))
            XI =X(1,IN)-DT_OLD*V(1,IN)
            YI =X(2,IN)-DT_OLD*V(2,IN)
            ZI =X(3,IN)-DT_OLD*V(3,IN)
C	  
            DO KS=NSZ(IBZ-1)+1,NSZ(IBZ)
              NS=ISZ(KS)
              DO J=1,4
                IN=ISEG(J,NS)
                XX(3*(J-1)+1)=X(1,IN)-DT_OLD*V(1,IN)
                XX(3*(J-1)+2)=X(2,IN)-DT_OLD*V(2,IN)
                XX(3*(J-1)+3)=X(3,IN)-DT_OLD*V(3,IN)
              END DO
              IF (ISEG(3,NS)/=ISEG(4,NS)) THEN
               TFLAG = 0
              ELSE
               TFLAG = 1
              ENDIF
              CALL SPH_NODSEG(XI,YI,ZI,XX,TFLAG,NP,LONFSPH,IXP,DPS,WNORMAL,0)
            END DO
            DPS_OLD=DPS(NP)
          ENDIF 
C
          DPS(NP)= 1.E+20
          WNORMAL(1,LONFSPH(IXP(NP)))=ZERO
          WNORMAL(2,LONFSPH(IXP(NP)))=ZERO
          WNORMAL(3,LONFSPH(IXP(NP)))=ZERO
          IN=KXSP(3,LONFSPH(IXP(NP)))
          XI =X(1,IN)
          YI =X(2,IN)
          ZI =X(3,IN)
C	  
          DO KS=NSZ(IBZ-1)+1,NSZ(IBZ)
            NS=ISZ(KS)
            DO J=1,4
              IN=ISEG(J,NS)
              XX(3*(J-1)+1)=X(1,IN)
              XX(3*(J-1)+2)=X(2,IN)
              XX(3*(J-1)+3)=X(3,IN)
            END DO
            IF (ISEG(3,NS)/=ISEG(4,NS)) THEN
              TFLAG = 0
            ELSE
              TFLAG = 1
            ENDIF
            CALL SPH_NODSEG(XI,YI,ZI,XX,TFLAG,NP,LONFSPH,IXP,DPS,WNORMAL,0)
          END DO    
C
          IF (ITYPE>1) THEN
C       detection if the particle is crossing the surface
             IF (DPS_OLD*DPS(NP)<ZERO) THEN
C       pur outlet/silent boundaries on permute le sens de comptage
               IF (ITYPE/=4) DPS_OLD = -DPS_OLD
C       la particule a traverse la surface dans le sens +
               IF (DPS_OLD>ZERO) MASS_CUM = MASS_CUM-SPBUF(12,N)
C       la particule a traverse la surface dans le sens -
               IF (DPS_OLD<ZERO) MASS_CUM = MASS_CUM+SPBUF(12,N)
             ENDIF
          ENDIF   
C
          NN=WNORMAL(1,LONFSPH(IXP(NP)))*WNORMAL(1,LONFSPH(IXP(NP)))
     .      +WNORMAL(2,LONFSPH(IXP(NP)))*WNORMAL(2,LONFSPH(IXP(NP)))
     .      +WNORMAL(3,LONFSPH(IXP(NP)))*WNORMAL(3,LONFSPH(IXP(NP)))
          NN=ONE/MAX(EM20,SQRT(NN))
          WNORMAL(1,LONFSPH(IXP(NP)))=WNORMAL(1,LONFSPH(IXP(NP)))*NN
          WNORMAL(2,LONFSPH(IXP(NP)))=WNORMAL(2,LONFSPH(IXP(NP)))*NN
          WNORMAL(3,LONFSPH(IXP(NP)))=WNORMAL(3,LONFSPH(IXP(NP)))*NN
C
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C-----------------------------------------------
      RETURN
      END
