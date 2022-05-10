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
Chd|  M46LAW                        source/materials/mat/mat046/m46law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        SIGEPS46                      source/materials/mat/mat046/sigeps46.F
Chd|====================================================================
      SUBROUTINE M46LAW (LFT   ,LLT    ,NFT   ,MTN     ,
     2                  PM     ,OFF    ,SIG   ,EINT    ,RHO   ,
     3                  QOLD   ,VOL    ,UVAR  ,BUFMAT  ,STIFN ,
     4                  MAT    ,NGL    ,NUVAR ,DT2T    ,NELTST,RHO0   ,
     5                  ITYPTST,OFFG   ,JLAG  ,GEO     ,PID   ,SSP    ,
     6                  AIRE   ,VOLN   ,VD2   ,DELTAX  ,VIS   ,D1     , 
     7                  D2     ,D3     ,PNEW  ,PSH     ,Q     ,SSP_EQ ,
     8                  WXX  ,
     9                  WYY    ,WZZ    ,IPM   ,MSSA    ,DMELS ,DVOL   ,
     A                  SOLD1  ,SOLD2  ,SOLD3 ,SOLD4   ,SOLD5 ,SOLD6  ,
     B                  CONDE  ,VOL_AVG,DTEL  ,G_DT    ,NEL   ,D4     ,
     C                  D5     ,D6     ,RHOREF  ,RHOSP   )
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
#include      "units_c.inc"
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT     ,LLT    ,NFT     ,MTN, NUVAR,
     .   MAT(MVSIZ),NGL(MVSIZ),PID(MVSIZ),IPM(NPROPMI,*),G_DT,NEL
C
      INTEGER NELTST, ITYPTST,JLAG
      my_real DT2T
C 
      my_real  PM(NPROPM,*),OFF(*), SIG(NEL,6), EINT(*), 
     .         RHO(*),QOLD(*), VOL(*), UVAR(*), BUFMAT(*),
     .         OFFG(*),STIFN(*),GEO(*) ,SSP(*) ,
     .         AIRE(*) ,VOLN(*) ,VD2(*) ,DELTAX(*) ,VIS(*) , D1(*) , 
     .         D2(*) ,D3(*) ,PNEW(*) ,PSH(*) ,Q(*) ,SSP_EQ(*) ,
     .         WXX(MVSIZ), WYY(MVSIZ), WZZ(MVSIZ),RHO0(MVSIZ),
     .         MSSA(*), DMELS(*),SOLD1(*) ,SOLD2(*) ,SOLD3(*) ,
     .         SOLD4(*) ,SOLD5(*) ,SOLD6(*), DVOL(MVSIZ),CONDE(*),
     .         VOL_AVG(*),DTEL(*),D4(MVSIZ),D5(MVSIZ),D6(MVSIZ),
     .         RHOREF(*)  ,RHOSP(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .E1,E2,E3,E4,E5,E6,P2,BID1,BID2,BID3,C1(MVSIZ),
     .EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),EP4(MVSIZ),EP5(MVSIZ),EP6(MVSIZ),
     .S1(MVSIZ) ,S2(MVSIZ) ,S3(MVSIZ) ,S4(MVSIZ) ,S5(MVSIZ) ,S6(MVSIZ),
     .DE1(MVSIZ),DE2(MVSIZ),DE3(MVSIZ),DE4(MVSIZ),DE5(MVSIZ),DE6(MVSIZ),
     .SV1(MVSIZ),SV2(MVSIZ),SV3(MVSIZ),SV4(MVSIZ),SV5(MVSIZ),SV6(MVSIZ),
     .SO1(MVSIZ),SO2(MVSIZ),SO3(MVSIZ),SO4(MVSIZ),SO5(MVSIZ),SO6(MVSIZ)
C
      INTEGER NPAR,IADBUF,I,IBID,MX
      my_real  FACQ0
c=======================================================================
      MX = MAT(1)
      NPAR   = IPM(9,MX)
      IADBUF = IPM(7,MX)
      FACQ0 = UN
      
C
      DO I=LFT,LLT
        VIS(I) = ZERO
        EP1(I) = D1(I)*OFF(I)
        EP2(I) = D2(I)*OFF(I)
        EP3(I) = D3(I)*OFF(I)
        EP4(I) = D4(I)*OFF(I)
        EP5(I) = D5(I)*OFF(I)
        EP6(I) = D6(I)*OFF(I)
        DE1(I) = EP1(I)*DT1
        DE2(I) = EP2(I)*DT1
        DE3(I) = EP3(I)*DT1
        DE4(I) = EP4(I)*DT1
        DE5(I) = EP5(I)*DT1
        DE6(I) = EP6(I)*DT1
      ENDDO
      DO I=LFT,LLT
        SO1(I) = SIG(I,1)
        SO2(I) = SIG(I,2)
        SO3(I) = SIG(I,3)
        SO4(I) = SIG(I,4)
        SO5(I) = SIG(I,5)
        SO6(I) = SIG(I,6)
      ENDDO
C
      CALL SIGEPS46(LLT ,NPAR,NUVAR,
     .                  TT,DT1,BUFMAT,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     .                  IPM ,MAT ,WXX,WYY,WZZ,MTN,DELTAX,
     .                  AIRE)
      DO I=LFT,LLT
        SIG(I,1) = S1(I)*OFF(I)
        SIG(I,2) = S2(I)*OFF(I)
        SIG(I,3) = S3(I)*OFF(I)
        SIG(I,4) = S4(I)*OFF(I)
        SIG(I,5) = S5(I)*OFF(I)
        SIG(I,6) = S6(I)*OFF(I)
        SVIS(I,1)= SV1(I)*OFF(I)
        SVIS(I,2)= SV2(I)*OFF(I)
        SVIS(I,3)= SV3(I)*OFF(I)
        SVIS(I,4)= SV4(I)*OFF(I)
        SVIS(I,5)= SV5(I)*OFF(I)
        SVIS(I,6)= SV6(I)*OFF(I)
      ENDDO
C-------------------------------------------
C   BULK VISCOSITY AND TIME STEP COMPUTATION
C   THIS SUBROUTINE RETURN THE NEW BULK VISCOSITY Q
C-----------------------
      CALL MQVISCB(PM     ,OFF     ,RHO     ,BID1  ,BID2    ,SSP   ,
     2            BID3    ,STIFN   ,DT2T   ,NELTST ,ITYPTST ,AIRE  ,
     3            OFFG    ,GEO     ,PID    ,VOLN   ,VD2     ,DELTAX,
     4            VIS     ,D1      ,D2     ,D3     ,PNEW    ,PSH   ,
     5            MAT     ,NGL     ,Q      ,SSP_EQ ,VOL     ,MSSA  ,
     6            DMELS   ,IBID    ,FACQ0   ,CONDE  ,DTEL  ,G_DT   ,
     7            IPM     ,RHOREF  ,RHOSP   )
C
      DO I=LFT,LLT
        P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+
     .         SIG(I,2)+SOLD3(I)+SIG(I,3))/TROIS
        EINT(I)=EINT(I)-(Q(I)+QOLD(I)+P2)*DVOL(I)*UNDEMI
        QOLD(I)=Q(I)
      ENDDO
      IF(INCOMP.EQ.0 .OR. JLAG.EQ.1)THEN
        DO I=LFT,LLT
         E1=D1(I)*SVIS(I,1)
         E2=D2(I)*SVIS(I,2)
         E3=D3(I)*SVIS(I,3)
         E4=D4(I)*SVIS(I,4)
         E5=D5(I)*SVIS(I,5)
         E6=D6(I)*SVIS(I,6)
         EINT(I)=EINT(I)+(E1+E2+E3+E4+E5+E6)*VOL_AVG(I)*DT1
        ENDDO
      ENDIF
      DO I=LFT,LLT
        EINT(I)=EINT(I)/VOL(I)
      ENDDO
C DANS LE CAS QUASI COMPRESSIBLE ALE ET EULERIEN, L'ENERGIE
C N'EST PAS TRANSPORTEE. ON N'ACCUMULE DONC PAS LES TERMES VISQUEUX
C
c      IF(INCOMP.EQ.1.AND. JLAG.EQ.0)THEN
c       DO I=LFT,LLT
c        E1=D1(I)*(SOLD1(I)+SIG(1,I))
c        E2=D2(I)*(SOLD2(I)+SIG(2,I))
c        E3=D3(I)*(SOLD3(I)+SIG(3,I))
c        E4=D4(I)*(SOLD4(I)+SIG(4,I))
c        E5=D5(I)*(SOLD5(I)+SIG(5,I))
c        E6=D6(I)*(SOLD6(I)+SIG(6,I))
c        EINT(I)=EINT(I)
c     .       +(E1+E2+E3+E4+E5+E6)*(VOLN(I)-0.5*DVOL(I))*DT1*0.5
c        EINT(I)=EINT(I)/VOL(I)
c        QOLD(I)=Q(I)
c       ENDDO
c       RETURN
c      ENDIF
cC
c      DO 140 I=LFT,LLT
c      E1=D1(I)*(SOLD1(I)+SIG(1,I))
c      E2=D2(I)*(SOLD2(I)+SIG(2,I))
c      E3=D3(I)*(SOLD3(I)+SIG(3,I))
c      E4=D4(I)*(SOLD4(I)+SIG(4,I))
c      E5=D5(I)*(SOLD5(I)+SIG(5,I))
c      E6=D6(I)*(SOLD6(I)+SIG(6,I))
c      EINT(I)=EINT(I)
c     .       -(Q(I)+QOLD(I))*DVOL(I)*0.5
c     .       +(E1+E2+E3+E4+E5+E6)*(VOLN(I)-0.5*DVOL(I))*DT1*0.5
c      EINT(I)=EINT(I)/VOL(I)
c      QOLD(I)=Q(I)
c  140 CONTINUE

C------------------------------------------
      RETURN
      END
