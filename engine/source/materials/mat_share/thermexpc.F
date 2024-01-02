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
Chd|  THERMEXPC                     source/materials/mat_share/thermexpc.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THERMEXPC(ELBUF_STR,
     1                       JFT  ,JLT  ,FORTH  ,FOR    ,EINT  ,
     2                       OFF  ,ETH  ,THK0   ,EXX    , EYY  ,  
     3                       PM   ,NPT  ,AREA   ,A1     ,A2    , 
     4                       MAT  ,MTN  ,EINTTH ,DIR    ,IR    ,
     5                       IS   ,NLAY ,THK    ,NEL    ,IGTYP ,
     6                       NPF  , TF  ,IPM   , TEMPEL , DTEMP ,
     7                       THKLY ,POSLY ,MOM, MATLY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NPT,MTN,NSG,IR,IS,NLAY,
     .     MAT(MVSIZ),NEL,MATLY(*),IPM(NPROPMI,*),IGTYP,
     .     NPF(*)
C     REAL
      my_real
     .   FOR(NEL,5), FORTH(NEL,2), EINT(JLT,2), EINTTH(*),
     .   OFF(*),EINTT(MVSIZ),DIR(NEL,2),THK(*), PM(NPROPM,*),
     .   THKLY(*),DTEMP(*),TEMPEL(*),TF(*),POSLY(MVSIZ,*),
     .   MOM(NEL,3)
      my_real
     .   EXX(MVSIZ), EYY(MVSIZ), ETH(MVSIZ),THK0(MVSIZ) ,
     .   AREA(MVSIZ),PLA(MVSIZ),A1(MVSIZ), A2(MVSIZ)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, MX, J,IORTH,IPT,ILAY,II(5),NPTT,IT,
     .         IORTH_LAY,IFUNC_ALPHA,J1,J2,J3,JJ,IPT_ALL
C     REAL
      my_real
     .   DTINV, AMU, FACT, VISC,M12,ANU12,ANU21,S1,S2,
     .   FSCAL_ALPHA,ALPHA,DEINTTH,DF,WMC,AA,THKLAY,KXX,KYY
      my_real
     .   NU(MVSIZ),E(MVSIZ),A12(MVSIZ),
     .   E11(MVSIZ), E22(MVSIZ),
     .   B3(MVSIZ), DEGMB(MVSIZ), DEGFX(MVSIZ),DEGMBTH(MVSIZ),
     .   EZZ(MVSIZ),EINF(MVSIZ),P(MVSIZ),DEGFXTH(MVSIZ),
     .   P1(MVSIZ),P2(MVSIZ),ETHKE(MVSIZ),ZI2(MVSIZ),SUMALZ(MVSIZ)
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-----------------------------------------------    
      my_real FINTER 
      EXTERNAL FINTER
C-----------------------------------------------  
      DO I=1,5
        II(I) = NEL*(I-1)
      ENDDO
C-------- 1st Step : Elasticity matrix-----------------------
      IORTH_LAY = 0
      IORTH= -1! not activated
      IF(IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
        IORTH_LAY = 1
      ELSEIF (MTN==19.OR.MTN==15.OR.MTN==25) THEN
        IORTH=1
      ELSE
        IORTH=0
      ENDIF
C
      IF(IORTH == 1) THEN
        MX  =MAT(JFT)
        DO I=JFT,JLT
          E(I) = PM(20,MX)
          NU(I) = PM(21,MX)
          A1(I) = PM(24,MX)
          A2(I) = PM(25,MX)
C
          E11(I)  = PM(33,MX)
          E22(I)  = PM(34,MX)
          ANU12   = PM(35,MX)
          ANU21   = PM(36,MX)
C
          A12(I) = (1.-ANU12*ANU21)
          A1(I) = E11(I)/A12(I)
          A2(I) = E22(I)/A12(I)
          A12(I) = ANU21*A1(I)
        ENDDO
      ENDIF
C-------- 2nd Step : Thermal stress computation -----------------------

        IF(IORTH ==0)THEN
          MX  =MAT(JFT)
          DO I=JFT,JLT
            P(I) =(A1(I)+A2(I))*ETH(I)
            FORTH(I,1)=FORTH(I,1)+ P(I)
            FORTH(I,2)=FORTH(I,2)+ P(I)
          END DO
        ELSEIF(IORTH == 1) THEN
          DO I=JFT,JLT
             P1(I) = A1(I)*ETH(I)+A12(I)*ETH(I)
             P2(I) = A12(I)*ETH(I)+A2(I)*ETH(I)
             S1 = DIR(I,1)*DIR(I,1)*P1(I)
     .          + DIR(I,2)*DIR(I,2)*P2(I)
             S2 = DIR(I,2)*DIR(I,2)*P1(I)
     .          + DIR(I,1)*DIR(I,1)*P2(I)
             FORTH(I,1)=FORTH(I,1)+ S1
             FORTH(I,2)=FORTH(I,2)+ S2
          END DO   
        ENDIF 
C
C
      IF(IORTH_LAY > 0 ) THEN
         ETHKE(JFT : JLT) = ZERO
        IF(MTN == 15 .OR. MTN == 25) THEN
          IPT_ALL = 0
          DO ILAY=1,NLAY
             NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
             J1 = 1+(ILAY-1)*JLT     ! JMLY
             J3 = 1+(ILAY-1)*JLT*2     ! jdir
            DO IT=1,NPTT
              LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT) 
              IPT = IPT_ALL + IT        ! count all NPTT through all layers
              J2 = 1+(IPT-1)*JLT        ! JPOS
              DO I=JFT,JLT 
                JJ = J2 - 1 + I
                MX = MATLY(J1+I-1)
C
                E11(I)  = PM(33,MX)
                E22(I)  = PM(34,MX)
                ANU12   = PM(35,MX)
                ANU21   = PM(36,MX)
C
                IFUNC_ALPHA = IPM(219, MX) 
                FSCAL_ALPHA = PM(191, MX)
                ALPHA = FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,DF)
                ALPHA = ALPHA * FSCAL_ALPHA
                ETH(I) = ALPHA*DTEMP(I)
C                  
                A12(I) = (ONE - ANU12*ANU21)
                A1(I)  = E11(I)/A12(I)
                A2(I)  = E22(I)/A12(I)
                A12(I) =   ANU21*A1(I)
C
                P1(I) = A1(I)*ETH(I ) + A12(I)*ETH(I)
                P2(I) = A12(I)*ETH(I) + A2(I)*ETH(I) 
C                
                LBUF%SIG(II(1)+I)=LBUF%SIG(II(1)+I) - P1(I) 
                LBUF%SIG(II(2)+I)=LBUF%SIG(II(2)+I) - P2(I)
C                
                FOR(I,1)=FOR(I,1) - THKLY(JJ)*P1(I) 
                FOR(I,2)=FOR(I,2) - THKLY(JJ)*P2(I)
C                
                WMC = POSLY(I,IPT)*THKLY(JJ)
                MOM(I,1) = MOM(I,1) - WMC*P1(I)
                MOM(I,2) = MOM(I,2) - WMC*P2(I)  
C       
                THKLAY = THKLY(JJ)*THK0(I)
                ETHKE(I) = ETHKE(I) + THKLAY*ETH(I)             
              ENDDO
            ENDDO !!  DO IT=1,NPTT
             IPT_ALL = IPT_ALL + NPTT
          ENDDO  ! DO ILAY=1,NLAY  
         ELSEIF(MTN > 26) THEN
           AA = ZERO
           IPT_ALL = 0
           ZI2(JFT:JLT) = ZERO
           SUMALZ(JFT:JLT) = ZERO
           DO ILAY=1,NLAY
            NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
            J1 = 1+(ILAY-1)*JLT     ! JMLY
            J3 = 1+(ILAY-1)*JLT*2 
            DO IT=1,NPTT
              LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT) 
              IPT = IPT_ALL + IT        ! count all NPTT through all layers
              J2 = 1+(IPT-1)*JLT        ! JPOS
              DO I=JFT,JLT
                JJ = J2 - 1 + I
                MX = MATLY(J1+I-1)
                E(I)  = PM(20,MX)
                NU(I) = PM(21,MX)
                A1(I) = PM(24,MX)
                A2(I) = PM(25,MX)
                A1(I) = E(I)/ (ONE - NU(I)*NU(I))
                A2(I) = NU(I)*A1(I)
C                    
                IFUNC_ALPHA = IPM(219, MX) 
                FSCAL_ALPHA = PM(191, MX)
                ALPHA = FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,DF)
                ALPHA = ALPHA * FSCAL_ALPHA
                ETH(I) = ALPHA*DTEMP(I)    
C            
                P(I) = A1(I)*ETH(I) + A2(I)*ETH(I)              
C                
                LBUF%SIG(II(1)+I)=LBUF%SIG(II(1)+I) - P(I) 
                LBUF%SIG(II(2)+I)=LBUF%SIG(II(2)+I) - P(I)
C forces  
                FOR(I,1)=FOR(I,1) - THKLY(JJ)*P(I) 
                FOR(I,2)=FOR(I,2) - THKLY(JJ)*P(I)
C                                                        
                WMC = POSLY(I,IPT)*THKLY(JJ)
                MOM(I,1) = MOM(I,1) - WMC*P(I)
                MOM(I,2) = MOM(I,2) - WMC*P(I)                
!!                ZI2(I) = ZI2(I) +  POSLY(I,IPT)**2
!!                SUMALZ(I) = SUMALZ(I) + POSLY(I,IPT)* ETH(I)
C      
                THKLAY = THKLY(JJ)*THK0(I)
                ETHKE(I) = ETHKE(I) + THKLAY*ETH(I)
              ENDDO ! I 
            ENDDO !!  DO IT=1,NPTT
             IPT_ALL = IPT_ALL + NPTT
          ENDDO  ! DO ILAY=1,NLAY
        ENDIF
          
         DO I=JFT,JLT
C-------- 3rd Step : Energies computation -----------------------
!!             KXX = SUMALZ(I)/ ZI2(I)
!!             KYY = KXX  
!!    DEGFXTH(I) = -( MOM(I,1)*KXX + MOM(I,2)*KYY)*HALF*THK0(I)*THK0(I)*AREA(I) ! depending to kxx et kyy
             DEGMBTH(I) = -(FOR(I,1)+FOR(I,2))*ETHKE(I)*HALF*AREA(I)
             EINTTH(I) = EINTTH(I) + DEGMBTH(I)
             EINT(I,1) = EINT(I,1) + DEGMBTH(I)
!!             EINT(I,2) = EINT(I,2) + DEGFXTH(I)
C------Thickness change due to thermal expansion-------           
             THK(I) = (THK(I)  + ETHKE(I))*OFF(I)
          ENDDO        
      ELSE
        IF(NPT/=0) THEN
          DO ILAY=1,NLAY
            NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
            DO IT=1,NPTT
              LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT) 
              IF(IORTH ==0)THEN
                DO I=JFT,JLT
                  LBUF%SIG(II(1)+I)=LBUF%SIG(II(1)+I)-P(I) 
                  LBUF%SIG(II(2)+I)=LBUF%SIG(II(2)+I)-P(I) 
                ENDDO
              ELSE
                DO I=JFT,JLT
                  LBUF%SIG(II(1)+I)=LBUF%SIG(II(1)+I)-P1(I) 
                  LBUF%SIG(II(2)+I)=LBUF%SIG(II(2)+I)-P2(I) 
                ENDDO
              ENDIF
            ENDDO
          ENDDO     
         ENDIF
         IF(IORTH ==0)THEN
           DO I=JFT,JLT
              FOR(I,1)=FOR(I,1) - P(I) 
              FOR(I,2)=FOR(I,2) - P(I)  
           ENDDO
         ELSE
           DO I=JFT,JLT
              FOR(I,1)=FOR(I,1) - P1(I) 
              FOR(I,2)=FOR(I,2) - P2(I) 
            ENDDO
         ENDIF
C
C-------- 3rd Step : Energies computation -----------------------
         DO I=JFT,JLT
           DEGMBTH(I) = -(FOR(I,1)+FOR(I,2))*ETH(I)*HALF*THK0(I)*AREA(I)
         END DO

         DO I=JFT,JLT
          EINTTH(I) = EINTTH(I) + DEGMBTH(I)
          EINT(I,1) = EINT(I,1) + DEGMBTH(I)
         ENDDO
C------Thickness change due to thermal expansion-------
          DO I=JFT,JLT
            THK(I) = THK(I) *(1 + ETH(I))*OFF(I)
          ENDDO          
        ENDIF ! IORTH_LAY > 0
C
      RETURN
      END

