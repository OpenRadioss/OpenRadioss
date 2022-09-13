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
Chd|  I24CORK3                      source/interfaces/int24/i24cork3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24CORK3(
     1               JLT     ,X      ,IRECT ,NSV   ,CAND_E ,
     2               CAND_N  ,STF    ,STFN  ,STIF  ,
     3               XX0     ,YY0    ,ZZ0   ,VX    ,VY     ,
     5               VZ      ,XI     ,YI    ,ZI    ,VXI    ,
     7               VYI     ,VZI    ,IXX   ,NSVG  ,NVOISIN,
     9               MS      ,MSI    ,NSN   ,V     ,
     A               KINI    ,ITY    ,NIN   ,IGSTI ,KMIN   ,
     B               KMAX    ,GAP_S  ,GAPS  ,ITRIV   ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        JLT,IDT, NOINT ,NDDIM, NSN, ITY, NIN, IGSTI,
     .        NVOISIN(8,*), KINI(*)
      INTEGER IXX(MVSIZ,13), NSVG(MVSIZ), ITRIV(4,MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*),
     .   MS(*), V(3,*),GAPS(MVSIZ),GAP_S(*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   XX0(MVSIZ,17),YY0(MVSIZ,17),ZZ0(MVSIZ,17),
     .   VX(MVSIZ,17),VY(MVSIZ,17),VZ(MVSIZ,17),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   KMIN, KMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI
C-----------------------------------------------
C
C
      DO I=1,JLT
           NI = CAND_N(I)
           L  = IABS(CAND_E(I))
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
c             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
             GAPS(I) = GAP_S(NI)
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
c             KINI(I) = KINFI(NIN)%P(NN)
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
             GAPS(I) = GAPFI(NIN)%P(NN)
           END IF
C
           IX=IRECT(1,L)  
           IXX(I,1)=IX      
           XX0(I,1)=X(1,IX)  
           YY0(I,1)=X(2,IX)  
           ZZ0(I,1)=X(3,IX)  
           VX(I,1)=V(1,IX)  
           VY(I,1)=V(2,IX)  
           VZ(I,1)=V(3,IX)  
C
           IX=IRECT(2,L)  
           IXX(I,2)=IX      
           XX0(I,2)=X(1,IX)  
           YY0(I,2)=X(2,IX)  
           ZZ0(I,2)=X(3,IX)  
           VX(I,2)=V(1,IX)  
           VY(I,2)=V(2,IX)  
           VZ(I,2)=V(3,IX)  
C
           IX=IRECT(3,L)  
           IXX(I,3)=IX      
           XX0(I,3)=X(1,IX)  
           YY0(I,3)=X(2,IX)  
           ZZ0(I,3)=X(3,IX)  
           VX(I,3)=V(1,IX)  
           VY(I,3)=V(2,IX)  
           VZ(I,3)=V(3,IX)  
C
           IX=IRECT(4,L)  
           IXX(I,4)=IX      
           XX0(I,4)=X(1,IX)  
           YY0(I,4)=X(2,IX)  
           ZZ0(I,4)=X(3,IX)  
           VX(I,4)=V(1,IX)  
           VY(I,4)=V(2,IX)  
           VZ(I,4)=V(3,IX)  
C
           IF(IXX(I,3) /= IXX(I,4))THEN
            XX0(I,5) = FOURTH*(XX0(I,1)+XX0(I,2)+XX0(I,3)+XX0(I,4))
            YY0(I,5) = FOURTH*(YY0(I,1)+YY0(I,2)+YY0(I,3)+YY0(I,4))
            ZZ0(I,5) = FOURTH*(ZZ0(I,1)+ZZ0(I,2)+ZZ0(I,3)+ZZ0(I,4)) 
            VX(I,5) = FOURTH*(VX(I,1)+VX(I,2)+VX(I,3)+VX(I,4))
            VY(I,5) = FOURTH*(VY(I,1)+VY(I,2)+VY(I,3)+VY(I,4))
            VZ(I,5) = FOURTH*(VZ(I,1)+VZ(I,2)+VZ(I,3)+VZ(I,4)) 
           ELSE
            XX0(I,5) = XX0(I,3)
            YY0(I,5) = YY0(I,3)
            ZZ0(I,5) = ZZ0(I,3) 
            VX(I,5) = VX(I,3)
            VY(I,5) = VY(I,3)
            VZ(I,5) = VZ(I,3) 
           ENDIF

           IX=IABS(NVOISIN(1,L))  
           IXX(I,6)=IX 
           IF(IX /= 0)THEN 
            XX0(I,6)=X(1,IX) 
            YY0(I,6)=X(2,IX) 
            ZZ0(I,6)=X(3,IX) 
            VX(I,6) =V(1,IX) 
            VY(I,6) =V(2,IX) 
            VZ(I,6) =V(3,IX)
           ELSE
            XX0(I,6)=XX0(I,1) 
            YY0(I,6)=YY0(I,1) 
            ZZ0(I,6)=ZZ0(I,1) 
            VX(I,6) =VX(I,1)  
            VY(I,6) =VY(I,1)  
            VZ(I,6) =VZ(I,1) 
           ENDIF
 
           IF(NVOISIN(2,L)/=0)IX=IABS(NVOISIN(2,L)) 
           IXX(I,7)=IX  
           IF(IX /= 0)THEN 
            XX0(I,7)=X(1,IX) 
            YY0(I,7)=X(2,IX) 
            ZZ0(I,7)=X(3,IX) 
            VX(I,7)=V(1,IX) 
            VY(I,7)=V(2,IX) 
            VZ(I,7)=V(3,IX)
           ELSE
            XX0(I,7)=XX0(I,2) 
            YY0(I,7)=YY0(I,2) 
            ZZ0(I,7)=ZZ0(I,2) 
            VX(I,7) =VX(I,2)  
            VY(I,7) =VY(I,2)  
            VZ(I,7) =VZ(I,2) 
           ENDIF

           IF(NVOISIN(1,L)<0)THEN
             IF(NVOISIN(2,L)<0)THEN
               ITRIV(1,I)=4
             ELSE
               ITRIV(1,I)=2
             ENDIF
           ELSEIF(NVOISIN(2,L)<0)THEN
               ITRIV(1,I)=3
           ELSE
               ITRIV(1,I)=1
           ENDIF

           IX=IABS(NVOISIN(3,L))  
           IXX(I,8)=IX  
           IF(IX /= 0)THEN 
            XX0(I,8)=X(1,IX) 
            YY0(I,8)=X(2,IX) 
            ZZ0(I,8)=X(3,IX) 
            VX(I,8)=V(1,IX) 
            VY(I,8)=V(2,IX) 
            VZ(I,8)=V(3,IX) 
           ELSE
            XX0(I,8)=XX0(I,2) 
            YY0(I,8)=YY0(I,2) 
            ZZ0(I,8)=ZZ0(I,2) 
            VX(I,8) =VX(I,2)  
            VY(I,8) =VY(I,2)  
            VZ(I,8) =VZ(I,2) 
           ENDIF
 
           IF(NVOISIN(4,L)/=0)IX=IABS(NVOISIN(4,L)) 
           IXX(I,9)=IX  
           IF(IX /= 0)THEN 
            XX0(I,9)=X(1,IX) 
            YY0(I,9)=X(2,IX) 
            ZZ0(I,9)=X(3,IX) 
            VX(I,9)=V(1,IX) 
            VY(I,9)=V(2,IX) 
            VZ(I,9)=V(3,IX)
           ELSE
            XX0(I,9)=XX0(I,3) 
            YY0(I,9)=YY0(I,3) 
            ZZ0(I,9)=ZZ0(I,3) 
            VX(I,9) =VX(I,3)  
            VY(I,9) =VY(I,3)  
            VZ(I,9) =VZ(I,3) 
           ENDIF

           IF(NVOISIN(3,L)<0)THEN
             IF(NVOISIN(4,L)<0)THEN
               ITRIV(2,I)=4
             ELSE
               ITRIV(2,I)=2
             ENDIF
           ELSEIF(NVOISIN(4,L)<0)THEN
               ITRIV(2,I)=3
           ELSE
               ITRIV(2,I)=1
           ENDIF

 
           IX=IABS(NVOISIN(5,L))  
           IXX(I,10)=IX  
           IF(IX /= 0)THEN 
            XX0(I,10)=X(1,IX) 
            YY0(I,10)=X(2,IX) 
            ZZ0(I,10)=X(3,IX) 
            VX(I,10)=V(1,IX) 
            VY(I,10)=V(2,IX) 
            VZ(I,10)=V(3,IX) 
           ELSE
            XX0(I,10)=XX0(I,3) 
            YY0(I,10)=YY0(I,3) 
            ZZ0(I,10)=ZZ0(I,3) 
            VX(I,10) =VX(I,3)  
            VY(I,10) =VY(I,3)  
            VZ(I,10) =VZ(I,3) 
           ENDIF
 
           IF(NVOISIN(6,L)/=0)IX=IABS(NVOISIN(6,L))
           IXX(I,11)=IX  
           IF(IX /= 0)THEN 
            XX0(I,11)=X(1,IX) 
            YY0(I,11)=X(2,IX) 
            ZZ0(I,11)=X(3,IX) 
            VX(I,11)=V(1,IX) 
            VY(I,11)=V(2,IX) 
            VZ(I,11)=V(3,IX)
           ELSE
            XX0(I,11)=XX0(I,4) 
            YY0(I,11)=YY0(I,4) 
            ZZ0(I,11)=ZZ0(I,4) 
            VX(I,11) =VX(I,4)  
            VY(I,11) =VY(I,4)  
            VZ(I,11) =VZ(I,4) 
           ENDIF

 
           IF(NVOISIN(5,L)<0)THEN
             IF(NVOISIN(6,L)<0)THEN
               ITRIV(3,I)=4
             ELSE
               ITRIV(3,I)=2
             ENDIF
           ELSEIF(NVOISIN(6,L)<0)THEN
               ITRIV(3,I)=3
           ELSE
               ITRIV(3,I)=1
           ENDIF

           IX=IABS(NVOISIN(7,L))  
           IXX(I,12)=IX  
           IF(IX /= 0)THEN 
            XX0(I,12)=X(1,IX) 
            YY0(I,12)=X(2,IX) 
            ZZ0(I,12)=X(3,IX) 
            VX(I,12)=V(1,IX) 
            VY(I,12)=V(2,IX) 
            VZ(I,12)=V(3,IX)
           ELSE
            XX0(I,12)=XX0(I,4) 
            YY0(I,12)=YY0(I,4) 
            ZZ0(I,12)=ZZ0(I,4) 
            VX(I,12) =VX(I,4)  
            VY(I,12) =VY(I,4)  
            VZ(I,12) =VZ(I,4) 
           ENDIF
 
           IF(NVOISIN(8,L)/=0)IX=IABS(NVOISIN(8,L))
           IXX(I,13)=IX  
           IF(IX /= 0)THEN 
            XX0(I,13)=X(1,IX)
            YY0(I,13)=X(2,IX)
            ZZ0(I,13)=X(3,IX)
            VX(I,13)=V(1,IX)
            VY(I,13)=V(2,IX)
            VZ(I,13)=V(3,IX)
           ELSE
            XX0(I,13)=XX0(I,1) 
            YY0(I,13)=YY0(I,1) 
            ZZ0(I,13)=ZZ0(I,1) 
            VX(I,13) =VX(I,1)  
            VY(I,13) =VY(I,1)  
            VZ(I,13) =VZ(I,1) 
           ENDIF

           IF(NVOISIN(7,L)<0)THEN
             IF(NVOISIN(8,L)<0)THEN
               ITRIV(4,I)=4
             ELSE
               ITRIV(4,I)=2
             ENDIF
           ELSEIF(NVOISIN(8,L)<0)THEN
               ITRIV(4,I)=3
           ELSE
               ITRIV(4,I)=1
           ENDIF

           IF(IXX(I, 6)==IXX(I, 7))THEN
              XX0(I,14) = XX0(I,6)
              YY0(I,14) = YY0(I,6)
              ZZ0(I,14) = ZZ0(I,6)
              VX(I,14) = VX(I,6)
              VY(I,14) = VY(I,6)
              VZ(I,14) = VZ(I,6)
           ELSE
              XX0(I,14) = FOURTH*(XX0(I,2)+XX0(I,1)+XX0(I,6)+XX0(I,7))
              YY0(I,14) = FOURTH*(YY0(I,2)+YY0(I,1)+YY0(I,6)+YY0(I,7))
              ZZ0(I,14) = FOURTH*(ZZ0(I,2)+ZZ0(I,1)+ZZ0(I,6)+ZZ0(I,7))
              VX(I,14) = FOURTH*(VX(I,2)+VX(I,1)+VX(I,6)+VX(I,7))
              VY(I,14) = FOURTH*(VY(I,2)+VY(I,1)+VY(I,6)+VY(I,7))
              VZ(I,14) = FOURTH*(VZ(I,2)+VZ(I,1)+VZ(I,6)+VZ(I,7))
           ENDIF
           IF(IXX(I, 8)==IXX(I, 9))THEN
              XX0(I,15) = XX0(I,8)
              YY0(I,15) = YY0(I,8)
              ZZ0(I,15) = ZZ0(I,8)
              VX(I,15) = VX(I,8)
              VY(I,15) = VY(I,8)
              VZ(I,15) = VZ(I,8)
           ELSE
              XX0(I,15) = FOURTH*(XX0(I,3)+XX0(I,2)+XX0(I,8)+XX0(I,9))
              YY0(I,15) = FOURTH*(YY0(I,3)+YY0(I,2)+YY0(I,8)+YY0(I,9))
              ZZ0(I,15) = FOURTH*(ZZ0(I,3)+ZZ0(I,2)+ZZ0(I,8)+ZZ0(I,9))
              VX(I,15) = FOURTH*(VX(I,3)+VX(I,2)+VX(I,8)+VX(I,9))
              VY(I,15) = FOURTH*(VY(I,3)+VY(I,2)+VY(I,8)+VY(I,9))
              VZ(I,15) = FOURTH*(VZ(I,3)+VZ(I,2)+VZ(I,8)+VZ(I,9))
           ENDIF
           IF(IXX(I,10)==IXX(I,11))THEN
              XX0(I,16) = XX0(I,10)
              YY0(I,16) = YY0(I,10)
              ZZ0(I,16) = ZZ0(I,10)
              VX(I,16) = VX(I,10)
              VY(I,16) = VY(I,10)
              VZ(I,16) = VZ(I,10)
           ELSE
              XX0(I,16) = FOURTH*(XX0(I,4)+XX0(I,3)+XX0(I,10)+XX0(I,11))
              YY0(I,16) = FOURTH*(YY0(I,4)+YY0(I,3)+YY0(I,10)+YY0(I,11))
              ZZ0(I,16) = FOURTH*(ZZ0(I,4)+ZZ0(I,3)+ZZ0(I,10)+ZZ0(I,11))
              VX(I,16) = FOURTH*(VX(I,4)+VX(I,3)+VX(I,10)+VX(I,11))
              VY(I,16) = FOURTH*(VY(I,4)+VY(I,3)+VY(I,10)+VY(I,11))
              VZ(I,16) = FOURTH*(VZ(I,4)+VZ(I,3)+VZ(I,10)+VZ(I,11))
           ENDIF
           IF(IXX(I,12)==IXX(I,13))THEN
              XX0(I,17) = XX0(I,12)
              YY0(I,17) = YY0(I,12)
              ZZ0(I,17) = ZZ0(I,12)
              VX(I,17) = VX(I,12)
              VY(I,17) = VY(I,12)
              VZ(I,17) = VZ(I,12)
           ELSE
              XX0(I,17) = FOURTH*(XX0(I,1)+XX0(I,4)+XX0(I,12)+XX0(I,13))
              YY0(I,17) = FOURTH*(YY0(I,1)+YY0(I,4)+YY0(I,12)+YY0(I,13))
              ZZ0(I,17) = FOURTH*(ZZ0(I,1)+ZZ0(I,4)+ZZ0(I,12)+ZZ0(I,13))
              VX(I,17) = FOURTH*(VX(I,1)+VX(I,4)+VX(I,12)+VX(I,13))
              VY(I,17) = FOURTH*(VY(I,1)+VY(I,4)+VY(I,12)+VY(I,13))
              VZ(I,17) = FOURTH*(VZ(I,1)+VZ(I,4)+VZ(I,12)+VZ(I,13))
           ENDIF

      END DO
      IF(IGSTI<=1)THEN
         DO I=1,JLT
          L  = IABS(CAND_E(I))
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=STF(L)*ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
          END IF
         ENDDO
      ELSEIF(IGSTI==2)THEN
         DO I=1,JLT
          L  = IABS(CAND_E(I))
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=HALF*(STF(L)+STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ELSEIF(IGSTI==3)THEN
         DO I=1,JLT
          L  = IABS(CAND_E(I))
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=MAX(STF(L),STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ELSEIF(IGSTI==4.OR.IGSTI==6)THEN
         DO I=1,JLT
          L  = IABS(CAND_E(I))
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=MIN(STF(L),STIF(I))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ELSEIF(IGSTI==5)THEN
         DO I=1,JLT
          L  = IABS(CAND_E(I))
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF(I)=ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF(I)=ABS(STIFI(NIN)%P(NN))
          END IF
          STIF(I)=STF(L)*STIF(I)/
     .            MAX(EM30,(STF(L)+STIF(I)))
          STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
         ENDDO
      ENDIF
C      
      RETURN
      END
C
Chd|====================================================================
Chd|  I24CORP3                      source/interfaces/int24/i24cork3.F
Chd|-- called by -----------
Chd|        I7FORCF3                      source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24CORP3(JLT   ,X     ,IRECT ,CAND_E  ,CAND_N  ,
     1                   STIF  ,H1    ,H2    ,H3      ,H4      ,
     2                   N1    ,N2    ,N3    ,IX1     ,IX2     ,
     3                   IX3   ,IX4   ,NSVG  ,VXI     ,VYI     ,
     4                   VZI   ,MSI   ,DXI   ,DYI     ,DZI     ,
     5                   NSN   ,NIN   ,JLT_NEW ,LREM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), CAND_E(*), CAND_N(*),
     .         JLT,JLT_NEW,NIN,NSN,LREM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
C     REAL
C     REAL
      my_real
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   X(3,*), STIF(*),N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,NI, L, NN, NS ,NE,IC(4,4),ITQ
      DATA IC / 
     1    3, 4, 1, 2, 
     2    4, 1, 2, 3,
     3    1, 2, 3, 4,
     4    2, 3, 4, 1/
C-----------------------------------------------
C
        DO I=1,JLT
         NI = CAND_N(I)
         L  = CAND_E(I)
         IF(NI>NSN)THEN
           NN = NI - NSN
           JLT_NEW = JLT_NEW + 1
           VXI(JLT_NEW)= VFI(NIN)%P(1,NN)
           VYI(JLT_NEW)= VFI(NIN)%P(2,NN)
           VZI(JLT_NEW)= VFI(NIN)%P(3,NN)
           MSI(JLT_NEW)= MSFI(NIN)%P(NN)
           ITQ = IRTLM_FI(NIN)%P(2,NN)
           IF (ITQ>4.OR.ITQ==0) THEN
            print *,'Intenal Error, ITQ=',ITQ
            CALL ARRET(2)
           END IF
           IX1(JLT_NEW)  = IRECT(IC(1,ITQ),L)
           IX2(JLT_NEW)  = IRECT(IC(2,ITQ),L)
           IX3(JLT_NEW)  = IRECT(IC(3,ITQ),L)
           IX4(JLT_NEW)  = IRECT(IC(4,ITQ),L)
           NS=IND_INT(NIN)%P(NN)
           NE=SHF_INT(NIN)+JLT_NEW+LREM
           NSVG(JLT_NEW) = NS
           H1(JLT_NEW)  = H_E(1,NE)
           H2(JLT_NEW)  = H_E(2,NE)
           H3(JLT_NEW)  = H_E(3,NE)
           H4(JLT_NEW)  = H_E(4,NE)
           STIF(JLT_NEW)=STIFS(NE)
           N1(JLT_NEW)=N_E(1,NE)
           N2(JLT_NEW)=N_E(2,NE)
           N3(JLT_NEW)=N_E(3,NE)
C----------displacement
           DXI(JLT_NEW)=DFI(1,NS)
           DYI(JLT_NEW)=DFI(2,NS)
           DZI(JLT_NEW)=DFI(3,NS)
C
         END IF
C
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  I24CORKM                      source/interfaces/int24/i24cork3.F
Chd|-- called by -----------
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|-- calls ---------------
Chd|        I24MSEGV1                     source/interfaces/int24/i24cork3.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24CORKM(
     1               JLT     ,X      ,IRECT ,NSV   ,CAND_E ,
     2               CAND_N  ,STIF   ,STIF_IMP,XI     ,YI    ,
     3               ZI      ,VXI    ,VYI     ,VZI    ,IX1   ,
     4               IX2     ,IX3    ,IX4     ,NSVG  ,NVOISIN,
     5               MS      ,MSI    ,NSN     ,V     ,NIN   , 
     6               N1      ,N2     ,N3     ,H1    ,H2      ,
     7               H3      ,H4     ,NJ_IMP ,HJ_IMP ,SUBTRIA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        JLT,NSN, NIN, NVOISIN(8,*),SUBTRIA(*)
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ), NSVG(MVSIZ)
C     REAL
      my_real
     .   X(3,*), STIF_IMP(*), MS(*), V(3,*),N1(*),N2(*),N3(*),
     .   H1(*),H2(*),H3(*),H4(*),NJ_IMP(3,*),HJ_IMP(4,*)
C     REAL
      my_real
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI,IRTLM(4),NE,NEI
C-----------------------------------------------
C
C
      DO I=1,JLT
           NI = CAND_N(I)
           NE = CAND_E(I)
           L  = IABS(NE)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
c             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
c             KINI(I) = KINFI(NIN)%P(NN)
             XI(I) = XFI(NIN)%P(1,NN)
             YI(I) = XFI(NIN)%P(2,NN)
             ZI(I) = XFI(NIN)%P(3,NN)
             VXI(I)= VFI(NIN)%P(1,NN)
             VYI(I)= VFI(NIN)%P(2,NN)
             VZI(I)= VFI(NIN)%P(3,NN)
             MSI(I)= MSFI(NIN)%P(NN)
           END IF
C
        IF (NE<0) THEN
         NEI=-NE
         CALL I24MSEGV1(IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
         IX1(I) = IRTLM(1)
         IX2(I) = IRTLM(2)
         IX3(I) = IRTLM(3)
         IX4(I) = IRTLM(4)
        ELSE
         IX1(I) = IRECT(1,L)
         IX2(I) = IRECT(2,L)
         IX3(I) = IRECT(3,L)
         IX4(I) = IRECT(4,L)
        END IF
        STIF(I) = STIF_IMP(I)
        N1(I) = NJ_IMP(1,I)  
        N2(I) = NJ_IMP(2,I)  
        N3(I) = NJ_IMP(3,I)  
        H1(I) = HJ_IMP(1,I)  
        H2(I) = HJ_IMP(2,I)  
        H3(I) = HJ_IMP(3,I)  
        H4(I) = HJ_IMP(4,I)  
      END DO
C      
      RETURN
      END
Chd|====================================================================
Chd|  I24MSEGV1                     source/interfaces/int24/i24cork3.F
Chd|-- called by -----------
Chd|        I24CORKM                      source/interfaces/int24/i24cork3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24MSEGV1(IRTLMV  ,SUBTRIA,IRTLM ,NVOISIN)
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTLMV(4),IRTLM(4),SUBTRIA,NVOISIN(8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX,IC(4,20),IXX(13),j
C-----------------------------------------------
C        11-------10       
C         |\ 19  /|        
C         | \   / |                                    
C         |  \ /  |           
C         |  16   |           
C         |15/ \11|        
C         | /   \ |      
C         |/  7  \|       
C12-------4-------3-------9
C |\ 12  /|\     /|\ 14  /|
C | \   / | \ 3 / | \   / |
C |  \ /  |  \ /2 |6 \ /18|
C |  17   |   5   |  15   |
C |20/ \ 8| 4/ \  |  / \  |
C | /   \ | / 1 \ | /   \ |
C |/ 16  \|/     \|/ 10  \|
C13-------1-------2-------8
C         |\  5  /|       
C         | \   / |      
C         |9 \ /13|    
C         |  14   |    
C         |  / \  |   
C         | /   \ |  
C         |/ 17  \| 
C         6-------7
C-----------------------------------------
      DATA IC / 
     1   3, 4, 1, 2, 
     2   4, 1, 2, 3,
     3   1, 2, 3, 4,
     4   2, 3, 4, 1,
     5   6, 7, 2, 1,
     6   8, 9, 3, 2,
     7  10,11, 4, 3,
     8  12,13, 1, 4,
     9   7, 2, 1, 6,
     .   9, 3, 2, 8,
     1  11, 4, 3,10,
     2  13, 1, 4,12,
     3   1, 6, 7, 2,
     4   2, 8, 9, 3,
     5   3,10,11, 4,
     6   4,12,13, 1,
     7   2, 1, 6, 7,
     8   3, 2, 8, 9,
     9   4, 3,10,11,
     .   1, 4,12,13/
C---------different than I24MSEGV(ind_glob_k.F), consisting order of IXJ w/ Hj    
           IXX(1:4)=IRTLM(1:4)  
           IX=IABS(NVOISIN(1))  
           IXX(6)=IX 
           IF(NVOISIN(2)/=0)IX=IABS(NVOISIN(2)) 
           IXX(7)=IX  
           IX=IABS(NVOISIN(3))  
           IXX(8)=IX  
           IF(NVOISIN(4)/=0)IX=IABS(NVOISIN(4)) 
           IXX(9)=IX  
           IX=IABS(NVOISIN(5))  
           IXX(10)=IX  
           IF(NVOISIN(6)/=0)IX=IABS(NVOISIN(6))
           IXX(11)=IX  
           IX=IABS(NVOISIN(7))  
           IXX(12)=IX   
           IF(NVOISIN(8)/=0)IX=IABS(NVOISIN(8))
           IXX(13)=IX 
C
        IRTLMV(1) = IXX(IC(1,SUBTRIA))
        IRTLMV(2) = IXX(IC(2,SUBTRIA))
        IRTLMV(3) = IXX(IC(3,SUBTRIA))
        IRTLMV(4) = IXX(IC(4,SUBTRIA))
C
       RETURN
      END
C
