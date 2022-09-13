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
Chd|  I24COR3                       source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/int24/i24for3e.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24COR3(
     1               JLT      ,X        ,IRECT   ,NSV     ,CAND_E   ,
     2               CAND_N   ,CAND_T   ,STF     ,STFN    ,STIF     ,
     3               XX0      ,YY0      ,ZZ0     ,VX      ,VY       ,
     5               VZ       ,XI       ,YI      ,ZI      ,VXI      ,
     7               VYI      ,VZI      ,IXX     ,NSVG    ,NVOISIN  ,
     9               MS       ,MSI      ,NSN     ,V       ,KINET    ,
     A               KINI     ,ITY      ,NIN     ,IGSTI   ,KMIN     ,
     B               KMAX     ,GAP_S    ,GAPS    ,NODNX_SMS,NSMS    ,
     C               ITRIV    ,XFIC     ,VFIC    ,MSF     ,IRTSE    ,
     D               IS2SE    ,IS2PT    ,ISEGPT  ,NSNE     , 
     E               IRTLM    ,NPT      ,NRTSE   ,IEDG4   ,ISPT2    , 
     F               ISPT2_LOC,INTFRIC  ,IPARTFRICS,IPARTFRICSI,
     G               IPARTFRICM,IPARTFRICMI,INTNITSCHE,FORNEQS,FORNEQSI,
     H               IORTHFRIC,IREP_FRICM,DIR_FRICM,IREP_FRICMI,DIR_FRICMI,
     I               IXX3     ,IXX4      , XX1     ,XX2   ,XX3       ,
     3               XX4      ,YY1       ,YY2      ,YY3   ,YY4       ,
     4               ZZ1      ,ZZ2       ,ZZ3      ,ZZ4   ,NINLOADP  ,
     5               DIST    ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE DEBUG_MOD
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
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),KINET(*),KINI(*),
     .        JLT,IDT, NOINT ,NDDIM, NSN, ITY, NIN, IGSTI,NRTSE,
     .        NVOISIN(8,*), NODNX_SMS(*), CAND_T(*),IRTLM(2,*),NPT,
     .        IEDG4,INTFRIC,INTNITSCHE ,IORTHFRIC 
      INTEGER IXX(MVSIZ,13), NSVG(MVSIZ), NSMS(MVSIZ),ITRIV(4,MVSIZ),
     .        IRTSE(5,*),IS2SE(2,*),IS2PT(*),ISEGPT(*),NSNE,ISPT2(NSN),
     *        ISPT2_LOC(MVSIZ),IPARTFRICS(*),IPARTFRICSI(MVSIZ),IPARTFRICM(*),
     .        IPARTFRICMI(MVSIZ),IREP_FRICM(*),IREP_FRICMI(MVSIZ),
     .        IXX3(MVSIZ),IXX4(MVSIZ)
           INTEGER  , INTENT(IN) :: NINLOADP
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
     .   KMIN, KMAX,XFIC(3,*),VFIC(3,*),MSF(*), FORNEQS(3,*),FORNEQSI(MVSIZ,3),
     .   DIR_FRICM(2,*) ,DIR_FRICMI(MVSIZ,2) ,
     .   XX1(MVSIZ), XX2(MVSIZ), XX3(MVSIZ), XX4(MVSIZ),
     .   YY1(MVSIZ), YY2(MVSIZ), YY3(MVSIZ), YY4(MVSIZ),
     .   ZZ1(MVSIZ), ZZ2(MVSIZ), ZZ3(MVSIZ), ZZ4(MVSIZ)
      my_real  , INTENT(INOUT) :: DIST(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI,IGF,IPT,IE,NS1,NS2,
     .        ICONT1,ICONT2,NSI,IPT2,NS
      my_real
     .        SX1, SY1, SZ1, SX2, SY2, SZ2, NORM
C-----------------------------------------------
C initiailisation
      DO I=1,JLT
        DO J=1,13
          IXX(I,J) = 0
        ENDDO
      ENDDO
C-initialize ISPT2_LOC (local or Remote)
       IF (IEDG4 > 0) THEN
         DO I=1,JLT
           NI = CAND_N(I)
           IF(NI<=NSN)THEN
              ISPT2_LOC(I) = ISPT2(NI)
           ELSE
             NN = NI - NSN
             ISPT2_LOC(I) =  ISPT2_FI(NIN)%P(NN)
           ENDIF
         ENDDO
       ELSE
         DO I=1,JLT
             ISPT2_LOC(I) = 0
         ENDDO
                  
       END IF !(IEDGE4 > 0) THEN
C      
      DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IG = NSV(NI)
             NSVG(I) = IG
C---------------voir KINET(IG) est initi   quand    
            IF (IG <= NUMNOD) THEN         
             KINI(I) = KINET(IG)
             XI(I) = X(1,IG)
             YI(I) = X(2,IG)
             ZI(I) = X(3,IG)
             VXI(I) = V(1,IG)
             VYI(I) = V(2,IG)
             VZI(I) = V(3,IG)
             MSI(I)= MS(IG)
            ELSE
             IGF = IG-NUMNOD
C--------KINI isn't used             
C             KINI(I) = KINETF(IGF)
             XI(I) = XFIC(1,IGF)
             YI(I) = XFIC(2,IGF)
             ZI(I) = XFIC(3,IGF)
             VXI(I) = VFIC(1,IGF)
             VYI(I) = VFIC(2,IGF)
             VZI(I) = VFIC(3,IGF)
             MSI(I)= MSF(IGF)
            END IF !(IG <= NUMNOD)
            GAPS(I) = GAP_S(NI)
           ELSE
             NN = NI - NSN
             NSVG(I) = -NN
             KINI(I) = KINFI(NIN)%P(NN)
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

           IF(IXX(I,6)==IXX(I,7))THEN
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
C	  
      IF(IGSTI<=1)THEN
         DO I=1,JLT
          L  = CAND_E(I)
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
          L  = CAND_E(I)
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
          L  = CAND_E(I)
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
          L  = CAND_E(I)
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
          L  = CAND_E(I)
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
c      DO I=1,JLT
c        IF(NSVG(I)>NUMNOD)THEN
c          NI = CAND_N(I)
c          IF (ISEGPT(NI)>0) THEN
c           STIF(I) = (ONE/NPT)*STIF(I)
c          ELSE
c           STIF(I) = ZEP3*STIF(I)
c          END IF
c        END IF
c      END DO !I=1,JLT
C----------
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(NSVG(I)>0)THEN
         IF (NSVG(I) <= NUMNOD) THEN 
          NN = NODNX_SMS(NSVG(I))
         ELSE
          NN = NSVG(I)-NUMNOD
          CALL I24FIC_GETN(NN      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     +                     NS2     )
          NN = MAX(NODNX_SMS(NS1),NODNX_SMS(NS2))
         END IF         
          NSMS(I)=      NN
     .                       +NODNX_SMS(IXX(I,1))+NODNX_SMS(IXX(I,2))
     .                       +NODNX_SMS(IXX(I,3))+NODNX_SMS(IXX(I,4))
        ELSE
          NN=-NSVG(I)
          NSMS(I)=NODNXFI(NIN)%P(NN)
     .                       +NODNX_SMS(IXX(I,1))+NODNX_SMS(IXX(I,2))
     .                       +NODNX_SMS(IXX(I,3))+NODNX_SMS(IXX(I,4))
        END IF
       ENDDO
       IF(IDTMINS_INT/=0)THEN
         DO I=1,JLT
          IF(NSMS(I)==0)NSMS(I)=-1
         ENDDO
       END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        DO I=1,JLT
         NSMS(I)=-1
        ENDDO
      ENDIF
         
C----Friction model : secnd part IDs---------
      IF(INTFRIC > 0) THEN
         DO I=1,JLT
           NI = CAND_N(I)
           L  = CAND_E(I)
           IF(NI<=NSN)THEN
             IPARTFRICSI(I)= IPARTFRICS(NI)
           ELSE
             NN = NI - NSN
             IPARTFRICSI(I)= IPARTFRICSFI(NIN)%P(NN)
           END IF
C
           IPARTFRICMI(I) = IPARTFRICM(L)

           IF(IORTHFRIC > 0) THEN
              IREP_FRICMI(I) =IREP_FRICM(L) 
              DIR_FRICMI(I,1:2)=DIR_FRICM(1:2,L) 
              IXX3(I) = IXX(I,3)
              IXX4(I) = IXX(I,4)
              XX1(I) = XX0(I,1)
              XX2(I) = XX0(I,2)
              XX3(I) = XX0(I,3)
              XX4(I) = XX0(I,4)
              YY1(I) = YY0(I,1)
              YY2(I) = YY0(I,2)
              YY3(I) = YY0(I,3)
              YY4(I) = YY0(I,4)
              ZZ1(I) = ZZ0(I,1)
              ZZ2(I) = ZZ0(I,2)
              ZZ3(I) = ZZ0(I,3)
              ZZ4(I) = ZZ0(I,4)
           ENDIF
         ENDDO
       ENDIF
C
C----Friction model : secnd part IDs---------
      IF(INTNITSCHE > 0) THEN
         DO I=1,JLT
           NI = CAND_N(I)
           IF(NI<=NSN)THEN
             IG = NSVG(I)
             FORNEQSI(I,1)= FORNEQS(1,IG)
             FORNEQSI(I,2)= FORNEQS(2,IG)
             FORNEQSI(I,3)= FORNEQS(3,IG)
           ELSE
             NN = NI - NSN
             FORNEQSI(I,1)= FORNEQSFI(NIN)%P(1,NN)
             FORNEQSI(I,2)= FORNEQSFI(NIN)%P(2,NN)
             FORNEQSI(I,3)= FORNEQSFI(NIN)%P(3,NN)
           END IF
         ENDDO
       ENDIF
C
      IF(NINLOADP > 0) THEN
C-----------------------------------------------
C         Distance between secnd node 
C              and main segment 
C-----------------------------------------------
        DO I=1,JLT 
C
           SX1=(YY0(I,1)-YY0(I,3))*(ZZ0(I,2)-ZZ0(I,4)) - (ZZ0(I,1)-ZZ0(I,3))*(YY0(I,2)-YY0(I,4))
           SY1=(ZZ0(I,1)-ZZ0(I,3))*(XX0(I,2)-XX0(I,4)) - (XX0(I,1)-XX0(I,3))*(ZZ0(I,2)-ZZ0(I,4))
           SZ1=(XX0(I,1)-XX0(I,3))*(YY0(I,2)-YY0(I,4)) - (YY0(I,1)-YY0(I,3))*(XX0(I,2)-XX0(I,4))
C         
           NORM = SQRT(SX1**2 + SY1**2 + SZ1**2)  
C
           IF(IXX(I,4)/=IXX(I,3))THEN
             SX2 = FOURTH*(XX0(I,1) + XX0(I,2) + XX0(I,3) + XX0(I,4)) - XI(I)    
             SY2 = FOURTH*(YY0(I,1) + YY0(I,2) + YY0(I,3) + YY0(I,4)) - YI(I)    
             SZ2 = FOURTH*(ZZ0(I,1) + ZZ0(I,2) + ZZ0(I,3) + ZZ0(I,4)) - ZI(I)  
           ELSE
             SX2 = THIRD*(XX0(I,1) + XX0(I,2) + XX0(I,3)) - XI(I)    
             SY2 = THIRD*(YY0(I,1) + YY0(I,2) + YY0(I,3)) - YI(I)    
             SZ2 = THIRD*(ZZ0(I,1) + ZZ0(I,2) + ZZ0(I,3)) - ZI(I)  
           END IF  
           DIST(I) = (SX2*SX1+SY2*SY1+SZ2*SZ1) / MAX(EM15,NORM)
           DIST(I) = ABS(DIST(I))

         ENDDO
C
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  I24INI_ISPT2                  source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24INI_ISPT2(
     1               JLT     ,NSV   ,CAND_N ,NSN   ,IRTSE  ,
     2               IS2SE   ,ISPT2 ,ISEGPT ,IRTLM ,NSNE   ,
     3               NRTSE   ,IEDG4 ) 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,IS2SE(2,*),JLT ,NSV(*),CAND_N(*) ,NSN   ,
     +        ISPT2(*) ,ISEGPT(*),IRTLM(2,*),NSNE   , NRTSE,
     +        IEDG4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- get edge NS1,NS2 and--Secnd seg id :IE-
      INTEGER ITAG(NRTSE),IE1,IE2
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI,IGF,IPT,IE,NS1,NS2,
     .        ICONT1,ICONT2,NSI,IPT2,NS,ip
C=======================================================================
C----IRTSE(5,*) -> id of edge  
C=======================================================================
C----ISEGPT(NI) : ISEGPT(NI)<0 internal node; ISEGPT(NI)=NI fic nodes on edge
C---  ISEGPT(NI)>0 (NI<=NSN0) : exatrem nodes on edge
C initiailisation
      IF (IEDG4==1) RETURN
        ITAG(1:NRTSE)=0
        DO I=1,JLT
           NI = CAND_N(I)
           NSI = -ISEGPT(NI)
C-----internal nodes-----           
           IF (NSI >0) THEN
             NS=NSV(NSI)-NUMNOD
             IE = IS2SE(1,NS)
             ITAG(IE) = NSI
           END IF
        END DO
C-initialize ISPT2(takes nodal normal or not), 
C---not set ISPT2=0 : when only one internal point is on contact  
        DO I=1,JLT
           NI = CAND_N(I)
           NSI = ISEGPT(NI)
           IF (NSI >0) THEN
            NS = NSV(NSI)-NUMNOD
            ICONT1 = 0
            ICONT2 = 0
            IE1 = IS2SE(1,NS)
C-----------one internal point  could be IE2>0,IE1=0          
            IF (IE1>0) THEN
             NN = ITAG(IE1)
             IF (NN > 0) ICONT1 = IRTLM(1,NN)
            END IF
C-----------second internal point (if exist)           
            IE2 = IS2SE(2,NS)
            IF (IE2>0) THEN
             NN = ITAG(IE2)
             IF (NN > 0) ICONT2 = IRTLM(1,NN)
            END IF
            IF ((ICONT1 /=0.AND.ICONT2 ==0).OR.
     +                      (ICONT2 /=0.AND.ICONT1 ==0)) THEN
             ISPT2(I) = 0
            ELSE
             ISPT2(I) = NSI
            END IF
C---------interal nodes            
           ELSEIF (NSI <0) THEN
             ISPT2(I) = NSI
           END IF
        END DO 
C      
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24ISPT2_INI                  source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24ISPT2_INI(
     1               JLT     ,NSV   ,CAND_N ,NSN   ,IRTSE  ,
     2               IS2SE   ,ISPT2 ,ISEGPT ,IRTLM ,NSNE   ,
     3               NRTSE   ,IEDG4 ,NIN) 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      USE TRI7BOX
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,IS2SE(2,*),JLT ,NSV(*),CAND_N(*) ,NSN   ,
     +        ISPT2(*) ,ISEGPT(*),IRTLM(2,*),NSNE   , NRTSE,
     +        IEDG4,NIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- get edge NS1,NS2 and--Secnd seg id :IE-
      INTEGER IE1,IE2
      INTEGER I ,J  ,IL, L, NN, IG,JFT, IX,  NI,IGF,IPT,IE,NS1,NS2,
     .        ICONT1,ICONT2,NSI,IPT2,NS,ip
C=======================================================================
C----IRTSE(5,*) -> id of edge  
C=======================================================================
C----ISEGPT(NI) : ISEGPT(NI)<0 internal node; ISEGPT(NI)=NI fic nodes on edge
C---  ISEGPT(NI)>0 (NI<=NSN0) : exatrem nodes on edge
C initiailisation
      IF (IEDG4==1) RETURN
C-initialize ISPT2(takes nodal normal or not), 
C---not set ISPT2=0 : when only one internal point is on contact  
        DO I=1,JLT
           NI = CAND_N(I)
           IF(NI <= NSN)THEN
             NSI = ISEGPT(NI)
             IF (NSI >0) THEN
              NS = NSV(NSI)-NUMNOD
              ICONT1 = 0
C-----------one internal point for SPMD reason ----   
               NN = NSI
               ICONT1 = IRTLM(1,NN)
              IF (ICONT1 /=0) THEN
	       ISPT2(I) = 0
              ELSE
               ISPT2(I) = NSI
              END IF
C---------interal nodes            
             ELSEIF (NSI <0) THEN
               ISPT2(I) = NSI
             END IF
           ELSE
             NSI=ISEGPT_FI(NIN)%P(NI-NSN)
             IF (NSI >0) THEN 
                ICONT1 = IRTLM_FI(NIN)%P(1,NSI)
                IF (ICONT1 /=0) THEN
                  ISPT2(I) = 0
                ELSE
                  ISPT2(I) = NSI
                ENDIF
             ELSE
                ISPT2(I) = NSI
             ENDIF       
           ENDIF
        END DO 
C      
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  I24ISPT2_INI_OPTTRI           source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|        I24OPTCD                      source/interfaces/intsort/i24optcd.F
Chd|-- calls ---------------
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24ISPT2_INI_OPTTRI(
     1               JFT,JLT     ,NSV   ,CAND_N ,NSN   ,IRTSE  ,
     2               IS2SE   ,ISPT2 ,ISEGPT ,IRTLM ,NSNE   ,
     3               NRTSE   ,IEDG4 ,NIN) 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      USE TRI7BOX
      USE DEBUG_MOD
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,IS2SE(2,*),JLT ,NSV(*),CAND_N(*) ,NSN   ,
     +        ISPT2(*) ,ISEGPT(*),IRTLM(2,*),NSNE   , NRTSE,
     +        IEDG4,NIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- get edge NS1,NS2 and--Secnd seg id :IE-
      INTEGER IE1,IE2
      INTEGER I ,J  ,IL, L, IG,JFT, IX,  NI,IGF,IPT,IE,NS1,NS2,
     .        ICONT1,ICONT2,NSI,IPT2,ip,SN
C=======================================================================
C----IRTSE(5,*) -> id of edge  
C=======================================================================
C----ISEGPT(NI) : ISEGPT(NI)<0 internal node; ISEGPT(NI)=NI fic nodes on edge
C---  ISEGPT(NI)>0 (NI<=NSN0) : exatrem nodes on edge
C initiailisation
      IF (IEDG4==1) RETURN
C-initialize ISPT2(takes nodal normal or not), 
C---not set ISPT2=0 : when only one internal point is on contact  
        DO NI=JFT,JLT
             NSI = ISEGPT(NI)
             SN = NSV(NI)
             IF (NSI >0) THEN
              ICONT1 = 0
C-----------one internal point for SPMD reason ----   
               ICONT1 = IRTLM(1,NSI)
              IF (ICONT1 /=0) THEN
	       ISPT2(NI) = 0
              ELSE
               ISPT2(NI) = 1
              END IF
C---------interal nodes            
             ELSEIF (NSI <0) THEN
               ISPT2(NI) = 1
             END IF
        END DO 
C      
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  I_CORPFIT3                    source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I_CORPFIT3(
     1               JLT      ,STF     ,STFN    ,STIF     ,NSN    ,
     2               CAND_E   ,CAND_N  ,NIN     ,IGSTI   ,KMIN     ,
     3               KMAX     ,INACTI  ,NCFIT   ,TNCY) 
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_E(*), CAND_N(*),JLT,NSN, NIN, IGSTI,NCFIT,INACTI
C     REAL
      my_real
     .   STF(*), STFN(*),STIF(*),KMIN,KMAX,TNCY
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,L, NN, IG,JFT, IX,  NI
      my_real
     .   STIF_S,STIF_R(MVSIZ),F_PFIT,FA,FB,FAB
C-----------------------------------------------
C--- fixing min(stif) with Inacti=-1
      IF (INACTI==-1)THEN
         DO I=1,JLT
          L  = CAND_E(I)
          NI = CAND_N(I)
          IF(NI<=NSN)THEN
            STIF_S =ABS(STFN(NI))
          ELSE
            NN = NI - NSN
            STIF_S =ABS(STIFI(NIN)%P(NN))
          END IF
c		  STIF_R(I) = MIN(STIF_S,STF(L))/MAX(EM20,STIF(I))
		  STIF(I) = MIN(STIF_S,STF(L))
         ENDDO
      END IF
      IF(NCFIT>0)THEN
         FA = MIN(ONE,THREE*TNCY)
         FAB= MAX(ZERO,THREE*TNCY-ONE)
         FB = MAX(ZERO,THREE*TNCY-TWO)
         F_PFIT = EM04*(FA+FAB)+FB
         STIF(1:JLT)=F_PFIT*STIF(1:JLT)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  I_COR_EPFIT3                  source/interfaces/int24/i24cor3.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I_COR_EPFIT3(
     1               JLT      ,STFE    ,STIF    ,CAND_S ,CAND_M ,
     3               NEDGE    ,NIN     ,INACTI  ,NCFIT   ,TNCY) 
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
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(MVSIZ),INTENT(IN):: CAND_S, CAND_M
      INTEGER JLT,NIN,NCFIT,INACTI,NEDGE
C     REAL
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF
      my_real, DIMENSION(NEDGE), INTENT(IN) :: STFE
      my_real, INTENT(IN) :: TNCY
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,EM,ES, NN, IG,JFT, IX,  NI
      my_real
     .   STIF_S,STIF_R(MVSIZ),F_PFIT,FA,FB,FAB
C-----------------------------------------------
C--- fixing min(stif) with Inacti=-1
      IF (INACTI==-1)THEN
         DO I=1,JLT
          EM = CAND_M(I)
          NI = CAND_S(I)
          IF(NI<=NEDGE)THEN
            ES =NI
            STIF_S =STFE(ES)
          ELSE
            NN = NI - NEDGE
            STIF_S =STIFIE(NIN)%P(NN)
          END IF
c		  STIF_R(I) = MIN(STIF_S,STFE(EM))/MAX(EM20,STIF(I))
		  STIF(I) = MIN(STIF_S,STFE(EM))
         ENDDO
      END IF
      IF(NCFIT>0)THEN
         FA = MIN(ONE,THREE*TNCY)
         FAB= MAX(ZERO,THREE*TNCY-ONE)
         FB = MAX(ZERO,THREE*TNCY-TWO)
         F_PFIT = EM04*(FA+FAB)+FB
         STIF(1:JLT)=F_PFIT*STIF(1:JLT)
      END IF
C
      RETURN
      END
