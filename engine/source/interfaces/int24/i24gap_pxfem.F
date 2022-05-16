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
Chd|  I24GAP_PXFEM                  source/interfaces/int24/i24gap_pxfem.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24GAP_PXFEM(
     1                  NRTM     ,IRECT,CAND_E ,GAP_NM , 
     2                  MVOISIN ,NVOISIN , MSEGTYP ,INOD_PXFEM ,
     3                  X       ,MS_PLY  , WAGAP   ,ITAB,
     .                  ISEG_PXFEM,ISEG_PLY, STFM) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
C-------------------------------------------f----
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, CAND_E(*),MVOISIN(4,*), INOD_PXFEM(*),
     .        MSEGTYP(*),ITAB(*),NVOISIN(8,*),IRECT(4,*),
     .        ISEG_PXFEM(*),ISEG_PLY(12,*)
      my_real
     .     GAP_NM(12,*),WAGAP(2,*),MS_PLY(NPLYXFE,*),X(3,*),
     .     STFM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITAG(MVSIZ),ISEG(NRTM), IXX(NRTM,13),INDX(NRTM)
      INTEGER I,J1,J2,J3,J4,J5,J6,NN,ILY,IJ6,J7,J8,J9,J10,J11,J12,J,IX,
     .        IDX, NINDX,II
      my_real
     .     XO16, XO17, XO15, XO14, X163, X153, X152, X142, X141, 
     .     YO16, YO17, YO15, YO14, Y163, Y153, Y152, Y142, Y141, 
     .     ZO16, ZO17, ZO15, ZO14, Z163, Z153, Z152, Z142, Z141,
     .     X51 , Y51,Z51,X52,Y52,Z52,X53,Y53,Z53,X54,Y54,Z54  ,
     .     X164,Y164,Z164,X174,Y174,Z174,X171,Y171,Z171,NX1,
     .     NY1,NZ1,AAA,GAPN,NX2,NY2,NZ2,NX3,NY3,NZ3,DX,DY,DZ,
     .     NX4,NY4,NZ4,
     .     SX125(NRTM),SX235(NRTM),SX345(NRTM),SX415(NRTM),
     .     SY125(NRTM),SY235(NRTM),SY345(NRTM),SY415(NRTM),
     .     SZ125(NRTM),SZ235(NRTM),SZ345(NRTM),SZ415(NRTM),
     .     SX1250(NRTM),SX2350(NRTM),SX3450(NRTM),SX4150(NRTM),
     .     SY1250(NRTM),SY2350(NRTM),SY3450(NRTM),SY4150(NRTM),
     .     SZ1250(NRTM),SZ2350(NRTM),SZ3450(NRTM),SZ4150(NRTM),
     .     SX2114(NRTM),SX3215(NRTM),SX4316(NRTM),SX1417(NRTM),
     .     SY2114(NRTM),SY3215(NRTM),SY4316(NRTM),SY1417(NRTM),
     .     SZ2114(NRTM),SZ3215(NRTM),SZ4316(NRTM),SZ1417(NRTM),
     .     SX21140(NRTM),SX32150(NRTM),SX43160(NRTM),SX14170(NRTM),
     .     SY21140(NRTM),SY32150(NRTM),SY43160(NRTM),SY14170(NRTM),
     .     SZ21140(NRTM),SZ32150(NRTM),SZ43160(NRTM),SZ14170(NRTM),
     .     NXX(NRTM,17),NYY(NRTM,17),NZZ(NRTM,17),BBB,
     .     XX0(NRTM,17),YY0(NRTM,17),ZZ0(NRTM,17),DN
C --------------------------------------------------------------------- 
C  all cordinate of segment
C
C initiailisation
      NINDX =0
      DO I=1,NRTM
        ISEG(I) = 0
        DO J=1,13
          IXX(I,J) = 0
        ENDDO
         IF(MSEGTYP(I) > 0) ISEG(I) = 1 
         IF(MSEGTYP(I) < 0) ISEG(I) = 2
         IF(ISEG_PXFEM(I) == 0 .OR. STFM(I) == ZERO) CYCLE 
         NINDX = NINDX + 1
         INDX(NINDX) = I 
      ENDDO
C

C        
      DO II=1,NINDX
         I = INDX(II) 
C
           IX=IRECT(1,I)  
           IXX(I,1)=IX      
           XX0(I,1)=X(1,IX)  
           YY0(I,1)=X(2,IX)  
           ZZ0(I,1)=X(3,IX) 
C
           IX=IRECT(2,I)  
           IXX(I,2)=IX      
           XX0(I,2)=X(1,IX)  
           YY0(I,2)=X(2,IX)  
           ZZ0(I,2)=X(3,IX) 
C
           IX=IRECT(3,I)  
           IXX(I,3)=IX      
           XX0(I,3)=X(1,IX)  
           YY0(I,3)=X(2,IX)  
           ZZ0(I,3)=X(3,IX) 
C
           IX=IRECT(4,I)  
           IXX(I,4)=IX      
           XX0(I,4)=X(1,IX)  
           YY0(I,4)=X(2,IX)  
           ZZ0(I,4)=X(3,IX)
C
           IF(IXX(I,3) /= IXX(I,4))THEN
            XX0(I,5) = QUART*(XX0(I,1)+XX0(I,2)+XX0(I,3)+XX0(I,4))
            YY0(I,5) = QUART*(YY0(I,1)+YY0(I,2)+YY0(I,3)+YY0(I,4))
            ZZ0(I,5) = QUART*(ZZ0(I,1)+ZZ0(I,2)+ZZ0(I,3)+ZZ0(I,4))
           ELSE
            XX0(I,5) = XX0(I,3)
            YY0(I,5) = YY0(I,3)
            ZZ0(I,5) = ZZ0(I,3) 
           ENDIF

           IX=IABS(NVOISIN(1,I))  
           IXX(I,6)=IX 
           IF(IX /= 0)THEN 
            XX0(I,6)=X(1,IX) 
            YY0(I,6)=X(2,IX) 
            ZZ0(I,6)=X(3,IX) 
           ELSE
            XX0(I,6)=XX0(I,1) 
            YY0(I,6)=YY0(I,1) 
            ZZ0(I,6)=ZZ0(I,1)
           ENDIF
 
           IF(NVOISIN(2,I)/=0)IX=IABS(NVOISIN(2,I)) 
           IXX(I,7)=IX  
           IF(IX /= 0)THEN 
            XX0(I,7)=X(1,IX) 
            YY0(I,7)=X(2,IX) 
            ZZ0(I,7)=X(3,IX)
           ELSE
            XX0(I,7)=XX0(I,2) 
            YY0(I,7)=YY0(I,2) 
            ZZ0(I,7)=ZZ0(I,2)
           ENDIF
           IX=IABS(NVOISIN(3,I))  
           IXX(I,8)=IX  
           IF(IX /= 0)THEN 
            XX0(I,8)=X(1,IX) 
            YY0(I,8)=X(2,IX) 
            ZZ0(I,8)=X(3,IX)
           ELSE
            XX0(I,8)=XX0(I,2) 
            YY0(I,8)=YY0(I,2) 
            ZZ0(I,8)=ZZ0(I,2) 
           ENDIF
 
           IF(NVOISIN(4,I)/=0)IX=IABS(NVOISIN(4,I)) 
           IXX(I,9)=IX  
           IF(IX /= 0)THEN 
            XX0(I,9)=X(1,IX) 
            YY0(I,9)=X(2,IX) 
            ZZ0(I,9)=X(3,IX) 
           ELSE
            XX0(I,9)=XX0(I,3) 
            YY0(I,9)=YY0(I,3) 
            ZZ0(I,9)=ZZ0(I,3) 
           ENDIF
!
           IX=IABS(NVOISIN(5,I))  
           IXX(I,10)=IX  
           IF(IX /= 0)THEN 
            XX0(I,10)=X(1,IX) 
            YY0(I,10)=X(2,IX) 
            ZZ0(I,10)=X(3,IX)
           ELSE
            XX0(I,10)=XX0(I,3) 
            YY0(I,10)=YY0(I,3) 
            ZZ0(I,10)=ZZ0(I,3) 
           ENDIF
 
           IF(NVOISIN(6,I)/=0)IX=IABS(NVOISIN(6,I))
           IXX(I,11)=IX  
           IF(IX /= 0)THEN 
            XX0(I,11)=X(1,IX) 
            YY0(I,11)=X(2,IX) 
            ZZ0(I,11)=X(3,IX)
           ELSE
            XX0(I,11)=XX0(I,4) 
            YY0(I,11)=YY0(I,4) 
            ZZ0(I,11)=ZZ0(I,4) 
           ENDIF
!
           IX=IABS(NVOISIN(7,I))  
           IXX(I,12)=IX  
           IF(IX /= 0)THEN 
            XX0(I,12)=X(1,IX) 
            YY0(I,12)=X(2,IX) 
            ZZ0(I,12)=X(3,IX)
           ELSE
            XX0(I,12)=XX0(I,4) 
            YY0(I,12)=YY0(I,4) 
            ZZ0(I,12)=ZZ0(I,4) 
           ENDIF
 
           IF(NVOISIN(8,I)/=0)IX=IABS(NVOISIN(8,I))
           IXX(I,13)=IX  
           IF(IX /= 0)THEN 
            XX0(I,13)=X(1,IX)
            YY0(I,13)=X(2,IX)
            ZZ0(I,13)=X(3,IX)
           ELSE
            XX0(I,13)=XX0(I,1) 
            YY0(I,13)=YY0(I,1) 
            ZZ0(I,13)=ZZ0(I,1)
           ENDIF
!!
           IF(IXX(I, 6)==IXX(I, 7))THEN
              XX0(I,14) = XX0(I,6)
              YY0(I,14) = YY0(I,6)
              ZZ0(I,14) = ZZ0(I,6)
           ELSE
              XX0(I,14) = QUART*(XX0(I,2)+XX0(I,1)+XX0(I,6)+XX0(I,7))
              YY0(I,14) = QUART*(YY0(I,2)+YY0(I,1)+YY0(I,6)+YY0(I,7))
              ZZ0(I,14) = QUART*(ZZ0(I,2)+ZZ0(I,1)+ZZ0(I,6)+ZZ0(I,7))
           ENDIF
           IF(IXX(I, 8)==IXX(I, 9))THEN
              XX0(I,15) = XX0(I,8)
              YY0(I,15) = YY0(I,8)
              ZZ0(I,15) = ZZ0(I,8)
           ELSE
              XX0(I,15) = QUART*(XX0(I,3)+XX0(I,2)+XX0(I,8)+XX0(I,9))
              YY0(I,15) = QUART*(YY0(I,3)+YY0(I,2)+YY0(I,8)+YY0(I,9))
              ZZ0(I,15) = QUART*(ZZ0(I,3)+ZZ0(I,2)+ZZ0(I,8)+ZZ0(I,9))
           ENDIF
           IF(IXX(I,10)==IXX(I,11))THEN
              XX0(I,16) = XX0(I,10)
              YY0(I,16) = YY0(I,10)
              ZZ0(I,16) = ZZ0(I,10)
           ELSE
              XX0(I,16) = QUART*(XX0(I,4)+XX0(I,3)+XX0(I,10)+XX0(I,11))
              YY0(I,16) = QUART*(YY0(I,4)+YY0(I,3)+YY0(I,10)+YY0(I,11))
              ZZ0(I,16) = QUART*(ZZ0(I,4)+ZZ0(I,3)+ZZ0(I,10)+ZZ0(I,11))
           ENDIF
           IF(IXX(I,12)==IXX(I,13))THEN
              XX0(I,17) = XX0(I,12)
              YY0(I,17) = YY0(I,12)
              ZZ0(I,17) = ZZ0(I,12)
           ELSE
              XX0(I,17) = QUART*(XX0(I,1)+XX0(I,4)+XX0(I,12)+XX0(I,13))
              YY0(I,17) = QUART*(YY0(I,1)+YY0(I,4)+YY0(I,12)+YY0(I,13))
              ZZ0(I,17) = QUART*(ZZ0(I,1)+ZZ0(I,4)+ZZ0(I,12)+ZZ0(I,13))
           ENDIF
      END DO     
C---------------------------
C
C        
C       
C--------------------------------------------------------
C
C  for update of gap when main segement is ply-xfem formulation
C           <
         DO II=1,NINDX
          I = INDX(II) 
          
          XO14 = XX0(I,14)
          YO14 = YY0(I,14)
          ZO14 = ZZ0(I,14)

          XO15 = XX0(I,15)
          YO15 = YY0(I,15)
          ZO15 = ZZ0(I,15)

          XO16 = XX0(I,16)
          YO16 = YY0(I,16)
          ZO16 = ZZ0(I,16)

          XO17 = XX0(I,17)
          YO17 = YY0(I,17)
          ZO17 = ZZ0(I,17)

          X51 = XX0(I,1) - XX0(I,5)
          Y51 = YY0(I,1) - YY0(I,5)
          Z51 = ZZ0(I,1) - ZZ0(I,5)
 
          X52 = XX0(I,2) - XX0(I,5)
          Y52 = YY0(I,2) - YY0(I,5)
          Z52 = ZZ0(I,2) - ZZ0(I,5)
 
          X53 = XX0(I,3) - XX0(I,5)
          Y53 = YY0(I,3) - YY0(I,5)
          Z53 = ZZ0(I,3) - ZZ0(I,5)
 
          X54 = XX0(I,4) - XX0(I,5)
          Y54 = YY0(I,4) - YY0(I,5)
          Z54 = ZZ0(I,4) - ZZ0(I,5)
 

          SX1250(I) = Y51*Z52 - Z51*Y52
          SY1250(I) = Z51*X52 - X51*Z52
          SZ1250(I) = X51*Y52 - Y51*X52

          SX2350(I) = Y52*Z53 - Z52*Y53
          SY2350(I) = Z52*X53 - X52*Z53
          SZ2350(I) = X52*Y53 - Y52*X53

          SX3450(I) = Y53*Z54 - Z53*Y54
          SY3450(I) = Z53*X54 - X53*Z54
          SZ3450(I) = X53*Y54 - Y53*X54

          SX4150(I) = Y54*Z51 - Z54*Y51
          SY4150(I) = Z54*X51 - X54*Z51
          SZ4150(I) = X54*Y51 - Y54*X51


          X141 = XX0(I,1) - XX0(I,14)
          Y141 = YY0(I,1) - YY0(I,14)
          Z141 = ZZ0(I,1) - ZZ0(I,14)
 
          X142 = XX0(I,2) - XX0(I,14)
          Y142 = YY0(I,2) - YY0(I,14)
          Z142 = ZZ0(I,2) - ZZ0(I,14)
 
          X152 = XX0(I,2) - XX0(I,15)
          Y152 = YY0(I,2) - YY0(I,15)
          Z152 = ZZ0(I,2) - ZZ0(I,15)
 
          X153 = XX0(I,3) - XX0(I,15)
          Y153 = YY0(I,3) - YY0(I,15)
          Z153 = ZZ0(I,3) - ZZ0(I,15)
 
          X163 = XX0(I,3) - XX0(I,16)
          Y163 = YY0(I,3) - YY0(I,16)
          Z163 = ZZ0(I,3) - ZZ0(I,16)
 
          X164 = XX0(I,4) - XX0(I,16)
          Y164 = YY0(I,4) - YY0(I,16)
          Z164 = ZZ0(I,4) - ZZ0(I,16)

          X174 = XX0(I,4) - XX0(I,17)
          Y174 = YY0(I,4) - YY0(I,17)
          Z174 = ZZ0(I,4) - ZZ0(I,17)

          X171 = XX0(I,1) - XX0(I,17)
          Y171 = YY0(I,1) - YY0(I,17)
          Z171 = ZZ0(I,1) - ZZ0(I,17)
c         
          IF(MVOISIN(1,I)/=0)THEN
            SX21140(I) = Y142*Z141 - Z142*Y141
            SY21140(I) = Z142*X141 - X142*Z141
            SZ21140(I) = X142*Y141 - Y142*X141
          ELSE
            SX21140(I) = SX1250(I)
            SY21140(I) = SY1250(I)
            SZ21140(I) = SZ1250(I)
          ENDIF

          IF(MVOISIN(2,I)/=0)THEN
            SX32150(I) = Y153*Z152 - Z153*Y152
            SY32150(I) = Z153*X152 - X153*Z152
            SZ32150(I) = X153*Y152 - Y153*X152
          ELSE
            SX32150(I) = SX2350(I)
            SY32150(I) = SY2350(I)
            SZ32150(I) = SZ2350(I)
          ENDIF

          IF(MVOISIN(3,I)/=0)THEN
            SX43160(I) = Y164*Z163 - Z164*Y163
            SY43160(I) = Z164*X163 - X164*Z163
            SZ43160(I) = X164*Y163 - Y164*X163
          ELSE
            SX43160(I) = SX3450(I)
            SY43160(I) = SY3450(I)
            SZ43160(I) = SZ3450(I)
          ENDIF

          IF(MVOISIN(4,I)/=0)THEN
            SX14170(I) = Y171*Z174 - Z171*Y174
            SY14170(I) = Z171*X174 - X171*Z174
            SZ14170(I) = X171*Y174 - Y171*X174
          ELSE
            SX14170(I) = SX4150(I)
            SY14170(I) = SY4150(I)
            SZ14170(I) = SZ4150(I)
          ENDIF
C
          IF(IXX(I,3) /= IXX(I,4))THEN

            NXX(I,1) =SX4150(I)+SX1250(I)+DEUX*(SX14170(I)+SX21140(I))
            NYY(I,1) =SY4150(I)+SY1250(I)+DEUX*(SY14170(I)+SY21140(I))
            NZZ(I,1) =SZ4150(I)+SZ1250(I)+DEUX*(SZ14170(I)+SZ21140(I))

            NXX(I,2) =SX1250(I)+SX2350(I)+DEUX*(SX21140(I)+SX32150(I))
            NYY(I,2) =SY1250(I)+SY2350(I)+DEUX*(SY21140(I)+SY32150(I))
            NZZ(I,2) =SZ1250(I)+SZ2350(I)+DEUX*(SZ21140(I)+SZ32150(I))

            NXX(I,3) =SX2350(I)+SX3450(I)+DEUX*(SX32150(I)+SX43160(I))
            NYY(I,3) =SY2350(I)+SY3450(I)+DEUX*(SY32150(I)+SY43160(I))
            NZZ(I,3) =SZ2350(I)+SZ3450(I)+DEUX*(SZ32150(I)+SZ43160(I))

            NXX(I,4) =SX3450(I)+SX4150(I)+DEUX*(SX43160(I)+SX14170(I))
            NYY(I,4) =SY3450(I)+SY4150(I)+DEUX*(SY43160(I)+SY14170(I))
            NZZ(I,4) =SZ3450(I)+SZ4150(I)+DEUX*(SZ43160(I)+SZ14170(I))

            NXX(I,5) =SX1250(I)+SX2350(I)+SX3450(I)+SX4150(I)
            NYY(I,5) =SY1250(I)+SY2350(I)+SY3450(I)+SY4150(I)
            NZZ(I,5) =SZ1250(I)+SZ2350(I)+SZ3450(I)+SZ4150(I)

          ELSE

            NXX(I,1) =SX1250(I)+SX14170(I)+SX21140(I)
            NYY(I,1) =SY1250(I)+SY14170(I)+SY21140(I)
            NZZ(I,1) =SZ1250(I)+SZ14170(I)+SZ21140(I)

            NXX(I,2) =SX1250(I)+SX21140(I)+SX32150(I)
            NYY(I,2) =SY1250(I)+SY21140(I)+SY32150(I)
            NZZ(I,2) =SZ1250(I)+SZ21140(I)+SZ32150(I)

            NXX(I,3) =SX1250(I)+SX32150(I)+SX14170(I)
            NYY(I,3) =SY1250(I)+SY32150(I)+SY14170(I)
            NZZ(I,3) =SZ1250(I)+SZ32150(I)+SZ14170(I)

            NXX(I,4) =NXX(I,3)
            NYY(I,4) =NYY(I,3)
            NZZ(I,4) =NZZ(I,3)

            NXX(I,5) =NXX(I,3)
            NYY(I,5) =NYY(I,3)
            NZZ(I,5) =NZZ(I,3)

          ENDIF
C
c          GAP : update
C                      
            NX1 = SX1250(I) + SX21140(I) + SX4150(I) + SX14170(I)
            NY1 = SY1250(I) + SY21140(I) + SY4150(I) + SY14170(I)
            NZ1 = SZ1250(I) + SZ21140(I) + SZ4150(I) + SZ14170(I)
            AAA = UN/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)
            NX1 = NX1 * AAA
            NY1 = NY1 * AAA
            NZ1 = NZ1 * AAA 
C            
            J1= IXX(I,1)
            IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1) 
               ILY = ISEG_PLY(1,I)
              
               GAPN  =  GAP_NM(1,I)
               IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*NX1
     .              + PLY(ILY)%U(2,NN)*NY1
     .              + PLY(ILY)%U(3,NN)*NZ1
                 GAPN = MAX(ZERO,GAP_NM(1,I)+DN)
               ENDIF 
#include "lockon.inc"              
             IF(ISEG(I)>0 )WAGAP(ISEG(I),J1)=MAX(WAGAP(ISEG(I),J1),GAPN)
#include "lockoff.inc"           
            ENDIF 
C            
            NX2 = SX2350(I) + SX32150(I) + SX1250(I) + SX21140(I)
            NY2 = SY2350(I) + SY32150(I) + SY1250(I) + SY21140(I)
            NZ2 = SZ2350(I) + SZ32150(I) + SZ1250(I) + SZ21140(I)
            AAA = UN/SQRT(NX2*NX2+NY2*NY2+NZ2*NZ2)
            NX2 = NX2 * AAA
            NY2 = NY2 * AAA
            NZ2 = NZ2 * AAA
C            
            J2= IXX(I,2) 
            IF(J2 > 0) THEN
              NN = INOD_PXFEM(J2) 
              ILY = ISEG_PLY(2,I)
              GAPN  =  GAP_NM(2,I)                   
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*NX2
     .              + PLY(ILY)%U(2,NN)*NY2
     .              + PLY(ILY)%U(3,NN)*NZ2
                 GAPN = MAX(ZERO,GAP_NM(2,I)+DN)
              ENDIF
#include "lockon.inc"              
            IF(ISEG(I)>0)WAGAP(ISEG(I),J2)=MAX(WAGAP(ISEG(I),J2),GAPN)
#include "lockoff.inc"
           ENDIF
            IF(IXX(I,3) /= IXX(I,4))THEN
              NX3 = SX3450(I) + SX43160(I) + SX2350(I) + SX32150(I)
              NY3 = SY3450(I) + SY43160(I) + SY2350(I) + SY32150(I)
              NZ3 = SZ3450(I) + SZ43160(I) + SZ2350(I) + SZ32150(I)
              AAA = UN/SQRT(NX3*NX3+NY3*NY3+NZ3*NZ3)
              NX3 = NX3 * AAA
              NY3 = NY3 * AAA
              NZ3 = NZ3 * AAA
c              
              J3= IXX(I,3)
              
              IF(J3 > 0 ) THEN
                 NN = INOD_PXFEM(J3) 
                 ILY = ISEG_PLY(3,I)
                 GAPN  =  GAP_NM(3,I)                      
                 IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*NX3
     .              + PLY(ILY)%U(2,NN)*NY3
     .              + PLY(ILY)%U(3,NN)*NZ3
                 GAPN = MAX(ZERO,GAP_NM(3,I)+DN)
                 ENDIF
#include "lockon.inc"              
              IF(ISEG(I)>0)WAGAP(ISEG(I),J3)=MAX(WAGAP(ISEG(I),J3),GAPN)
#include "lockoff.inc"              
              ENDIF
C             
              NX4 = SX4150(I) + SX14170(I) + SX3450(I) + SX43160(I)
              NY4 = SY4150(I) + SY14170(I) + SY3450(I) + SY43160(I)
              NZ4 = SZ4150(I) + SZ14170(I) + SZ3450(I) + SZ43160(I)
              AAA = UN/SQRT(NX4*NX4+NY4*NY4+NZ4*NZ4)
              NX4 = NX4 * AAA
              NY4 = NY4 * AAA
              NZ4 = NZ4 * AAA
C
              J4 = IXX(I,4)
              IF(J4  > 0) THEN
                  NN = INOD_PXFEM(J4) 
                  ILY = ISEG_PLY(4,I)
                  GAPN  =  GAP_NM(4,I)                        
                  IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*NX4
     .              + PLY(ILY)%U(2,NN)*NY4
     .              + PLY(ILY)%U(3,NN)*NZ4
                 GAPN = MAX(ZERO,GAP_NM(4,I)+DN)
                  ENDIF
#include "lockon.inc"              
             IF(ISEG(I)>0)WAGAP(ISEG(I),J4)=MAX(WAGAP(ISEG(I),J4),GAPN)
#include "lockoff.inc"
             ENDIF
            ELSE
              NX3 = SX1250(I) + SX32150(I) + SX14170(I)
              NY3 = SY1250(I) + SY32150(I) + SY14170(I)
              NZ3 = SZ1250(I) + SZ32150(I) + SZ14170(I)
              AAA = UN/SQRT(NX3*NX3+NY3*NY3+NZ3*NZ3)
              NX3 = NX3 * AAA
              NY3 = NY3 * AAA
              NZ3 = NZ3 * AAA

              NX4 = NX3
              NY4 = NY3
              NZ4 = NZ3
C              
              J3= IXX(I,3)
              IF(J3 > 0) THEN
                 NN = INOD_PXFEM(J3) 
                 ILY = ISEG_PLY(3,I)
                 GAPN  =  GAP_NM(3,I)                      
                 IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*NX3
     .              + PLY(ILY)%U(2,NN)*NY3
     .              + PLY(ILY)%U(3,NN)*NZ3
                 GAPN = MAX(ZERO,GAP_NM(3,I)+DN)
                 ENDIF
#include "lockon.inc"              
              IF(ISEG(I)>0)WAGAP(ISEG(I),J3)=MAX(WAGAP(ISEG(I),J3),GAPN)
#include "lockoff.inc" 
              ENDIF
            ENDIF
C!!!      
             BBB = SQRT(SX21140(I)*SX21140(I)
     +                 +SY21140(I)*SY21140(I)
     +                  +SZ21140(I)*SZ21140(I))
             AAA = UN/BBB
             SX21140(I)=SX21140(I) * AAA
             SY21140(I)=SY21140(I) * AAA
             SZ21140(I)=SZ21140(I) * AAA
C            
             J5= IXX(I,6)
             IF(J5 > 0) THEN
               NN = INOD_PXFEM(J5) 
               ILY = ISEG_PLY(5,I)
               GAPN  =  GAP_NM(5,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX21140(I)
     .              + PLY(ILY)%U(2,NN)*SY21140(I)
     .              + PLY(ILY)%U(3,NN)*SZ21140(I)
                 GAPN = MAX(ZERO,GAP_NM(5,I)+DN)
              ENDIF 
#include "lockon.inc"              
               IF(ISEG(I)>0 )WAGAP(ISEG(I),J5)=MAX(WAGAP(ISEG(I),J5),GAPN)
#include "lockoff.inc"
            ENDIF
             
C            
            J6= IXX(I,7)
            IF(J6 > 0) THEN
              NN = INOD_PXFEM(J6) 
              ILY = ISEG_PLY(6,I)
              GAPN  =  GAP_NM(6,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX21140(I)
     .              + PLY(ILY)%U(2,NN)*SY21140(I)
     .              + PLY(ILY)%U(3,NN)*SZ21140(I)
                 GAPN = MAX(ZERO,GAP_NM(6,I)+DN)
              ENDIF 
#include "lockon.inc"              
               IF(ISEG(I)>0 )WAGAP(ISEG(I),J6)=MAX(WAGAP(ISEG(I),J6),GAPN)
#include "lockoff.inc"
            ENDIF
            BBB = SQRT(SX32150(I)*SX32150(I)
     +               +SY32150(I)*SY32150(I)
     +               +SZ32150(I)*SZ32150(I))
            AAA = UN/BBB
            
            SX32150(I)=SX32150(I) * AAA
            SY32150(I)=SY32150(I) * AAA
            SZ32150(I)=SZ32150(I) * AAA

            J7= IXX(I,8)
            IF(J7 > 0) THEN
              NN = INOD_PXFEM(J7) 
              ILY = ISEG_PLY(7,I)
              GAPN  =  GAP_NM(7,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX32150(I) 
     .              + PLY(ILY)%U(2,NN)*SY32150(I) 
     .              + PLY(ILY)%U(3,NN)*SZ32150(I) 
                 GAPN = MAX(ZERO,GAP_NM(7,I)+DN)
              ENDIF 
#include "lockon.inc"              
              IF(ISEG(I)>0 )WAGAP(ISEG(I),J7)=MAX(WAGAP(ISEG(I),J7),GAPN)
#include "lockoff.inc"
           ENDIF 

           J8= IXX(I,9)
           IF(J8 > 0) THEN
             NN = INOD_PXFEM(J8) 
             ILY = ISEG_PLY(8,I)
             GAPN  =  GAP_NM(8,I)
             IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX32150(I) 
     .              + PLY(ILY)%U(2,NN)*SY32150(I) 
     .              + PLY(ILY)%U(3,NN)*SZ32150(I) 
                 GAPN = MAX(ZERO,GAP_NM(8,I)+DN)
             ENDIF 
#include "lockon.inc"              
               IF(ISEG(I)>0 )WAGAP(ISEG(I),J8)=MAX(WAGAP(ISEG(I),J8),GAPN)
#include "lockoff.inc"
          ENDIF
C           
          IF(IXX(I,3) /= IXX(I,4))THEN 
            BBB = SQRT(SX43160(I)*SX43160(I)          
     +                +SY43160(I)*SY43160(I)          
     +                +SZ43160(I)*SZ43160(I))         

            AAA = UN/BBB
            SX43160(I)=SX43160(I) * AAA
            SY43160(I)=SY43160(I) * AAA
            SZ43160(I)=SZ43160(I) * AAA
            J9= IXX(I,10)
            IF(J9 > 0) THEN
              NN = INOD_PXFEM(J9) 
              ILY = ISEG_PLY(9,I)
              GAPN  =  GAP_NM(9,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX43160(I) 
     .              + PLY(ILY)%U(2,NN)*SY43160(I) 
     .              + PLY(ILY)%U(3,NN)*SZ43160(I) 
                 GAPN = MAX(ZERO,GAP_NM(9,I)+DN)
              ENDIF 
#include "lockon.inc"              
              IF(ISEG(I)>0 )WAGAP(ISEG(I),J9)=MAX(WAGAP(ISEG(I),J9),GAPN)
#include "lockoff.inc"
           ENDIF

           J10= IXX(I,11)
           IF(J10 > 0) THEN
              NN = INOD_PXFEM(J10) 
              ILY = ISEG_PLY(10,I)
              GAPN  =  GAP_NM(10,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX43160(I) 
     .              + PLY(ILY)%U(2,NN)*SY43160(I) 
     .              + PLY(ILY)%U(3,NN)*SZ43160(I) 
                 GAPN = MAX(ZERO,GAP_NM(10,I)+DN)
               ENDIF 
#include "lockon.inc"              
               IF(ISEG(I)>0 )WAGAP(ISEG(I),J10)=MAX(WAGAP(ISEG(I),J10),GAPN)
#include "lockoff.inc"
           ENDIF 
                   
          ENDIF    !!   ix3/=ix4   
          BBB = SQRT(SX14170(I)*SX14170(I)
     +              +SY14170(I)*SY14170(I)
     +              +SZ14170(I)*SZ14170(I))
                   
          AAA = UN/BBB                             
          SX14170(I)=SX14170(I) * AAA
          SY14170(I)=SY14170(I) * AAA
          SZ14170(I)=SZ14170(I) * AAA

          J11= IXX(I,12)
          IF(J11 > 0) THEN
             NN = INOD_PXFEM(J11) 
             ILY = ISEG_PLY(11,I)
             GAPN  =  GAP_NM(11,I)
             IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX14170(I) 
     .              + PLY(ILY)%U(2,NN)*SY14170(I) 
     .              + PLY(ILY)%U(3,NN)*SZ14170(I) 
                 GAPN = MAX(ZERO,GAP_NM(11,I)+DN)
             ENDIF 
#include "lockon.inc"              
             IF(ISEG(I)>0 )WAGAP(ISEG(I),J11)=MAX(WAGAP(ISEG(I),J11),GAPN)
#include "lockoff.inc"
          ENDIF   
                                
           J12= IXX(I,13)
           IF(J12 > 0) THEN
              NN = INOD_PXFEM(J12) 
              ILY = ISEG_PLY(12,I)
              GAPN  =  GAP_NM(12,I)
              IF(NN > 0 .AND. ILY > 0) THEN 
                 DN = PLY(ILY)%U(1,NN)*SX14170(I) 
     .              + PLY(ILY)%U(2,NN)*SY14170(I) 
     .              + PLY(ILY)%U(3,NN)*SZ14170(I) 
                 GAPN = MAX(ZERO,GAP_NM(12,I)+DN)
              ENDIF 
#include "lockon.inc"              
               IF(ISEG(I)>0 )WAGAP(ISEG(I),J12)=MAX(WAGAP(ISEG(I),J12),GAPN)
#include "lockoff.inc"
           ENDIF                   
!!!!          
         ENDDO                   
C         
         RETURN
         END
