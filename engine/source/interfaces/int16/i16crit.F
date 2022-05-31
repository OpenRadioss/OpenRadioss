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
Chd|  I16CRIT                       source/interfaces/int16/i16crit.F
Chd|-- called by -----------
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|-- calls ---------------
Chd|        I10BOX                        source/interfaces/int16/i16crit.F
Chd|        I16BOX                        source/interfaces/int16/i16crit.F
Chd|        I20BOX                        source/interfaces/int16/i16crit.F
Chd|        I8BOX                         source/interfaces/int16/i16crit.F
Chd|====================================================================
       SUBROUTINE I16CRIT(
     1       X      ,NSV    ,NELEM ,NSN   ,EMINX  ,
     2       NME    ,ITASK  ,XSAV  ,IXS   ,IXS16  ,
     3       IXS20  ,IXS10  ,V      ,A     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
      COMMON /I16TMP/SIZE
      my_real 
     .        SIZE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,ITASK,NME,
     .        NSV(*),NELEM(*),IXS(NIXS,*),IXS16(8,*),IXS20(12,*),
     .        IXS10(6,*)
      my_real
     .   X(3,*),V(3,*),A(3,*),XSAV(3,*),EMINX(6,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF,NMEF,NSNL,NMEL,I, J, II, K,I16,I20,LFT16,LLT16,
     .      LFT20,LLT20,LFT8,LLT8,LFT10,LLT10,I8,I10,
     .      INDEX16(MVSIZ),INDEX20(MVSIZ),INDEX8(MVSIZ),INDEX10(MVSIZ)
      my_real
     .   XMSR(6),XSLV(6),SIZE_T ,XX,YY,ZZ      
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
C------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      NMEF = 1 + ITASK*NME / NTHREAD
      NMEL = (ITASK+1)*NME / NTHREAD
C--------------------------------------------------------------
C     0- CALCUL DU CRITERE POUR SAVOIR SI ON DOIT TRIER OU NON
C--------------------------------------------------------------
      XSLV(1) = -EP30
      XSLV(2) = -EP30
      XSLV(3) = -EP30
      XSLV(4) =  EP30
      XSLV(5) =  EP30
      XSLV(6) =  EP30
      XMSR(1) = -EP30
      XMSR(2) = -EP30
      XMSR(3) = -EP30
      XMSR(4) =  EP30
      XMSR(5) =  EP30
      XMSR(6) =  EP30      
C
      SIZE_T = ZERO
C
      DO I=NSNF,NSNL
            J=NSV(I)
            XX=X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            YY=X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            ZZ=X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
            XSLV(1)=MAX(XSLV(1),XX-XSAV(1,J))
            XSLV(2)=MAX(XSLV(2),YY-XSAV(2,J))
            XSLV(3)=MAX(XSLV(3),ZZ-XSAV(3,J))
            XSLV(4)=MIN(XSLV(4),XX-XSAV(1,J))
            XSLV(5)=MIN(XSLV(5),YY-XSAV(2,J))
            XSLV(6)=MIN(XSLV(6),ZZ-XSAV(3,J))
      END DO
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
      DO I=NMEF,NMEL
            EMINX(1,I) = EP30
            EMINX(2,I) = EP30
            EMINX(3,I) = EP30
            EMINX(4,I) = -EP30
            EMINX(5,I) = -EP30
            EMINX(6,I) = -EP30    
      ENDDO
C
      LFT16=1
      LLT16=0
      LFT20=1
      LLT20=0
      LFT8 =1
      LLT8 =0
      LFT10=1
      LLT10=0
      DO I=NMEF,NMEL
          I8  = NELEM(I)
          I10 = I8-NUMELS8
          I20 = I10-NUMELS10
          I16 = I20-NUMELS20
          IF(I16.GE.1.AND.I16.LE.NUMELS16)THEN
            LLT16=LLT16+1
            INDEX16(LLT16)=I
            IF(LLT16.EQ.MVSIZ-1)THEN
              CALL I16BOX(
     1            LFT16,LLT16  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS16,SIZE_T,
     3            XMSR ,INDEX16,XSAV  )
              LLT16=0
            ENDIF
          ELSEIF(I20.GE.1.AND.I20.LE.NUMELS20)THEN
            LLT20=LLT20+1
            INDEX20(LLT20)=I
            IF(LLT20.EQ.MVSIZ-1)THEN
              CALL I20BOX(
     1            LFT20,LLT20  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS20,SIZE_T,
     3            XMSR ,INDEX20,XSAV  )
              LLT20=0
            ENDIF
          ELSEIF(I10.GE.1)THEN
            LLT10=LLT10+1
            INDEX10(LLT10)=I
            IF(LLT10.EQ.MVSIZ-1)THEN
              CALL I10BOX(
     1            LFT10,LLT10  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS10,SIZE_T,
     3            XMSR ,INDEX10,XSAV  )
              LLT10=0
            ENDIF
          ELSEIF(I8.GE.1)THEN
            LLT8=LLT8+1
            INDEX8(LLT8)=I
            IF(LLT8.EQ.MVSIZ-1)THEN
              CALL I8BOX(
     1            LFT8 ,LLT8   ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,SIZE_T,
     3            XMSR ,INDEX8 ,XSAV )
              LLT8=0
            ENDIF
          ENDIF
      END DO
      IF(LLT16.GT.0)CALL I16BOX(
     1            LFT16,LLT16  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS16,SIZE_T,
     3            XMSR ,INDEX16,XSAV  )
      IF(LLT20.GT.0)CALL I20BOX(
     1            LFT20,LLT20  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS20,SIZE_T,
     3            XMSR ,INDEX20,XSAV  )
      IF(LLT8.GT.0)CALL I8BOX(
     1            LFT8 ,LLT8   ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,SIZE_T,
     3            XMSR ,INDEX8 ,XSAV  )
      IF(LLT10.GT.0)CALL I10BOX(
     1            LFT10,LLT10  ,NELEM,EMINX,NMEF ,NMEL  ,
     2            X    ,V      ,A    ,IXS  ,IXS10,SIZE_T,
     3            XMSR ,INDEX10,XSAV  )
C
#include "lockon.inc"
         XSLVG(1)=MAX(XSLVG(1),XSLV(1))
         XSLVG(2)=MAX(XSLVG(2),XSLV(2))
         XSLVG(3)=MAX(XSLVG(3),XSLV(3))
         XSLVG(4)=MIN(XSLVG(4),XSLV(4))
         XSLVG(5)=MIN(XSLVG(5),XSLV(5))
         XSLVG(6)=MIN(XSLVG(6),XSLV(6))
         XMSRG(1)=MAX(XMSRG(1),XMSR(1))
         XMSRG(2)=MAX(XMSRG(2),XMSR(2))
         XMSRG(3)=MAX(XMSRG(3),XMSR(3))
         XMSRG(4)=MIN(XMSRG(4),XMSR(4))
         XMSRG(5)=MIN(XMSRG(5),XMSR(5))
         XMSRG(6)=MIN(XMSRG(6),XMSR(6))
         SIZE = SIZE + SIZE_T
#include "lockoff.inc"
C
      RETURN
      END
Chd|====================================================================
Chd|  I16BOX                        source/interfaces/int16/i16crit.F
Chd|-- called by -----------
Chd|        I16CRIT                       source/interfaces/int16/i16crit.F
Chd|        I17CRIT                       source/interfaces/int17/i17crit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I16BOX(LFT ,LLT  ,NELEM,EMINX,NMEF,NMEL,
     2                  X   ,V    ,A    ,IXS ,IXS16,SIZE,
     3                  XMSR,INDEX,XSAV )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT ,LLT,NMEF,NMEL,
     .        IXS(NIXS,*),IXS16(8,*),NELEM(*),INDEX(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),SIZE,XMSR(*),XSAV(3,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NE,IFACE,IDIR,IPERM(8,2),N16
      my_real
     .   AN,AX,BN,BX,CN,CX,DX,DN,D4,D8,X1,X2,X3,X4,
     .   X9,X10,X11,X12,XC,XX,XN
      DATA IPERM / 2, 3, 4, 5, 1, 2, 3, 4,
     2             6, 7, 8, 9, 5, 6, 7, 8/
C
C-----------------------------------------------
C/*                                       
C                                     
C                         ( 7)8==============(14)6=============( 6)7
C                        //|                                  //|
C                       // |                                 //||
C                      //  |                                // ||
C                     //   |                               //  ||
C                    //    |                              //   ||
C                  (15)7   |                            (13)5  ||
C                  //      |                            //     ||
C                 //     ( 3)4--------------(10)2------//-----( 2)3
C                //       /                           //      //
C               //       /                           //      //
C              //       /                           //      //
C            ( 8)9==============(16)8=============( 5)6    //
C             ||      /                            ||     //
C             ||   (11)3              (C)          ||   ( 9)1    
C             ||    /                              ||   //
C             ||   /                               ||  //
C             ||  /                                || //
C             || /                                 ||//
C             ||/                                  ||/
C            ( 4)5==============(12)4=============( 1)2    
C
C*/
C---------------------------------------------------------
C    MONODIM
C---------------------------------------------------------
C
C                             x
C                          /     \
C                       /        (2)
C                    (3)
C                   /
C                 /
C              (1)
C
C      N1 = -0.5 (1-r)r     dN1/dr = r - 0.5
C      N2 =  0.5 (1+r)r     dN2/dr = r + 0.5
C      N3 = (1-r^2)         dN3/dr = - 2 r
C
C      x = N1 x1 + N2 x2 + N3 x3
C      x = -0.5 (1-r)r x1 + 0.5 (1+r)r x2 + (1-r^2) x3
C      2 x = (x1 + x2 - 2 x3) r^2 + (x2-x1) r + 2 x3
C
C   0) recherche du point xmax
C
C      dx/dr = (r - 0.5) x1 + (r + 0.5) x2 - 2 r x3 = 0
C      r  =  0.5 (x1 - x2) / (x1 + x2 - 2 x3)
C
C      2 x (x1 + x2 - 2 x3) = (x1 + x2 - 2 x3)^2 r^2 
C                           + (x2-x1)(x1 + x2 - 2 x3) r
C                           + 2 (x1 + x2 - 2 x3)x3
C
C      2 x (x1 + x2 - 2 x3) = - 0.25 (x1 - x2)^2
C                           + 2 (x1 + x2 - 2 x3)x3
C
C      x  = x3 - 0.125 (x1 - x2)^2 / (x1 + x2 - 2 x3)
C
C------------------------------------------------------------
C     solution 0 =>     x < x3 - 0.125 (x1 - x2)^2 / (x1 + x2 - 2 x3)
C            si x3 -> (x1 + x2)/ 2   x -> infini
C------------------------------------------------------------
C
C   1) recherche du point xmax entre 1 et 2
C
C si  r > 1        
C      =>   x  = x2
C
C si  r < -1        
C      =>   x  = x1
C
C si -1 < r < 1        
C      =>   -1/4 < 0.125 (x1 - x2) / (x1 + x2 - 2 x3) < 1/4
C
C      (x  - x3)/(x1 - x2) = - 0.125 (x1 - x2) / (x1 + x2 - 2 x3)
C      -1/4 < (x  - x3)/  < 1/4
C
C      si x2 > x1      x3 -1/4 (x2 - x1) < x < x3 + 1/4 (x2 - x1)
C      si x2 < x1      x3 -1/4 (x2 - x1) > x > x3 + 1/4 (x2 - x1)
C
C      =>   x3 - 1/4 |x2 - x1| < x < x3 + 1/4 |x2 - x1|
C
C------------------------------------------------------------
C     solution 1 =>     x < max (x1 , x2 , x3 + 1/4 |x2 - x1|)
C------------------------------------------------------------
C
C   2) recherche de la position de x3 la + defavorable
C
C      x  = x3 - 0.125 (x1 - x2)^2 / (x1 + x2 - 2 x3)
C      dx/dx3  = 1 - 0.25 (x1 - x2)^2 / (x1 + x2 - 2 x3)^2 = 0
C      (2 x3 - x1 - x2 )^2  = 0.25 (x2 - x1)^2 
C      si x2 > x1   et   x3 > (x1 + x2)/2 ou
C      si x2 < x1   et   x3 < (x1 + x2)/2
C             (2 x3 - x1 - x2 )  = 0.5 (x2 - x1) 
C             x3  =  (x1 + x2)/2  + (x2 - x1)/4
C      si x2 > x1   et   x3 < (x1 + x2)/2 ou 
C      si x2 < x1   et   x3 > (x1 + x2)/2
C             pas de solution
C
C        si   x3 <  (x1 + x2)/2  + (x2 - x1)/4
C           => x < max (x1 , x2 ) et 
C           => x > min (x1 , x2 )        => verifie par solution 1 et 2
C
C        si   x3  =  (x1 + x2)/2  + (x2 - x1)/4
C            x  = x3 + 0.125 (x1 - x2)^2 / (2 x3 - x1 - x2 )
C            x  = (x1 + x2)/2 + (x2 - x1)/4 
C               + 0.125 (x1 - x2)^2 / (2 ((x1 + x2)/2  + (x2 - x1)/4) - x1 - x2)
C            x  = (x1 + x2)/2 + (x2 - x1)/4 
C               + 0.25 (x2 - x1)^2 / (x2 - x1)
C            si x2 > x1  => x  = x2
C            si x2 < x1  => x  = x1
C            
C            et x  < x3 + 1/4 |x2 - x1|
C
C        si   x3  =  x2
C            x  = x3 + 0.125 (x1 - x2)^2 / (2 x3 - x1 - x2 )
C            x  = x3 + 0.125 (x1 - x2)^2 / (x2 - x1)
C            x  = x3 +- 1/8 |x1 - x2| = x2 +- 1/8 |x1 - x2|
C
C        si   (x1 + x2)/2  + (x2 - x1)/4 < x3 < max(x1,x2)
C               x < max(x1,x2) + 1/8  |x1 - x2| 
C            et x < x3 + 1/4  |x1 - x2| 
C
C        si   x3  >  max(x1,x2) 
C               x < x3 + 1/8  |x1 - x2| 
C------------------------------------------------------------
C     solution 2 =>    x < max (x1 , x2 , x3) + 1/8 |x2 - x1|
C------------------------------------------------------------
C     solution 1 :     x < max (x1 , x2 , x3 + 1/4 |x2 - x1|)
C------------------------------------------------------------
C     =>     x < min (solution 1,solution 2)
C------------------------------------------------------------
C
C  3) solution exacte (solution 0 bornee par solution 1)
C
C      solution 0 : 
C         x  = x3 - 0.125 (x1 - x2)^2 / (x1 + x2 - 2 x3)
C      solution 1 : 
C         x < max (x1 , x2 , x3 + 1/4 |x2 - x1|)
C------------------------------------------------------------
C     solution 3 =>     x < min (solution 1,solution 0)
C------------------------------------------------------------
C     s = (x1+x2)/2     d = |x2-x1|
C-------------------------------------------------------------------------
C            x3                         x min                x max
C-------------------------------------------------------------------------
C  -inf    < x3 < s - d/4         x3 + d^2 / 16(x3-s)      max(x1,x2) 
C  s - d/4 < x3 < s + d/4         min(x1,x2)               max(x1,x2) 
C  s + d/4 < x3 < +inf            min(x1,x2)               x3 + d^2 / 16(x3-s)     
C-------------------------------------------------------------------------
C---------------------------------------------------------
C    BIDIM ( pas  resolu )
C---------------------------------------------------------
C
C-----------------------------------------------
C  i    ri   si   ti       Ni                                  
C--------------------------------------------------------------------
C  1   -1   -1   -1     1/4(1-r)(1-t)(-r-t-1)
C  2   -1   -1   +1     1/4(1-r)(1+t)(-r+t-1)
C  3   +1   -1   +1     1/4(1+r)(1+t)(+r+t-1)
C  4   +1   -1   -1     1/4(1+r)(1-t)(+r-t-1)
C  9   -1   -1    0     1/2(1-t^2)(1-r)
C 10    0   -1   +1     1/2(1-r^2)     (1+t)
C 11   +1   -1    0     1/2(1-t^2)(1+r)
C 12    0   -1   -1     1/2(1-r^2)     (1-t)
C
C      x = N1 x1 + N2 x2 + N3 x3 + N4 x4
C        + N9 x9 + N10 x10 + N11 x11 + N12 x12
C
C   0) recherche du point xmax
C
C      dx/dr = -1/4(1-t)(-2r-t) x1
C              -1/4(1+t)(-2r+t) x2
C              +1/4(1+t)(+2r+t) x3
C              +1/4(1-t)(+2r-t) x4
C              -1/2(1-t^2) x9 
C              -r(1+t)     x10 
C              +1/2(1-t^2) x11 
C              -r(1-t)     x12             = 0
C      dx/dt = -1/4(1-r)(-2t-r) x1
C              +1/4(1-r)(+2t-r) x2
C              +1/4(1+r)(+2t+r) x3
C              +1/4(1+r)(-2t+r) x4
C              -t(1-r)     x9 
C              +1/2(1-r^2) x10 
C              -t(1+r)     x11 
C              -1/2(1-r^2) x12             = 0
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
      DO IFACE=1,2
C-----------------------------------------------------------------------
C       Face 1 2 3 4 ou 5 6 7 8
C-----------------------------------------------------------------------
        DO IDIR=1,3
C-----------------------------------------------------------------------
C         X Y ou Z
C-----------------------------------------------------------------------
          DO L=LFT,LLT
            I = INDEX(L)
            NE = NELEM(I)
            N16= NE - NUMELS8 - NUMELS10 - NUMELS20
C
            J = IXS(IPERM(1,IFACE),NE)
            X1 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X1-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X1-XSAV(IDIR,J))
            J = IXS(IPERM(2,IFACE),NE)
            X2 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X2-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X2-XSAV(IDIR,J))
            J = IXS(IPERM(3,IFACE),NE)
            X3 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X3-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X3-XSAV(IDIR,J))
            J = IXS(IPERM(4,IFACE),NE)
            X4 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X4-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X4-XSAV(IDIR,J))
            J = IXS16(IPERM(5,IFACE),N16)
            X9 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X9-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X9-XSAV(IDIR,J))
            J = IXS16(IPERM(6,IFACE),N16)
            X10= X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X10-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X10-XSAV(IDIR,J))
            J = IXS16(IPERM(7,IFACE),N16)
            X11= X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X11-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X11-XSAV(IDIR,J))
            J = IXS16(IPERM(8,IFACE),N16)
            X12= X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X12-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X12-XSAV(IDIR,J))
C
            XC = HALF*(X9+X10+X11+X12) - FOURTH*(X1+X2+X3+X4)
C
            D4 = FOURTH * ABS(X1-X2)
            AN = MIN( X1 , X2 , X9-D4 )
            AX = MAX( X1 , X2 , X9+D4 )
C
            D4 = FOURTH * ABS(X3-X4)
            BN = MIN( X3 , X4 , X11-D4 )
            BX = MAX( X3 , X4 , X11+D4 )
C
            D4 = FOURTH * ABS(X12-X10)
            CN = MIN( X12 , X10 , XC-D4 )
            CX = MAX( X12 , X10 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX-BN , BX-AN )
            D4 = D8 + D8
            DN = MAX(MIN( AN , BN , CN-D4 ),MIN(AN , BN , CN) - D8 )
            DX = MIN(MAX( AX , BX , CX+D4 ),MAX( AX , BX , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C
            SIZE = SIZE + DX - DN
C
          ENDDO
        ENDDO
      ENDDO
C--------------------------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  I20BOX                        source/interfaces/int16/i16crit.F
Chd|-- called by -----------
Chd|        I16CRIT                       source/interfaces/int16/i16crit.F
Chd|        I17CRIT                       source/interfaces/int17/i17crit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20BOX(LFT ,LLT  ,NELEM,EMINX,NMEF,NMEL,
     2                  X   ,V    ,A    ,IXS ,IXS20,SIZE,
     3                  XMSR,INDEX,XSAV )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT ,LLT,NMEF,NMEL,
     .        IXS(NIXS,*),IXS20(12,*),NELEM(*),INDEX(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),SIZE,XMSR(*),XSAV(3,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NE,IDIR,N20
      my_real
     .   AN12,AX12,AN34,AX34,AN56,AX56,AN78,AX78,CN,CX,DX,DN,D4,D8,
     .   X1,X2,X3,X4,X5,X6,X7,X8,
     .   X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,XC,XX,XN
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
C-----------------------------------------------------------------------
C       Face 1 2 3 4 ou 5 6 7 8
C-----------------------------------------------------------------------
      DO IDIR=1,3
C-----------------------------------------------------------------------
C         X Y ou Z
C-----------------------------------------------------------------------
        DO L=LFT,LLT
            I = INDEX(L)
            NE = NELEM(I)
            N20= NE - NUMELS8 - NUMELS10
C-----------------------------------------------------------------------
            J = IXS(2,NE)
            X1 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X1-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X1-XSAV(IDIR,J))
            J = IXS(3,NE)
            X2 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X2-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X2-XSAV(IDIR,J))
            J = IXS(4,NE)
            X3 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X3-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X3-XSAV(IDIR,J))
            J = IXS(5,NE)
            X4 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X4-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X4-XSAV(IDIR,J))
            J = IXS(6,NE)
            X5 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X5-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X5-XSAV(IDIR,J))
            J = IXS(7,NE)
            X6 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X6-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X6-XSAV(IDIR,J))
            J = IXS(8,NE)
            X7 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X7-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X7-XSAV(IDIR,J))
            J = IXS(9,NE)
            X8 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X8-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X8-XSAV(IDIR,J))
C
            J = IXS20(1,N20)
            IF(J.NE.0)THEN	
              X9  = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
              XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X9-XSAV(IDIR,J))
              XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X9-XSAV(IDIR,J))
	    ELSE
	      X9=0.5*(X(IDIR,IXS(2,NE))+X(IDIR,IXS(3,NE)))
	    ENDIF
            J = IXS20(2,N20)
            IF(J.NE.0)THEN
              X10 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
              XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X10-XSAV(IDIR,J))
              XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X10-XSAV(IDIR,J))
	    ELSE
	      X10=0.5*(X(IDIR,IXS(3,NE))+X(IDIR,IXS(4,NE)))
	    ENDIF
            J = IXS20(3,N20)
            IF(J.NE.0)THEN
             X11 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X11-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X11-XSAV(IDIR,J))
	    ELSE
	      X11=0.5*(X(IDIR,IXS(4,NE))+X(IDIR,IXS(5,NE)))
	    ENDIF
            J = IXS20(4,N20)
            IF(J.NE.0)THEN	
             X12 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X12-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X12-XSAV(IDIR,J))
	    ELSE
	      X12=0.5*(X(IDIR,IXS(5,NE))+X(IDIR,IXS(2,NE)))
	    ENDIF
            J = IXS20(5,N20)
            IF(J.NE.0)THEN	
             X13 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X13-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X13-XSAV(IDIR,J))
	    ELSE
	      X13=0.5*(X(IDIR,IXS(2,NE))+X(IDIR,IXS(6,NE)))
	    ENDIF  	   
            J = IXS20(6,N20)
            IF(J.NE.0)THEN	
             X14 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X14-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X14-XSAV(IDIR,J))
	    ELSE
	      X14=0.5*(X(IDIR,IXS(3,NE))+X(IDIR,IXS(6,NE)))
	    ENDIF
            J = IXS20(7,N20)
            IF(J.NE.0)THEN	
             X15 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X15-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X15-XSAV(IDIR,J))
	    ELSE
	      X15=0.5*(X(IDIR,IXS(4,NE))+X(IDIR,IXS(8,NE)))
	    ENDIF
            J = IXS20(8,N20)
            IF(J.NE.0)THEN
             X16 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X16-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X16-XSAV(IDIR,J))
	    ELSE
	      X16=0.5*(X(IDIR,IXS(5,NE))+X(IDIR,IXS(9,NE)))
	    ENDIF
            J = IXS20(9,N20)
            IF(J.NE.0)THEN	
             X17 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X17-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X17-XSAV(IDIR,J))
	    ELSE
	      X17=0.5*(X(IDIR,IXS(6,NE))+X(IDIR,IXS(7,NE)))
	    ENDIF
            J = IXS20(10,N20)
            IF(J.NE.0)THEN
             X18 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X18-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X18-XSAV(IDIR,J))
	    ELSE
	      X18=0.5*(X(IDIR,IXS(7,NE))+X(IDIR,IXS(8,NE)))
	    ENDIF
            J = IXS20(11,N20)
            IF(J.NE.0)THEN	
             X19 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X19-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X19-XSAV(IDIR,J))
	    ELSE
	      X19=0.5*(X(IDIR,IXS(8,NE))+X(IDIR,IXS(9,NE)))
	    ENDIF
            J = IXS20(12,N20)
            IF(J.NE.0)THEN
             X20 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
             XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X20-XSAV(IDIR,J))
             XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X20-XSAV(IDIR,J))
	    ELSE
	      X20=0.5*(X(IDIR,IXS(6,NE))+X(IDIR,IXS(9,NE)))
	    ENDIF
C
C-----------------------------------------------------------------------
C           Face 1 2 3 4
C-----------------------------------------------------------------------
            XC = HALF*(X9+X10+X11+X12) - FOURTH*(X1+X2+X3+X4)
C
            D4 = FOURTH * ABS(X1-X2)
            AN12 = MIN( X1 , X2 , X9-D4 )
            AX12 = MAX( X1 , X2 , X9+D4 )
C
            D4 = FOURTH * ABS(X3-X4)
            AN34 = MIN( X3 , X4 , X11-D4 )
            AX34 = MAX( X3 , X4 , X11+D4 )
C
            D4 = FOURTH * ABS(X12-X10)
            CN = MIN( X12 , X10 , XC-D4 )
            CX = MAX( X12 , X10 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX12-AN34 , AX34-AN12 )
            D4 = D8 + D8
            DN = MAX(MIN(AN12 , AN34 , CN-D4 ),
     .               MIN(AN12 , AN34 , CN) - D8 )
            DX = MIN(MAX(AX12 , AX34 , CX+D4 ),
     .               MAX(AX12 , AX34 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
C           Face 5 6 7 8
C-----------------------------------------------------------------------
            XC = HALF*(X17+X18+X19+X20) - FOURTH*(X5+X6+X7+X8)
C
            D4 = FOURTH * ABS(X5-X6)
            AN56 = MIN( X5 , X6 , X17-D4 )
            AX56 = MAX( X5 , X6 , X17+D4 )
C
            D4 = FOURTH * ABS(X7-X8)
            AN78 = MIN( X7 , X8 , X19-D4 )
            AX78 = MAX( X7 , X8 , X19+D4 )
C
            D4 = FOURTH * ABS(X20-X18)
            CN = MIN( X20 , X18 , XC-D4 )
            CX = MAX( X20 , X18 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX56-AN78 , AX78-AN56 )
            D4 = D8 + D8
            DN = MAX(MIN(AN56 , AN78 , CN-D4 ),
     .               MIN(AN56 , AN78 , CN) - D8 )
            DX = MIN(MAX(AX56 , AX78 , CX+D4 ),
     .               MAX(AX56 , AX78 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
C           Face 1 2 6 5
C-----------------------------------------------------------------------
            XC = HALF*(X9+X14+X17+X13) - FOURTH*(X1+X2+X6+X5)
C
            D4 = FOURTH * ABS(X13-X14)
            CN = MIN( X13 , X14 , XC-D4 )
            CX = MAX( X13 , X14 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX12-AN56 , AX56-AN12 )
            D4 = D8 + D8
            DN = MAX(MIN(AN12 , AN56 , CN-D4 ),
     .               MIN(AN12 , AN56 , CN) - D8 )
            DX = MIN(MAX(AX12 , AX56 , CX+D4 ),
     .               MAX(AX12 , AX56 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
C           Face 3 4 8 7
C-----------------------------------------------------------------------
            XC = HALF*(X11+X15+X19+X16) - FOURTH*(X3+X4+X8+X7)
C
            D4 = FOURTH * ABS(X16-X15)
            CN = MIN( X15 , X16 , XC-D4 )
            CX = MAX( X15 , X16 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX34-AN78 , AX78-AN34 )
            D4 = D8 + D8
            DN = MAX(MIN(AN34 , AN78 , CN-D4 ),
     .               MIN(AN34 , AN78 , CN) - D8 )
            DX = MIN(MAX(AX34 , AX78 , CX+D4 ),
     .               MAX(AX34 , AX78 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
C           Face 4 1 5 8
C-----------------------------------------------------------------------
            XC = HALF*(X12+X13+X20+X16) - FOURTH*(X4+X1+X5+X8)
C
            D4 = FOURTH * ABS(X4-X1)
            AN12 = MIN( X4 , X1 , X12-D4 )
            AX12 = MAX( X4 , X1 , X12+D4 )
C
            D4 = FOURTH * ABS(X8-X5)
            AN34 = MIN( X8 , X5 , X20-D4 )
            AX34 = MAX( X8 , X5 , X20+D4 )
C
            D4 = FOURTH * ABS(X16-X13)
            CN = MIN( X16 , X13 , XC-D4 )
            CX = MAX( X16 , X13 , XC+D4 )
C
            D8 = ONE_OVER_8 * MAX( AX12-AN34 , AX34-AN12 )
            D4 = D8 + D8
            DN = MAX(MIN(AN12 , AN34 , CN-D4 ),
     .               MIN(AN12 , AN34 , CN) - D8 )
            DX = MIN(MAX(AX12 , AX34 , CX+D4 ),
     .               MAX(AX12 , AX34 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
C           Face 3 2 6 7
C-----------------------------------------------------------------------
            XC = HALF*(X10+X14+X18+X15) - FOURTH*(X3+X2+X6+X7)
C
            D4 = FOURTH * ABS(X3-X2)
            AN12 = MIN( X3 , X2 , X10-D4 )
            AX12 = MAX( X3 , X2 , X10+D4 )
C
            D4 = FOURTH * ABS(X7-X6)
            AN34 = MIN( X7 , X6 , X18-D4 )
            AX34 = MAX( X7 , X6 , X18+D4 )
C
            D4 = FOURTH * ABS(X15-X14)
            CN = MIN( X15 , X14 , XC-D4 )
            CX = MAX( X15 , X14 , XC+D4 )
C
            D8 = ONE_OVER_8* MAX( AX12-AN34 , AX34-AN12 )
            D4 = D8 + D8
            DN = MAX(MIN(AN12 , AN34 , CN-D4 ),
     .               MIN(AN12 , AN34 , CN) - D8 )
            DX = MIN(MAX(AX12 , AX34 , CX+D4 ),
     .               MAX(AX12 , AX34 , CX) + D8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C-----------------------------------------------------------------------
            SIZE = SIZE + DX - DN
C
        ENDDO
      ENDDO
C--------------------------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  I10BOX                        source/interfaces/int16/i16crit.F
Chd|-- called by -----------
Chd|        I16CRIT                       source/interfaces/int16/i16crit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I10BOX(LFT ,LLT  ,NELEM,EMINX,NMEF,NMEL,
     2                  X   ,V    ,A    ,IXS ,IXS10,SIZE,
     3                  XMSR,INDEX,XSAV )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT ,LLT,NMEF,NMEL,
     .        IXS(NIXS,*),IXS10(6,*),NELEM(*),INDEX(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),SIZE,XMSR(*),XSAV(3,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NE,IDIR,N10
      my_real
     .   AN12,AX12,AN34,AX34,AN56,AX56,AN78,AX78,CN,CX,DX,DN,D4,D8,
     .   X1,X2,X3,X4,X5,X6,X7,X8,
     .   X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,XC,XX,XN
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
C-----------------------------------------------------------------------
C       Face 1 2 3 4 ou 5 6 7 8
C-----------------------------------------------------------------------
      DO IDIR=1,3
C-----------------------------------------------------------------------
C         X Y ou Z
C-----------------------------------------------------------------------
        DO L=LFT,LLT
            I = INDEX(L)
            NE = NELEM(I)
            N10= NE - NUMELS8
C-----------------------------------------------------------------------
            J = IXS(2,NE)
            X1 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X1-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X1-XSAV(IDIR,J))
            J = IXS(4,NE)
            X2 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X2-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X2-XSAV(IDIR,J))
            J = IXS(6,NE)
            X3 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X3-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X3-XSAV(IDIR,J))
            J = IXS(7,NE)
            X4 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X4-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X4-XSAV(IDIR,J))
C
            J = IXS10(1,N10)
            X5  = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X5-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X5-XSAV(IDIR,J))
            J = IXS10(2,N10)
            X6 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X6-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X6-XSAV(IDIR,J))
            J = IXS10(3,N10)
            X7 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X7-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X7-XSAV(IDIR,J))
            J = IXS10(4,N10)
            X8 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X8-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X8-XSAV(IDIR,J))
            J = IXS10(5,N10)
            X9 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X9-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X9-XSAV(IDIR,J))
            J = IXS10(6,N10)
            X10 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X10-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X10-XSAV(IDIR,J))
C-----------------------------------------------------------------------
            XX=MAX(X1,X2 ,X3 ,X4 ,X5 ,X6 ,X7 ,X8 ,X9,X10)
            XN=MIN(X1,X2 ,X3 ,X4 ,X5 ,X6 ,X7 ,X8 ,X9,X10)
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , XN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), XX )
C-----------------------------------------------------------------------
C           Face 1 2 3 4
C-----------------------------------------------------------------------
            XC = (TWO*(X5+X6+X7) - (X1+X2+X3))* THIRD
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , XC )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), XC )
C
            XC = (TWO*(X5+X8+X9) - (X1+X2+X4))*THIRD
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , XC )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), XC )
C
            XC = (TWO*(X6+X9+X10) - (X2+X3+X4)) * THIRD
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , XC )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), XC )
C
            XC = (TWO*(X7+X8+X10) - (X3+X1+X4)) * THIRD
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , XC )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), XC )
C
C-----------------------------------------------------------------------
C
        ENDDO
      ENDDO
C--------------------------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  I8BOX                         source/interfaces/int16/i16crit.F
Chd|-- called by -----------
Chd|        I16CRIT                       source/interfaces/int16/i16crit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I8BOX(LFT ,LLT  ,NELEM,EMINX,NMEF,NMEL,
     2                  X   ,V    ,A    ,IXS ,SIZE,
     3                  XMSR,INDEX,XSAV )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT ,LLT,NMEF,NMEL,
     .        IXS(NIXS,*),NELEM(*),INDEX(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),SIZE,XMSR(*),XSAV(3,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NE,IDIR,N10
      my_real
     .   AN12,AX12,AN34,AX34,AN56,AX56,AN78,AX78,CN,CX,DX,DN,D4,D8,
     .   X1,X2,X3,X4,X5,X6,X7,X8,XC,XX,XN
C------------------------------------
C     CALCUL DES BORNES DES ELEMENTS
C------------------------------------
C-----------------------------------------------------------------------
C       Face 1 2 3 4 ou 5 6 7 8
C-----------------------------------------------------------------------
      DO IDIR=1,3
C-----------------------------------------------------------------------
C         X Y ou Z
C-----------------------------------------------------------------------
        DO L=LFT,LLT
            I = INDEX(L)
            NE = NELEM(I)
C-----------------------------------------------------------------------
            J = IXS(2,NE)
            X1 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X1-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X1-XSAV(IDIR,J))
            J = IXS(3,NE)
            X2 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X2-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X2-XSAV(IDIR,J))
            J = IXS(4,NE)
            X3 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X3-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X3-XSAV(IDIR,J))
            J = IXS(5,NE)
            X4 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X4-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X4-XSAV(IDIR,J))
            J = IXS(6,NE)
            X5 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X5-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X5-XSAV(IDIR,J))
            J = IXS(7,NE)
            X6 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X6-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X6-XSAV(IDIR,J))
            J = IXS(8,NE)
            X7 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X7-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X7-XSAV(IDIR,J))
            J = IXS(9,NE)
            X8 = X(IDIR,J)+DT2*(V(IDIR,J)+DT12*A(IDIR,J))
            XMSR(IDIR)  =MAX(XMSR(IDIR)  ,X8-XSAV(IDIR,J))
            XMSR(IDIR+3)=MIN(XMSR(IDIR+3),X8-XSAV(IDIR,J))
C
           
            DX=MAX(X1,X2 ,X3 ,X4 ,X5 ,X6 ,X7 ,X8 )
            DN=MIN(X1,X2 ,X3 ,X4 ,X5 ,X6 ,X7 ,X8 )
C
            EMINX(IDIR,I)   = MIN( EMINX(IDIR,I)  , DN )
            EMINX(IDIR+3,I) = MAX( EMINX(IDIR+3,I), DX )
C
            SIZE = SIZE + DX - DN
C
        ENDDO
      ENDDO
C--------------------------------------------------------------
C
      RETURN
      END
