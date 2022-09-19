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
Chd|  I23COR3                       source/interfaces/int23/i23cor3.F
Chd|-- called by -----------
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I23COR3(JLT   ,NIN    ,X       ,IRECT ,NSN   ,
     2                NSV      ,CAND_E ,CAND_N  ,STF   ,STFN  ,
     3                MSR      ,MS       ,V      ,XI	,YI    ,
     4  	      ZI    ,IX1      ,IX2    ,IX3	,IX4   ,
     5  	      NSVG  ,IGSTI    ,STIF   ,KMIN	,KMAX  ,
     6  	      IGAP  ,GAP      ,GAP_S  ,GAPV	,GAPMAX,
     7  	      GAPMIN,GAP_M    ,VXI    ,VYI      ,VZI   ,
     8  	      MSI   ,NODNX_SMS,NSMS   ,KINET   ,X1     ,
     9  	      Y1    ,Z1    ,X2       ,Y2     ,Z2	,
     A  	      X3    ,Y3    ,Z3       ,X4     ,Y4	,
     B  	      Z4    ,NX1   ,NX2      ,NX3    ,NX4	,
     C                NY1   ,NY2   ,NY3      ,NY4    ,NZ1	,
     D                NZ2   ,NZ3   ,NZ4      ,KINI   ,INDEX     )
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),KINET(*),KINI(*),
     .        MSR(*), NODNX_SMS(*), INDEX(*),
     .        JLT, NOINT, IGAP , NSN, NIN, IGSTI
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), NSMS(MVSIZ)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*), GAP_S(*), GAP_M(*),
     .   MS(*), V(3,*),
     .   GAP, KMIN, KMAX, GAPMAX, GAPMIN
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .   NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .   NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .   NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .   PENE(MVSIZ),STIF(MVSIZ) ,GAPV(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,IL, L, IG, ITMP, NN, NI
C-----------------------------------------------
      IF(IGAP==0)THEN
       DO I=1,JLT
         GAPV(I)=GAP
       ENDDO
      ELSE
         DO I=1,JLT
          IF(CAND_N(I)<=NSN) THEN
            GAPV(I)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
          ELSE
            GAPV(I)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
          ENDIF
           GAPV(I)=MIN(GAPV(I),GAPMAX)
           GAPV(I)=MAX(GAPMIN,GAPV(I))
         END DO
      ENDIF
C
      DO I=1,JLT
        NI = CAND_N(I)
        IF(NI<=NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
C Kini is not used in TYPE23
          KINI(I) = KINET(IG)
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
          KINI(I) = KINFI(NIN)%P(NN)
          XI(I) = XFI(NIN)%P(1,NN)
          YI(I) = XFI(NIN)%P(2,NN)
          ZI(I) = XFI(NIN)%P(3,NN)
          VXI(I)= VFI(NIN)%P(1,NN)
          VYI(I)= VFI(NIN)%P(2,NN)
          VZI(I)= VFI(NIN)%P(3,NN)
          MSI(I)= MSFI(NIN)%P(NN)
        END IF
      END DO
C
       DO I=1,JLT
        L=CAND_E(I)
C
        IX1(I)=IRECT(1,L)
        IX2(I)=IRECT(2,L)
        IX3(I)=IRECT(3,L)
        IX4(I)=IRECT(4,L)
       END DO
C
       DO I=1,JLT
        L=CAND_E(I)
C
        X1(I)=X(1,IX1(I))
        Y1(I)=X(2,IX1(I))
        Z1(I)=X(3,IX1(I))
C
        X2(I)=X(1,IX2(I))
        Y2(I)=X(2,IX2(I))
        Z2(I)=X(3,IX2(I))
C
        X3(I)=X(1,IX3(I))
        Y3(I)=X(2,IX3(I))
        Z3(I)=X(3,IX3(I))
C
        X4(I)=X(1,IX4(I))
        Y4(I)=X(2,IX4(I))
        Z4(I)=X(3,IX4(I))
C
       END DO
C
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
C
      IF(IDTMINS==2)THEN
       DO I=1,JLT
        IF(NSVG(I)>0)THEN
          NSMS(I)=NODNX_SMS(NSVG(I))
     .                       +NODNX_SMS(IX1(I))+NODNX_SMS(IX2(I))
     .                       +NODNX_SMS(IX3(I))+NODNX_SMS(IX4(I))
        ELSE
          NN=-NSVG(I)
          NSMS(I)=NODNXFI(NIN)%P(NN)
     .                       +NODNX_SMS(IX1(I))+NODNX_SMS(IX2(I))
     .                       +NODNX_SMS(IX3(I))+NODNX_SMS(IX4(I))
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
C
      RETURN
      END

