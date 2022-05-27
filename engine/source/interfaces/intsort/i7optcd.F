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
Chd|  I7OPTCD                       source/interfaces/intsort/i7optcd.F
Chd|-- called by -----------
Chd|        I7MAIN_OPT_TRI                source/interfaces/intsort/i7main_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7OPTCD(NSV            ,CAND_E    ,CAND_N     ,X          ,I_STOK ,
     2                   IRECT          ,GAP       ,GAP_S      ,GAP_M      ,IGAP   ,
     3                   STFN           ,ITASK     ,STF        ,IFQ        ,IFPEN  ,
     4                   CAND_FX        ,CAND_FY   ,CAND_FZ    ,NIN        ,NSN    ,
     5                   GAPMAX         ,ICURV     ,GAP_S_L    ,GAP_M_L    ,
     6                   COUNT_REMSLV   ,GAPMIN    ,DRAD       ,DGAPLOAD   )
C===============================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), IFPEN(*),
     .        I_STOK,NIN,IGAP ,ITASK, NSN, IFQ,ICURV,COUNT_REMSLV(*)
      my_real
     .        X(3,*),GAP,GAP_S(*),GAP_M(*),STFN(*),STF(*),
     .        CAND_FX(*),CAND_FY(*),CAND_FZ(*),
     .        GAPMAX,GAP_S_L(*),GAP_M_L(*),GAPMIN
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,NLS2,SG,FIRST,LAST,MSEG,NLF,II
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ),COUNT_CAND,CT
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ)
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX,GAPF

C-----------------------------------------------
      COUNT_CAND=0
      CT = 0
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      JS = FIRST-1
      DO SG = FIRST,LAST,MSEG
       NSEG = MIN(MSEG,LAST-JS)
       NLS=0

       IF(NSPMD.GT.1) THEN
C
C Partage cand_n local / frontiere
C
         NLS = 0
         NLS2 = NSEG+1
         DO IS = 1, NSEG
           I=JS+IS
           IF(CAND_N(I).LE.NSN)THEN
             NLS=NLS+1
             LISTI(NLS)=IS
           ELSE
             NLS2=NLS2-1
             LISTI(NLS2) = IS
           ENDIF
         ENDDO
         IF(IGAP.EQ.0)THEN
           DO LS = 1, NLS
             IS = LISTI(LS)
             GAPV(IS)=GAP
           ENDDO
         ELSE
           DO LS = 1, NLS
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
             IF(IGAP==3)
     .         GAPV(IS)=MIN(GAPV(IS),
     .            GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
             GAPV(IS)=MIN(GAPV(IS),GAPMAX)
             GAPV(IS)=MAX(GAPV(IS),GAPMIN)
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
         IF(IGAP.EQ.0)THEN
          DO IS=1,NSEG
            GAPV(IS)=GAP
            LISTI(IS)=IS
          ENDDO
         ELSE
          DO IS=1,NSEG
            I=JS+IS
            GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
             IF(IGAP==3)
     .         GAPV(IS)=MIN(GAPV(IS),
     .            GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
            GAPV(IS)=MIN(GAPV(IS),GAPMAX)
            GAPV(IS)=MAX(GAPV(IS),GAPMIN)
            LISTI(IS)=IS
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
       IF(ICURV/=0)THEN
#include      "vectorize.inc"
        DO LS = NLF, NLT
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L).NE.ZERO.AND.STFN(CAND_N(I)).NE.ZERO) THEN 
           IG(IS) = NSV(CAND_N(I))
           GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
           XI = X(1,IG(IS))
           YI = X(2,IG(IS))
           ZI = X(3,IG(IS))
           IX1(IS)=IRECT(1,L)
           IX2(IS)=IRECT(2,L)
           IX3(IS)=IRECT(3,L)
           IX4(IS)=IRECT(4,L)
           X1=X(1,IX1(IS))
           X2=X(1,IX2(IS))
           X3=X(1,IX3(IS))
           X4=X(1,IX4(IS))
           Y1=X(2,IX1(IS))
           Y2=X(2,IX2(IS))
           Y3=X(2,IX3(IS))
           Y4=X(2,IX4(IS))
           Z1=X(3,IX1(IS))
           Z2=X(3,IX2(IS))
           Z3=X(3,IX3(IS))
           Z4=X(3,IX4(IS))
           X0 = FOURTH*(X1+X2+X3+X4)
           Y0 = FOURTH*(Y1+Y2+Y3+Y4)
           Z0 = FOURTH*(Z1+Z2+Z3+Z4)
           XXX=MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4)
           YYY=MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4)
           ZZZ=MAX(Z1,Z2,Z3,Z4)-MIN(Z1,Z2,Z3,Z4)
           CURV_MAX = HALF * MAX(XXX,YYY,ZZZ)
           XMIN = X0-CURV_MAX-GAPF
           YMIN = Y0-CURV_MAX-GAPF
           ZMIN = Z0-CURV_MAX-GAPF
           XMAX = X0+CURV_MAX+GAPF
           YMAX = Y0+CURV_MAX+GAPF
           ZMAX = Z0+CURV_MAX+GAPF
           IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                    CAND_N(I) = -CAND_N(I)
                    COUNT_CAND = COUNT_CAND+1
           ENDIF
          ENDIF
        ENDDO
       ELSE
#include      "vectorize.inc"
        DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L).NE.ZERO.AND.STFN(CAND_N(I)).NE.ZERO) THEN 
           IG(IS) = NSV(CAND_N(I))
           GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD) 
           ZI = X(3,IG(IS))
           IX1(IS)=IRECT(1,L)
           Z1=X(3,IX1(IS))
           IX2(IS)=IRECT(2,L)
           Z2=X(3,IX2(IS))
           IX3(IS)=IRECT(3,L)
           Z3=X(3,IX3(IS))
           IX4(IS)=IRECT(4,L)
           Z4=X(3,IX4(IS))
           ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPF
           ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPF
           YI=X(2,IG(IS))
           Y1=X(2,IX1(IS))
           Y2=X(2,IX2(IS))
           Y3=X(2,IX3(IS))
           Y4=X(2,IX4(IS))
           YMIN = MIN(Y1,Y2,Y3,Y4)-GAPF
           YMAX = MAX(Y1,Y2,Y3,Y4)+GAPF
           XI=X(1,IG(IS))
           X1=X(1,IX1(IS))
           X2=X(1,IX2(IS))
           X3=X(1,IX3(IS))
           X4=X(1,IX4(IS))
           XMIN = MIN(X1,X2,X3,X4)-GAPF
           XMAX = MAX(X1,X2,X3,X4)+GAPF
           IF (ZMIN.LE.ZI.AND.ZMAX.GE.ZI) THEN
                IF (YMIN.LE.YI.AND.YMAX.GE.YI) THEN
                        IF (XMIN.LE.XI.AND.XMAX.GE.XI) THEN
                                I=JS+IS
                                CAND_N(I) = -CAND_N(I)
                                COUNT_CAND = COUNT_CAND+1
                        ENDIF
                ENDIF
           ENDIF
         ENDIF
        ENDDO
       ENDIF

       IF(NSPMD.GT.1)THEN
          NLF = NLS2
          NLT = NSEG
          IF(IGAP.EQ.0)THEN
            DO LS = NLF, NLT
              IS = LISTI(LS)
              GAPV(IS)=GAP
            ENDDO
          ELSE
            IF(GAPMAX.NE.ZERO)THEN
              DO LS = NLF, NLT
                IS = LISTI(LS)
                I=JS+IS
                GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
                IF(IGAP==3)
     .            GAPV(IS)=MIN(GAPV(IS),
     .            GAP_LFI(NIN)%P(CAND_N(I)-NSN)+
     .            MAX(GAP_M(CAND_E(I)),GAP_M_L(CAND_E(I))))
                GAPV(IS)=MIN(GAPV(IS),GAPMAX)
                GAPV(IS)=MAX(GAPV(IS),GAPMIN)
              ENDDO
            ELSE
              DO LS = NLF, NLT
                IS = LISTI(LS)
                I=JS+IS
                GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
                IF(IGAP==3)
     .            GAPV(IS)=MIN(GAPV(IS),
     .            GAP_LFI(NIN)%P(CAND_N(I)-NSN)+
     .            MAX(GAP_M(CAND_E(I)),GAP_M_L(CAND_E(I))))
                GAPV(IS)=MAX(GAPV(IS),GAPMIN)
              ENDDO
            ENDIF
          ENDIF
          IF(ICURV/=0)THEN
#include      "vectorize.inc"
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IF(STF(L).NE.ZERO.AND.STIFI(NIN)%P(II).NE.ZERO) THEN 
             GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
             XI = XFI(NIN)%P(1,II)
             YI = XFI(NIN)%P(2,II)
             ZI = XFI(NIN)%P(3,II)
             IX1(IS)=IRECT(1,L)
             IX2(IS)=IRECT(2,L)
             IX3(IS)=IRECT(3,L)
             IX4(IS)=IRECT(4,L)
             X1=X(1,IX1(IS))
             X2=X(1,IX2(IS))
             X3=X(1,IX3(IS))
             X4=X(1,IX4(IS))
             Y1=X(2,IX1(IS))
             Y2=X(2,IX2(IS))
             Y3=X(2,IX3(IS))
             Y4=X(2,IX4(IS))
             Z1=X(3,IX1(IS))
             Z2=X(3,IX2(IS))
             Z3=X(3,IX3(IS))
             Z4=X(3,IX4(IS))
             X0 = FOURTH*(X1+X2+X3+X4)
             Y0 = FOURTH*(Y1+Y2+Y3+Y4)
             Z0 = FOURTH*(Z1+Z2+Z3+Z4)
             XXX=MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4)
             YYY=MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4)
             ZZZ=MAX(Z1,Z2,Z3,Z4)-MIN(Z1,Z2,Z3,Z4)
             CURV_MAX = HALF * MAX(XXX,YYY,ZZZ)
             XMIN = X0-CURV_MAX-GAPF
             YMIN = Y0-CURV_MAX-GAPF
             ZMIN = Z0-CURV_MAX-GAPF
             XMAX = X0+CURV_MAX+GAPF
             YMAX = Y0+CURV_MAX+GAPF
             ZMAX = Z0+CURV_MAX+GAPF
             IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                   CAND_N(I) = -CAND_N(I)
                   COUNT_CAND = COUNT_CAND + 1
                   CT = CT +1
             ENDIF
            END IF
           END DO
          ELSE
           NLS=0
#include      "vectorize.inc"
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IF(STF(L).NE.ZERO.AND.STIFI(NIN)%P(II).NE.ZERO) THEN 
              GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
              ZI = XFI(NIN)%P(3,II)
              IX1(IS)=IRECT(1,L)
              Z1=X(3,IX1(IS))
              IX2(IS)=IRECT(2,L)
              Z2=X(3,IX2(IS))
              IX3(IS)=IRECT(3,L)
              Z3=X(3,IX3(IS))
              IX4(IS)=IRECT(4,L)
              Z4=X(3,IX4(IS))
              ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPF
              ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPF
              IF (ZMIN.LE.ZI.AND.ZMAX.GE.ZI) THEN
                NLS=NLS+1
                LIST(NLS)=IS
              ENDIF
            ENDIF
           ENDDO
C
           NLF=1
           NLT=NLS
           NLS=0
#include      "vectorize.inc"
           DO LS=NLF,NLT
            IS=LIST(LS)
            GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
            I=JS+IS
            II=CAND_N(I)-NSN
            YI=XFI(NIN)%P(2,II)
            Y1=X(2,IX1(IS))
            Y2=X(2,IX2(IS))
            Y3=X(2,IX3(IS))
            Y4=X(2,IX4(IS))
            YMIN = MIN(Y1,Y2,Y3,Y4)-GAPF
            YMAX = MAX(Y1,Y2,Y3,Y4)+GAPF
            IF (YMIN.LE.YI.AND.YMAX.GE.YI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ENDIF
           ENDDO
C
#include      "vectorize.inc"
           DO LS=NLF,NLS
            IS=LIST(LS)
            GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
            I=JS+IS
            II = CAND_N(I)-NSN
            XI = XFI(NIN)%P(1,II)
            X1=X(1,IX1(IS))
            X2=X(1,IX2(IS))
            X3=X(1,IX3(IS))
            X4=X(1,IX4(IS))
            XMIN = MIN(X1,X2,X3,X4)-GAPF
            XMAX = MAX(X1,X2,X3,X4)+GAPF
            IF (XMIN.LE.XI.AND.XMAX.GE.XI) THEN
              CAND_N(I) = -CAND_N(I)
              COUNT_CAND = COUNT_CAND+1
              CT = CT + 1
            ENDIF
           ENDDO
          END IF
       ELSE
         CALL SYNC_DATA(NLS2)
       ENDIF
       JS = JS + NSEG
      ENDDO
      IF (IFQ > 0) THEN
        DO I=FIRST,LAST
          IF (IFPEN(I) == 0 .AND. IMCONV == 1) THEN
            CAND_FX(I) = ZERO
            CAND_FY(I) = ZERO
            CAND_FZ(I) = ZERO
          ENDIF
          IFPEN(I) = 0
        ENDDO
      ENDIF
C
      IF(COUNT_CAND > 0 .OR. CT > 0) THEN
#include "lockon.inc"
        LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
        COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+CT 
#include "lockoff.inc"
      ENDIF

      RETURN
      END
