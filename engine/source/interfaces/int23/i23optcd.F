Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i23optcd          ../engine/source/interfaces/int23/i23optcd.F
!||--- called by ------------------------------------------------------
!||    i23main_opt_tri   ../engine/source/interfaces/intsort/i23main_opt_tri.F
!||--- calls      -----------------------------------------------------
!||    sync_data         ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    tri7box           ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I23OPTCD(CAND_E ,CAND_N ,X      ,I_STOK  ,IRECT ,
     2                   GAP    ,GAP_S  ,IGAP    ,NSN    ,STFN   ,
     3                   ITASK  ,STF    ,INACTI  ,FTXSAV ,
     4                   FTYSAV ,FTZSAV ,CAND_P  ,NIN    ,GAPMAX  ,
     5                   ICURV  ,COUNT_REMSLV ,GAP_M ,MSR    ,NSV ,
     6                   GAPMIN )
C-----------------------------------------------
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
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), CAND_E(*), CAND_N(*),
     .        I_STOK,NIN,IGAP ,ITASK, NSN, INACTI,ICURV,
     .        COUNT_REMSLV(*), MSR(*), NSV(*)
      my_real
     .        X(3,*),GAP,GAP_S(*),STFN(*),STF(*),
     .        FTXSAV(*), FTYSAV(*), FTZSAV(*), CAND_P(*),
     .        GAPMAX, GAP_M(*), GAPMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,SG,FIRST,LAST,MSEG,NLF,II,J,NLS2
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ),COUNT_CAND,CT
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ)
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX
C-----------------------------------------------
      COUNT_CAND=0
C-----------------------------------------------
      CT = 0
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      JS = FIRST-1
      DO SG = FIRST,LAST,MSEG
       NSEG = MIN(MSEG,LAST-JS)
       NLS=0

       IF(NSPMD>1) THEN
C
C Partage cand_n local / frontiere
C
         NLS = 0
         NLS2 = NSEG+1
         DO IS = 1, NSEG
           I=JS+IS
           IF(CAND_N(I)<=NSN)THEN
             NLS=NLS+1
             LISTI(NLS)=IS
           ELSE
             NLS2=NLS2-1
             LISTI(NLS2) = IS
           ENDIF
         ENDDO
         IF(IGAP==0)THEN
           DO LS = 1, NLS
             IS = LISTI(LS)
             GAPV(IS)=GAP
           ENDDO
         ELSE
           DO LS = 1, NLS
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
C             IF(IGAP==3)
C     .         GAPV(IS)=MIN(GAPV(IS),
C     .            GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
             IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
             GAPV(IS)=MAX(GAPV(IS),GAPMIN)
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
         IF(IGAP==0)THEN
          DO IS=1,NSEG
            GAPV(IS)=GAP
            LISTI(IS)=IS
          ENDDO
         ELSE
          DO IS=1,NSEG
            I=JS+IS
            GAPV(IS)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
C             IF(IGAP==3)
C     .         GAPV(IS)=MIN(GAPV(IS),
C     .            GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
            IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
            GAPV(IS)=MAX(GAPV(IS),GAPMIN)
            LISTI(IS)=IS
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
C
C necessaire pour gap TYPE5
       DO LS=NLF,NLT
         IS = LISTI(LS)
         GAPV(IS)=SQRT(THREE)*GAPV(IS)
       ENDDO
       NLS=0
       IF(ICURV/=0)THEN
        DO LS = NLF, NLT
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN 
           IG(IS) = NSV(CAND_N(I))
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
           XMIN = X0-CURV_MAX-GAPV(IS)
           YMIN = Y0-CURV_MAX-GAPV(IS)
           ZMIN = Z0-CURV_MAX-GAPV(IS)
           XMAX = X0+CURV_MAX+GAPV(IS)
           YMAX = Y0+CURV_MAX+GAPV(IS)
           ZMAX = Z0+CURV_MAX+GAPV(IS)
           IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                    CAND_N(I) = -CAND_N(I)
                    COUNT_CAND = COUNT_CAND+1
           ENDIF
          ENDIF
        ENDDO
       ELSE
        DO LS = NLF, NLT
C   conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN 
           IG(IS) = NSV(CAND_N(I))
           ZI = X(3,IG(IS))
           IX1(IS)=IRECT(1,L)
           Z1=X(3,IX1(IS))
           IX2(IS)=IRECT(2,L)
           Z2=X(3,IX2(IS))
           IX3(IS)=IRECT(3,L)
           Z3=X(3,IX3(IS))
           IX4(IS)=IRECT(4,L)
           Z4=X(3,IX4(IS))
           ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
           ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
           IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
            NLS=NLS+1
            LIST(NLS)=IS
           ENDIF
          ENDIF
        ENDDO
C
        NLT=NLS
        NLS=0
        DO LS=NLF,NLT
          IS=LIST(LS)
          YI=X(2,IG(IS))
          Y1=X(2,IX1(IS))
          Y2=X(2,IX2(IS))
          Y3=X(2,IX3(IS))
          Y4=X(2,IX4(IS))
          YMIN = MIN(Y1,Y2,Y3,Y4)-GAPV(IS)
          YMAX = MAX(Y1,Y2,Y3,Y4)+GAPV(IS)
          IF (YMIN<=YI.AND.YMAX>=YI) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C
        DO LS=NLF,NLS
          IS=LIST(LS)
          XI=X(1,IG(IS))
          X1=X(1,IX1(IS))
          X2=X(1,IX2(IS))
          X3=X(1,IX3(IS))
          X4=X(1,IX4(IS))
          XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
          XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
          IF (XMIN<=XI.AND.XMAX>=XI) THEN
            I=JS+IS
            CAND_N(I) = -CAND_N(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
        ENDDO
       ENDIF
       IF(NSPMD>1)THEN
          NLF = NLS2
          NLT = NSEG
          IF(IGAP==0)THEN
            DO LS = NLF, NLT
              IS = LISTI(LS)
              GAPV(IS)=GAP
            ENDDO
          ELSE
            DO LS = NLF, NLT
              IS = LISTI(LS)
              I=JS+IS
              GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN)+GAP_M(CAND_E(I))
              IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
              GAPV(IS)=MAX(GAPV(IS),GAPMIN)
            ENDDO
          ENDIF
C
C necessaire pour gap TYPE5
          DO LS = NLF, NLT
            IS = LISTI(LS)
            GAPV(IS)=SQRT(THREE)*GAPV(IS)
          ENDDO
C
          IF(ICURV/=0)THEN
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IF(STF(L)/=ZERO.AND.STIFI(NIN)%P(II)/=ZERO) THEN 
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
             XMIN = X0-CURV_MAX-GAPV(IS)
             YMIN = Y0-CURV_MAX-GAPV(IS)
             ZMIN = Z0-CURV_MAX-GAPV(IS)
             XMAX = X0+CURV_MAX+GAPV(IS)
             YMAX = Y0+CURV_MAX+GAPV(IS)
             ZMAX = Z0+CURV_MAX+GAPV(IS)
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
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IF(STF(L)/=ZERO.AND.STIFI(NIN)%P(II)/=ZERO) THEN 
              ZI = XFI(NIN)%P(3,II)
              IX1(IS)=IRECT(1,L)
              Z1=X(3,IX1(IS))
              IX2(IS)=IRECT(2,L)
              Z2=X(3,IX2(IS))
              IX3(IS)=IRECT(3,L)
              Z3=X(3,IX3(IS))
              IX4(IS)=IRECT(4,L)
              Z4=X(3,IX4(IS))
              ZMIN = MIN(Z1,Z2,Z3,Z4)-GAPV(IS)
              ZMAX = MAX(Z1,Z2,Z3,Z4)+GAPV(IS)
              IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
                NLS=NLS+1
                LIST(NLS)=IS
              ENDIF
            ENDIF
           ENDDO
C
           NLF=1
           NLT=NLS
           NLS=0
           DO LS=NLF,NLT
            IS=LIST(LS)
            I=JS+IS
            II=CAND_N(I)-NSN
            YI=XFI(NIN)%P(2,II)
            Y1=X(2,IX1(IS))
            Y2=X(2,IX2(IS))
            Y3=X(2,IX3(IS))
            Y4=X(2,IX4(IS))
            YMIN = MIN(Y1,Y2,Y3,Y4)-GAPV(IS)
            YMAX = MAX(Y1,Y2,Y3,Y4)+GAPV(IS)
            IF (YMIN<=YI.AND.YMAX>=YI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ENDIF
           ENDDO
C
           DO LS=NLF,NLS
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            XI = XFI(NIN)%P(1,II)
            X1=X(1,IX1(IS))
            X2=X(1,IX2(IS))
            X3=X(1,IX3(IS))
            X4=X(1,IX4(IS))
            XMIN = MIN(X1,X2,X3,X4)-GAPV(IS)
            XMAX = MAX(X1,X2,X3,X4)+GAPV(IS)
            IF (XMIN<=XI.AND.XMAX>=XI) THEN
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
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+CT 
#include "lockoff.inc"
C
C-----------------------------------------------
      RETURN
      END
