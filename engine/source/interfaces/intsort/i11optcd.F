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
Chd|====================================================================
Chd|  I11OPTCD                      source/interfaces/intsort/i11optcd.F
Chd|-- called by -----------
Chd|        I11MAIN_OPT_TRI               source/interfaces/intsort/i11main_opt_tri.F
Chd|-- calls ---------------
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11OPTCD(CAND_M,CAND_S,X   ,I_STOK,IRECTS  ,
     2                    IRECTM,GAP   ,NIN ,V     ,GAP_S   ,
     3                    GAP_M ,IGAP  ,STFS,ITASK ,NRTS    ,
     4                    STFM  ,GAP_S_L,GAP_M_L,COUNT_REMSLV,
     5                    IFPEN ,CAND_FX,CAND_FY,CAND_FZ,IFORM,
     6                    DRAD  ,DGAPLOAD)
C============================================================================
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECTS(2,*),IRECTM(2,*), CAND_M(*), CAND_S(*),
     .        I_STOK, NIN,IGAP ,ITASK, NRTS,COUNT_REMSLV(*), 
     .        IFPEN(*),IFORM
      my_real
     .        X(3,*),GAP,GAP_S(*),GAP_M(*),V(3,*),STFS(*), STFM(*),
     .        GAP_S_L(*),GAP_M_L(*),CAND_FX(*),CAND_FY(*),CAND_FZ(*)
      my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , L, NN1, NN2
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMINS,XMAXS,YMINS,YMAXS,ZMINS,ZMAXS,
     .        XMINM,XMAXM,YMINM,YMAXM,ZMINM,ZMAXM,
     .        V12,V22,V32,V42,VV,GAPVD
      INTEGER MSEG,CT
      my_real
     .        GAPV(MVSIZ),DTTI(MVSIZ)
      INTEGER LIST(MVSIZ), LISTI(MVSIZ)
      INTEGER IS,JS,LS,NLS,NLT,NSEG,NLF,II,NLS2
      INTEGER N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      INTEGER SG, FIRST, LAST,COUNT_CAND
C-----------------------------------------------
      COUNT_CAND = 0
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
           IF(CAND_S(I)<=NRTS)THEN
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
             GAPV(IS)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
             IF(IGAP==3)
     .         GAPV(IS)=MIN(GAPV(IS),
     .            GAP_S_L(CAND_S(I))+GAP_M_L(CAND_M(I)))
             GAPV(IS)=MAX(GAPV(IS),GAP)
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
            GAPV(IS)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
             IF(IGAP==3)
     .         GAPV(IS)=MIN(GAPV(IS),
     .            GAP_S_L(CAND_S(I))+GAP_M_L(CAND_M(I)))
            GAPV(IS)=MAX(GAPV(IS),GAP)
            LISTI(IS)=IS
          ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
#include      "vectorize.inc"
       DO LS = NLF, NLT
         IS = LISTI(LS)
         I=JS+IS
         L  = CAND_S(I)
         IF (STFS(L)/=ZERO) THEN
           N1(IS)=IRECTS(1,L)
           Z1=X(3,N1(IS))
           N2(IS)=IRECTS(2,L)
           Z2=X(3,N2(IS))
           L  = CAND_M(I)
           IF (STFM(L)/=ZERO) THEN

            M1(IS)=IRECTM(1,L)
            Z3=X(3,M1(IS))
            M2(IS)=IRECTM(2,L)
            Z4=X(3,M2(IS))
            GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
            ZMINS = MIN(Z1,Z2)-GAPVD
            ZMAXS = MAX(Z1,Z2)+GAPVD
            ZMINM = MIN(Z3,Z4)-GAPVD
            ZMAXM = MAX(Z3,Z4)+GAPVD
            IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
             NLS=NLS+1
             LIST(NLS)=IS
            ENDIF
           ENDIF
         ENDIF
       ENDDO
C
       NLT=NLS
       NLS=0
#include      "vectorize.inc"
       DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          L  = CAND_S(I)
          N1(IS)=IRECTS(1,L)
          Y1=X(2,N1(IS))
          N2(IS)=IRECTS(2,L)
          Y2=X(2,N2(IS))
          L  = CAND_M(I)
          M1(IS)=IRECTM(1,L)
          Y3=X(2,M1(IS))
          M2(IS)=IRECTM(2,L)
          Y4=X(2,M2(IS))
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          YMINS = MIN(Y1,Y2)-GAPVD
          YMAXS = MAX(Y1,Y2)+GAPVD
          YMINM = MIN(Y3,Y4)-GAPVD
          YMAXM = MAX(Y3,Y4)+GAPVD
          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
       ENDDO
C
#include      "vectorize.inc"
       DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          L  = CAND_S(I)
          N1(IS)=IRECTS(1,L)
          X1=X(1,N1(IS))
          N2(IS)=IRECTS(2,L)
          X2=X(1,N2(IS))
          L  = CAND_M(I)
          M1(IS)=IRECTM(1,L)
          X3=X(1,M1(IS))
          M2(IS)=IRECTM(2,L)
          X4=X(1,M2(IS))
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          XMINS = MIN(X1,X2)-GAPVD
          XMAXS = MAX(X1,X2)+GAPVD
          XMINM = MIN(X3,X4)-GAPVD
          XMAXM = MAX(X3,X4)+GAPVD
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
          ENDIF
       ENDDO
C
       IF(NSPMD>1)THEN
         NLF = NLS2
         NLT = NSEG
         IF(IGAP==0)THEN
          DO LS=NLF, NLT
            IS = LISTI(LS)
            GAPV(IS)=GAP
          ENDDO
         ELSE
          DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            GAPV(IS)=GAPFI(NIN)%P(CAND_S(I)-NRTS)+GAP_M(CAND_M(I))
            IF(IGAP==3)
     .        GAPV(IS)=MIN(GAPV(IS),
     .        GAP_LFI(NIN)%P(CAND_S(I)-NRTS)+
     .        MAX(GAP_M(CAND_M(I)),GAP_M_L(CAND_M(I))))
            GAPV(IS)=MAX(GAPV(IS),GAP)
          ENDDO
         ENDIF
C
         NLS=0
#include      "vectorize.inc"
         DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          II = CAND_S(I)-NRTS
          IF (STIFI(NIN)%P(II)/=ZERO) THEN
           NN1 = 2*(II-1)+1
           NN2 = 2*II
           Z1=XFI(NIN)%P(3,NN1)
           Z2=XFI(NIN)%P(3,NN2)
           L  = CAND_M(I)
           IF (STFM(L)/=ZERO) THEN
            M1(IS)=IRECTM(1,L)
            Z3=X(3,M1(IS))
            M2(IS)=IRECTM(2,L)
            Z4=X(3,M2(IS))
            GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
            ZMINS = MIN(Z1,Z2)-GAPVD
            ZMAXS = MAX(Z1,Z2)+GAPVD
            ZMINM = MIN(Z3,Z4)-GAPVD
            ZMAXM = MAX(Z3,Z4)+GAPVD
            IF (ZMAXS>=ZMINM.AND.ZMAXM>=ZMINS) THEN
             NLS=NLS+1
             LIST(NLS)=IS
            ENDIF
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
          I=JS+IS
          II = CAND_S(I)-NRTS
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          Y1=XFI(NIN)%P(2,NN1)
          Y2=XFI(NIN)%P(2,NN2)
          L  = CAND_M(I)
          M1(IS)=IRECTM(1,L)
          Y3=X(2,M1(IS))
          M2(IS)=IRECTM(2,L)
          Y4=X(2,M2(IS))
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          YMINS = MIN(Y1,Y2)-GAPVD
          YMAXS = MAX(Y1,Y2)+GAPVD
          YMINM = MIN(Y3,Y4)-GAPVD
          YMAXM = MAX(Y3,Y4)+GAPVD
          IF (YMAXS>=YMINM.AND.YMAXM>=YMINS) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C
#include      "vectorize.inc"
        DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          II = CAND_S(I)-NRTS
          NN1 = 2*(II-1)+1
          NN2 = 2*II
          X1=XFI(NIN)%P(1,NN1)
          X2=XFI(NIN)%P(1,NN2)
          L  = CAND_M(I)
          M1(IS)=IRECTM(1,L)
          X3=X(1,M1(IS))
          M2(IS)=IRECTM(2,L)
          X4=X(1,M2(IS))
          GAPVD = MAX(GAPV(IS)+DGAPLOAD,DRAD)
          XMINS = MIN(X1,X2)-GAPVD
          XMAXS = MAX(X1,X2)+GAPVD
          XMINM = MIN(X3,X4)-GAPVD
          XMAXM = MAX(X3,X4)+GAPVD
          IF (XMAXS>=XMINM.AND.XMAXM>=XMINS) THEN
            CAND_S(I) = -CAND_S(I)
            COUNT_CAND = COUNT_CAND+1
            CT = CT+1
          ENDIF
        ENDDO
        CALL SYNC_DATA(NLS2)
       END IF
       JS = JS + NSEG
      ENDDO
      IF (ITASK == 0 .AND. IFORM == 2) THEN
        DO I=1,I_STOK
          IF (IFPEN(I) == 0 ) THEN
            CAND_FX(I) = ZERO
            CAND_FY(I) = ZERO
            CAND_FZ(I) = ZERO
          ENDIF
          IFPEN(I) = 0
        ENDDO
      ENDIF
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+CT
#include "lockoff.inc"

C
      RETURN
      END

