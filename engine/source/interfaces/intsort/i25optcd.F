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
Chd|  I25OPTCD                      source/interfaces/intsort/i25optcd.F
Chd|-- called by -----------
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25OPTCD(
     1       NSV    ,CAND_E ,CAND_N  ,X         ,I_STOK   ,
     2       IRECT  ,GAP_S  ,GAP_M   ,V         ,ICURV    ,
     3       STFN   ,ITASK  ,STF     ,NIN       ,NSN      ,
     4       IRTLM  ,TIME_S ,MSEGLO  ,COUNT_REMSLV,ITAB   ,
     5       SECND_FR,NSNR   ,PENE_OLD,STIF_OLD  ,MSEGTYP  ,
     6       NRTM   ,PMAX_GAP,I_OPT_STOK,CAND_OPT_E,CAND_OPT_N ,
     7       SIZOPT ,IGAP   ,GAP_S_L ,GAP_M_L    ,DRAD    ,
     8       DGAPLOAD,ICONT_I)
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSNR,I_STOK,NIN ,ITASK, NSN, ICURV, I_OPT_STOK, NRTM,
     .        IGAP, IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        IRTLM(4,NSN),MSEGLO(*),COUNT_REMSLV(*), ITAB(*),
     .        MSEGTYP(*), CAND_OPT_E(*), CAND_OPT_N(*), SIZOPT
      INTEGER , INTENT(INOUT) :: ICONT_I(NSN)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .        GAP,PMAX_GAP,
     .        X(3,*),GAP_S(*),GAP_M(*),STFN(*),STF(*),
     .        V(3,*),SECND_FR(6,*),TIME_S(*),
     .        PENE_OLD(5,NSN),STIF_OLD(2,NSN),
     .        GAP_S_L(*),GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,NLS2,SG,FIRST,LAST,MSEG,NLF,
     .        MG,II,N,KLEAVE, ISH, NSNRF,NSNRL,NSNF,NSNL
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ)
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ),PREC
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX,TZINF,VX,VY,VZ,VV,
     .        VXI,VYI,VZI,
     .        VX1,VX2,VX3,VX4,VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4

C-----------------------------------------------
      IF (IRESP.EQ.1) THEN
         PREC = (SEVEN+HALF)*EM06
      ELSE
         PREC = EM8
      ENDIF
C------
C initialize PMAX_GAP to Zero for future treatments in force computation.
      PMAX_GAP   = ZERO
C
C CONT_I contains Starter infos, it must be flushed when IRTLM(1,xxx) is no more Zero.
C

      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      DO I = NSNF,NSNL 
        IF(IRTLM(1,I)/=0) ICONT_I(I)=0
      ENDDO

      NSNRF = 1 + ITASK*NSNR / NTHREAD
      NSNRL = (ITASK+1)*NSNR / NTHREAD

      DO I=NSNRF,NSNRL
        IF(IRTLM_FI(NIN)%P(1,I)/=0) ICONT_I_FI(NIN)%P(I)=0
      ENDDO
 
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
         DO LS = 1, NLS
           IS = LISTI(LS)
           I=JS+IS
           GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),
     .     	GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
         ENDDO
       ELSE
         NLS = NSEG
         DO IS=1,NSEG
           I=JS+IS
           GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),
     .     	GAP_S_L(CAND_N(I))+GAP_M_L(CAND_E(I)))
           LISTI(IS)=IS
         ENDDO
       ENDIF
C
       NLF = 1
       NLT = NLS
       NLS=0
#include      "vectorize.inc"
        DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          N  = CAND_N(I)
          MG = IRTLM(1,N)
          KLEAVE=IRTLM(3,N)
          IF(STFN(N)/=ZERO.AND.STF(L)/=ZERO.AND.MG==0.AND.KLEAVE/=-1)THEN
c else, keep only candidates wrt non impacted nodes
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
            VZI = V(3,IG(IS))
            VZ1=V(3,IX1(IS))
            VZ2=V(3,IX2(IS))
            VZ3=V(3,IX3(IS))
            VZ4=V(3,IX4(IS))
            VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4))
C
            TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VZ*DT1)
            TZINF = MAX(PREC,TZINF)
            ZMIN = MIN(Z1,Z2,Z3,Z4)-TZINF
            ZMAX = MAX(Z1,Z2,Z3,Z4)+TZINF
            IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
             NLS=NLS+1
             LIST(NLS)=IS
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
          YI=X(2,IG(IS))
          Y1=X(2,IX1(IS))
          Y2=X(2,IX2(IS))
          Y3=X(2,IX3(IS))
          Y4=X(2,IX4(IS))
          VYI = V(2,IG(IS))
          VY1=V(2,IX1(IS))
          VY2=V(2,IX2(IS))
          VY3=V(2,IX3(IS))
          VY4=V(2,IX4(IS))
          VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
C
          TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VY*DT1)
          TZINF = MAX(PREC,TZINF)
          YMIN = MIN(Y1,Y2,Y3,Y4)-TZINF
          YMAX = MAX(Y1,Y2,Y3,Y4)+TZINF
          IF (YMIN<=YI.AND.YMAX>=YI) THEN
            NLS=NLS+1
            LIST(NLS)=IS
          ENDIF
        ENDDO
C
#include      "vectorize.inc"
        DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          XI=X(1,IG(IS))
          X1=X(1,IX1(IS))
          X2=X(1,IX2(IS))
          X3=X(1,IX3(IS))
          X4=X(1,IX4(IS))
          VXI = V(1,IG(IS))
          VX1=V(1,IX1(IS))
          VX2=V(1,IX2(IS))
          VX3=V(1,IX3(IS))
          VX4=V(1,IX4(IS))
          VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
C
          TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VX*DT1)
          TZINF = MAX(PREC,TZINF)
          XMIN = MIN(X1,X2,X3,X4)-TZINF
          XMAX = MAX(X1,X2,X3,X4)+TZINF
          IF (XMIN<=XI.AND.XMAX>=XI) THEN
            CAND_N(I) = -CAND_N(I)
          ENDIF
        ENDDO

C-------------------------------------------------------------
       IF(NSPMD>1)THEN
          NLF = NLS2
          NLT = NSEG
          DO LS = NLF, NLT
           IS = LISTI(LS)
           I=JS+IS
           GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN) + GAP_M(CAND_E(I))
           IF(IGAP==3)
     .       GAPV(IS)=MIN(GAPV(IS),
     .         GAP_LFI(NIN)%P(CAND_N(I)-NSN)+GAP_M_L(CAND_E(I)))
          ENDDO
C---------
          NLS=0
#include      "vectorize.inc"
          DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            MG = IRTLM_FI(NIN)%P(1,II)
            KLEAVE = IRTLM_FI(NIN)%P(3,II)
            IF(STIFI(NIN)%P(II)/=ZERO.AND.STF(L)/=ZERO.AND.MG==0.AND.KLEAVE/=-1)THEN
              ZI = XFI(NIN)%P(3,II)                                     
              IX1(IS)=IRECT(1,L)                                        
              Z1=X(3,IX1(IS))                                           
              IX2(IS)=IRECT(2,L)                                        
              Z2=X(3,IX2(IS))                                           
              IX3(IS)=IRECT(3,L)                                        
              Z3=X(3,IX3(IS))                                           
              IX4(IS)=IRECT(4,L)                                        
              Z4=X(3,IX4(IS))                                           
              VZI = VFI(NIN)%P(3,II)                                    
              VZ1=V(3,IX1(IS))                                          
              VZ2=V(3,IX2(IS))                                          
              VZ3=V(3,IX3(IS))                                          
              VZ4=V(3,IX4(IS))                                          
              VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4)) 
C
              TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VZ*DT1)
              TZINF = MAX(PREC,TZINF)
              ZMIN = MIN(Z1,Z2,Z3,Z4)-TZINF                             
              ZMAX = MAX(Z1,Z2,Z3,Z4)+TZINF                             
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
#include      "vectorize.inc"
          DO LS=NLF,NLT
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            YI=XFI(NIN)%P(2,II)
            Y1=X(2,IX1(IS))
            Y2=X(2,IX2(IS))
            Y3=X(2,IX3(IS))
            Y4=X(2,IX4(IS))
            VYI = VFI(NIN)%P(2,II)
            VY1=V(2,IX1(IS))
            VY2=V(2,IX2(IS))
            VY3=V(2,IX3(IS))
            VY4=V(2,IX4(IS))
            VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
C
            TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VY*DT1)
            TZINF = MAX(PREC,TZINF)
            YMIN = MIN(Y1,Y2,Y3,Y4)-TZINF
            YMAX = MAX(Y1,Y2,Y3,Y4)+TZINF
            IF (YMIN<=YI.AND.YMAX>=YI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ENDIF
          ENDDO
C
#include      "vectorize.inc"
          DO LS=NLF,NLS
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            XI = XFI(NIN)%P(1,II)
            X1=X(1,IX1(IS))
            X2=X(1,IX2(IS))
            X3=X(1,IX3(IS))
            X4=X(1,IX4(IS))
            VXI = VFI(NIN)%P(1,II)
            VX1=V(1,IX1(IS))
            VX2=V(1,IX2(IS))
            VX3=V(1,IX3(IS))
            VX4=V(1,IX4(IS))
            VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
C
            TZINF = MAX(MAX(GAPV(IS)+DGAPLOAD,DRAD),ONEP01*VX*DT1)
            TZINF = MAX(PREC,TZINF)
            XMIN = MIN(X1,X2,X3,X4)-TZINF
            XMAX = MAX(X1,X2,X3,X4)+TZINF
            IF (XMIN<=XI.AND.XMAX>=XI) THEN
              CAND_N(I) = -CAND_N(I)
            ENDIF
          ENDDO
       ELSE
         CALL SYNC_DATA(NLS2)
       ENDIF
       JS = JS + NSEG
      ENDDO
C
C     Compact candidates
C
      CALL MY_BARRIER
C
!$OMP SINGLE
      DO I=1,I_STOK
        IF(CAND_N(I)<0)THEN
          CAND_N(I) = -CAND_N(I)
          I_OPT_STOK= I_OPT_STOK + 1
          IF(I_OPT_STOK <= SIZOPT)THEN
            CAND_OPT_N(I_OPT_STOK)= CAND_N(I)
            CAND_OPT_E(I_OPT_STOK)= CAND_E(I)
          END IF
        END IF
      END DO
      IF(I_OPT_STOK <= SIZOPT)THEN
        LSKYI_COUNT=LSKYI_COUNT+I_OPT_STOK*5
        COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+I_OPT_STOK
      END IF
!$OMP END SINGLE
C
c     print *,'i_stok',ispmd+1,i_stok,i_opt_stok
      RETURN
      END
