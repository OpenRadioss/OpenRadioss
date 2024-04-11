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
Chd|  I24OPTCD                      source/interfaces/intsort/i24optcd.F
Chd|-- called by -----------
Chd|        I24MAIN_OPT_TRI               source/interfaces/intsort/i24main_opt_tri.F
Chd|-- calls ---------------
Chd|        I24ISPT2_INI_OPTTRI           source/interfaces/int24/i24cor3.F
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24OPTCD(
     1       NSV    ,CAND_E ,CAND_N  ,X         ,I_STOK   ,
     2       IRECT  ,GAP_S  ,GAP_M   ,V         ,ICURV    ,
     3       STFN   ,ITASK  ,STF     ,NIN       ,NSN      ,
     4       IRTLM  ,TIME_S ,MSEGLO  ,COUNT_REMSLV,
     5       SECND_FR,NSNR   ,PENE_OLD,STIF_OLD  ,
     6       PMAX_GAP,EDGE_L2,IEDGE  ,IGSTI  ,MVOISIN ,
     7       ICONT_I ,IS2SE,IRTSE, 
     8       NSNE,NRTSE,IS2PT,ISPT2,ISEGPT,IEDG4,T2MAIN_SMS,
     9       LSKYI_SMS_NEW,DGAPLOAD)
C========================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE DEBUG_MOD
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
#include      "com04_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSNR,IEDGE,I_STOK,NIN ,ITASK, NSN, ICURV,
     .        IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        IRTLM(2,NSN),MSEGLO(*),COUNT_REMSLV(*),IGSTI ,MVOISIN(4,*),
     .        ICONT_I(*),IS2SE(2,*),IRTSE(5,*),NSNE,NRTSE,IS2PT(*),ISPT2(*),
     .        ISEGPT(*),IEDG4,T2MAIN_SMS(6,*),LSKYI_SMS_NEW
      my_real
     .        GAP,PMAX_GAP,
     .        X(3,*),GAP_S(*),GAP_M(*),STFN(*),STF(*),
     .        V(3,*),SECND_FR(6,*),TIME_S(*),EDGE_L2(*),
     .        PENE_OLD(5,NSN),STIF_OLD(2,NSN)
      my_real , INTENT(IN) :: DGAPLOAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,NLS2,SG,FIRST,LAST,MSEG,NLF,
     .        MG,II,NSNF,NSNL,N,IGL,N1,N2,N3,N4
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ),COUNT_CAND,CT,NSNRF,NSNRL,SE,E,SN,ISEDGE,
     .        COUNT_CONNEC_SMS,NS,IE,KK
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ),EDGE_L(MVSIZ),PREC
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX,TZINF,VX,VY,VZ,VV,
     .        VXI,VYI,VZI,
     .        VX1,VX2,VX3,VX4,VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4

C-----------------------------------------------
      IG(1:MVSIZ) = 0
      IF (IRESP==1) THEN
         PREC = (SEVEN+HALF)*EM06
      ELSE
         PREC = EM8
      ENDIF
c      write(iout,*)'i24optcd 1'
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      DO I = NSNF,NSNL 
c copy old friction forces
        SECND_FR(4,I)=SECND_FR(1,I)
        SECND_FR(5,I)=SECND_FR(2,I)
        SECND_FR(6,I)=SECND_FR(3,I)
c set new friction forces at 0
        SECND_FR(1,I)=ZERO
        SECND_FR(2,I)=ZERO
        SECND_FR(3,I)=ZERO

        TIME_S(I)=ZERO
        
        PENE_OLD(2,I) = PENE_OLD(1,I)
        STIF_OLD(2,I) = STIF_OLD(1,I) 
        PENE_OLD(1,I) = ZERO
        PENE_OLD(3,I) = ZERO
C        STIF_OLD(1,I) = ZERO
C CONT_I contains Starter infos, it must be flushed when IRTLM(1,xxx) is no more Zero.
        IF(IRTLM(1,I)/=0) ICONT_I(I)=0
      ENDDO
C----------------      
      IF (IGSTI/=6) THEN
       DO I = NSNF,NSNL 
        STIF_OLD(1,I) = ZERO
       ENDDO
      END IF

      NSNRF = 1 + ITASK*NSNR / NTHREAD
      NSNRL = (ITASK+1)*NSNR / NTHREAD

      DO I=NSNRF,NSNRL
        TIME_SFI(NIN)%P(I)=ZERO
        PENE_OLDFI(NIN)%P(3,I)=ZERO
C CONT_I contains Starter infos, it must be flushed when IRTLM(1,xxx) is no more Zero.
        IF(IRTLM_FI(NIN)%P(1,I)/=0) ICONT_I_FI(NIN)%P(I)=0
      ENDDO
C initialize PMAX_GAP to Zero for future treatments in force computation.
      PMAX_GAP = ZERO
 
      COUNT_CAND = 0
      COUNT_CONNEC_SMS = 0
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
         IF(IEDGE==0)THEN
           DO LS = 1, NLS
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
             EDGE_L(IS)=ZERO
           ENDDO
         ELSE
           DO LS = 1, NLS
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
             EDGE_L(IS)=EDGE_L2(CAND_N(I))
           ENDDO
         ENDIF
       ELSE
         NLS = NSEG
         IF(IEDGE==0)THEN
           DO IS=1,NSEG
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
             EDGE_L(IS)=ZERO
             LISTI(IS)=IS
           ENDDO
         ELSE
           DO IS=1,NSEG
             I=JS+IS
             GAPV(IS)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
             EDGE_L(IS)=EDGE_L2(CAND_N(I))
             LISTI(IS)=IS
           ENDDO
         ENDIF
       ENDIF
C
       NLF = 1
       NLT = NLS
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
           VXI = V(1,IG(IS))
           VYI = V(2,IG(IS))
           VZI = V(3,IG(IS))
           VX1=V(1,IX1(IS))
           VX2=V(1,IX2(IS))
           VX3=V(1,IX3(IS))
           VX4=V(1,IX4(IS))
           VY1=V(2,IX1(IS))
           VY2=V(2,IX2(IS))
           VY3=V(2,IX3(IS))
           VY4=V(2,IX4(IS))
           VZ1=V(3,IX1(IS))
           VZ2=V(3,IX2(IS))
           VZ3=V(3,IX3(IS))
           VZ4=V(3,IX4(IS))
           VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
           VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
           VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4))
           VV = ONEP01*MAX(VX,VY,VZ)
c PMAX_GAP is not used here (node not in contact)
           TZINF = MAX(CURV_MAX+GAPV(IS)+DGAPLOAD,VV*DT1,EDGE_L(IS))
           TZINF = MAX(PREC,TZINF)
           XMIN = X0-TZINF
           YMIN = Y0-TZINF
           ZMIN = Z0-TZINF
           XMAX = X0+TZINF
           YMAX = Y0+TZINF
           ZMAX = Z0+TZINF
           IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
             CAND_N(I) = -CAND_N(I)
C
             IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
               COUNT_CAND = COUNT_CAND+1
               IF(IG(IS) > NUMNOD) COUNT_CAND = COUNT_CAND + 3
             ELSE
CC--         Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
               IF(IG(IS) > NUMNOD) THEN
                 COUNT_CAND = COUNT_CAND+4
                 NS = IG(IS) - NUMNOD
                 IF (IS2SE(1,NS) > 0) THEN
                   IE = IS2SE(1,NS)
                 ELSE
                   IE = IS2SE(2,NS)
                 ENDIF
                 DO KK=1,4
                   COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IRTSE(KK,IE))*
     .             (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                 ENDDO
               ELSE
                 COUNT_CAND = COUNT_CAND+1
                 COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IG(IS))*
     .           (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
               ENDIF
C
             ENDIF  
           ENDIF
          ENDIF
        ENDDO
       ELSE
        DO LS = NLF, NLT
C   conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IRTLM(1,CAND_N(I)) = IABS(IRTLM(1,CAND_N(I)))
          MG = IRTLM(1,CAND_N(I))
          IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN 
            IF(MG /= 0 .and. MG /= MSEGLO(CAND_E(I))) THEN 
c secnd in contact on an other main don't keep candidate
               TIME_S(CAND_N(I))=ZERO
            ELSEIF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
             NLS=NLS+1
             LIST(NLS)=IS
            ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
             TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VZ*DT1,EDGE_L(IS))
             TZINF = MAX(PREC,TZINF)
             ZMIN = MIN(Z1,Z2,Z3,Z4)-TZINF
             ZMAX = MAX(Z1,Z2,Z3,Z4)+TZINF
             IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
             ELSE
              TIME_S(CAND_N(I))=ZERO
             ENDIF
            ENDIF
          ELSE
C Shooting Nodes
C Reset IRTLM when a Secnd node is in contact with a Main surface
C which was deleted.
           IF(STF(L)==ZERO)THEN
             MG = IRTLM(1,CAND_N(I))
             IF (MG == MSEGLO(CAND_E(I)))THEN
               IRTLM(1,CAND_N(I))=0
               TIME_S(CAND_N(I))=-EP20
             ENDIF
           ENDIF
            IF(STFN(CAND_N(I))==ZERO)THEN
               IRTLM(1,CAND_N(I))=0
               TIME_S(CAND_N(I))=-EP20
            ENDIF
          ENDIF
        ENDDO
C
        NLT=NLS
        NLS=0
        DO LS=NLF,NLT
          IS=LIST(LS)
          I=JS+IS
          MG = IRTLM(1,CAND_N(I))
          IF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
            NLS=NLS+1
            LIST(NLS)=IS
          ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
            TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VY*DT1,EDGE_L(IS))
            TZINF = MAX(PREC,TZINF)
            YMIN = MIN(Y1,Y2,Y3,Y4)-TZINF
            YMAX = MAX(Y1,Y2,Y3,Y4)+TZINF
            IF (YMIN<=YI.AND.YMAX>=YI) THEN
              NLS=NLS+1
              LIST(NLS)=IS
            ELSE
              TIME_S(CAND_N(I))=ZERO
            ENDIF
          ENDIF
        ENDDO
C
        DO LS=NLF,NLS
          IS=LIST(LS)
          I=JS+IS
          MG = IRTLM(1,CAND_N(I))
          IF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
              IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
                CAND_N(I) = -CAND_N(I)
                COUNT_CAND = COUNT_CAND+1
                IF(IG(IS) > NUMNOD) COUNT_CAND = COUNT_CAND + 3
              ELSE
CC--         Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
                IG(IS) = NSV(CAND_N(I))
                CAND_N(I) = -CAND_N(I)
                L  = CAND_E(I)
                IX1(IS)=IRECT(1,L)
                IX2(IS)=IRECT(2,L)
                IX3(IS)=IRECT(3,L)
                IX4(IS)=IRECT(4,L)
C
                IF(IG(IS) > NUMNOD) THEN
                  COUNT_CAND = COUNT_CAND+4
                  NS = IG(IS) - NUMNOD
                  IF (IS2SE(1,NS) > 0) THEN
                    IE = IS2SE(1,NS)
                  ELSE
                    IE = IS2SE(2,NS)
                  ENDIF
                  DO KK=1,4
                    COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IRTSE(KK,IE))*
     .              (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                  ENDDO
                ELSE
                  COUNT_CAND = COUNT_CAND+1
                  COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IG(IS))*
     .            (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                ENDIF
              ENDIF  
          ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
            TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VX*DT1,EDGE_L(IS))
            TZINF = MAX(PREC,TZINF)
            XMIN = MIN(X1,X2,X3,X4)-TZINF
            XMAX = MAX(X1,X2,X3,X4)+TZINF
            IF (XMIN<=XI.AND.XMAX>=XI) THEN
              CAND_N(I) = -CAND_N(I)
              IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
                COUNT_CAND = COUNT_CAND+1
                IF(IG(IS) > NUMNOD) COUNT_CAND = COUNT_CAND + 3
              ELSE
CC--         Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
                IF(IG(IS) > NUMNOD) THEN
                  COUNT_CAND = COUNT_CAND+4
                  NS = IG(IS) - NUMNOD
                  IF (IS2SE(1,NS) > 0) THEN
                    IE = IS2SE(1,NS)
                  ELSE
                    IE = IS2SE(2,NS)
                  ENDIF
                  DO KK=1,4
                    COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IRTSE(KK,IE))*
     .              (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                  ENDDO
                ELSE
                  COUNT_CAND = COUNT_CAND+1
                  COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS(1,IG(IS))*
     .            (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                ENDIF
              ENDIF 
            ELSE
              TIME_S(CAND_N(I))=ZERO
            ENDIF
          ENDIF
        ENDDO
       ENDIF
c      write(iout,*)'i24optcd 2'
       IF(NSPMD>1)THEN
          NLF = NLS2
          NLT = NSEG
          IF(IEDGE==0)THEN
            DO LS = NLF, NLT
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN) + GAP_M(CAND_E(I))
             EDGE_L(IS)=ZERO
            ENDDO
          ELSE
            DO LS = NLF, NLT
             IS = LISTI(LS)
             I=JS+IS
             GAPV(IS)=GAPFI(NIN)%P(CAND_N(I)-NSN) + GAP_M(CAND_E(I))
             EDGE_L(IS)=ZERO
c a faire     EDGE_L(IS)=EDGE_L2FI(NIN)%P(CAND_N(I)-NSN)
            ENDDO
          ENDIF
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
             VXI = VFI(NIN)%P(1,II)
             VYI = VFI(NIN)%P(2,II)
             VZI = VFI(NIN)%P(3,II)
             VX1=V(1,IX1(IS))
             VX2=V(1,IX2(IS))
             VX3=V(1,IX3(IS))
             VX4=V(1,IX4(IS))
             VY1=V(2,IX1(IS))
             VY2=V(2,IX2(IS))
             VY3=V(2,IX3(IS))
             VY4=V(2,IX4(IS))
             VZ1=V(3,IX1(IS))
             VZ2=V(3,IX2(IS))
             VZ3=V(3,IX3(IS))
             VZ4=V(3,IX4(IS))
             VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
             VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
             VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4))
             VV = MAX(VX,VY,VZ)
c PMAX_GAP is not used here (node not in contact)
             TZINF = MAX(CURV_MAX+GAPV(IS)+DGAPLOAD,ONEP01*VV*DT1,EDGE_L(IS))
             TZINF = MAX(PREC,TZINF)
             XMIN = X0-TZINF
             YMIN = Y0-TZINF
             ZMIN = Z0-TZINF
             XMAX = X0+TZINF
             YMAX = Y0+TZINF
             ZMAX = Z0+TZINF
             IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                 CAND_N(I) = -CAND_N(I)
                 COUNT_CAND = COUNT_CAND+1
                 IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
                   CT = CT + 1
                   IF(ISEDGE_FI(NIN)%P(II)==1) CT = CT+3
                 ELSE
CC--             Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
                   IF(ISEDGE_FI(NIN)%P(II)==1) THEN
                     CT = CT + 4
                     IF (IS2SE_FI(NIN)%P(1,II) > 0) THEN
                       IE = IS2SE_FI(NIN)%P(1,II)
                     ELSE
                       IE = IS2SE_FI(NIN)%P(2,II)
                     ENDIF
                     DO KK=1,4
                       COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,IRTSE_FI(NIN)%P(KK,IE))*
     .                 (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                     ENDDO
                   ELSE
                     CT = CT + 1
                     COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,II)*
     .               (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                   ENDIF
                ENDIF         
             ELSE
                 TIME_SFI(NIN)%P(II)=ZERO
             END IF
            END IF
           END DO
          ELSE
           NLS=0
           DO LS = NLF, NLT
            IS = LISTI(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            L  = CAND_E(I)
            IRTLM_FI(NIN)%P(1,II)=IABS(IRTLM_FI(NIN)%P(1,II))
            MG = IRTLM_FI(NIN)%P(1,II)
            IF(STF(L)/=ZERO.AND.STIFI(NIN)%P(II)/=ZERO) THEN 
              IF(MG /= 0 .and. MG /= MSEGLO(CAND_E(I))) THEN 
c secnd in contact on a remote main don't keep candidate
                 TIME_SFI(NIN)%P(II)=ZERO 
              ELSEIF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
               NLS=NLS+1
               LIST(NLS)=IS
              ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
               TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VZ*DT1,EDGE_L(IS))                         
               TZINF = MAX(PREC,TZINF)
               ZMIN = MIN(Z1,Z2,Z3,Z4)-TZINF                             
               ZMAX = MAX(Z1,Z2,Z3,Z4)+TZINF                             
               IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN                       
                 NLS=NLS+1                                               
                 LIST(NLS)=IS                                            
               ELSE
                 TIME_SFI(NIN)%P(II)=ZERO     
               ENDIF    
              ENDIF                                                 
            ELSE
              IF(STF(L)==ZERO)THEN
                MG = IRTLM_FI(NIN)%P(1,II)
                IF (MG == MSEGLO(CAND_E(I)))THEN
                  IRTLM_FI(NIN)%P(1,II)=0
                  TIME_SFI(NIN)%P(II)=-EP20
                ENDIF
              ENDIF
              IF(STIFI(NIN)%P(II)==ZERO)THEN
                 IRTLM_FI(NIN)%P(1,II)=0
                  TIME_SFI(NIN)%P(II)=-EP20
              ENDIF
            ENDIF
           ENDDO
C
c      write(iout,*)'i24optcd 3'
           NLF=1
           NLT=NLS
           NLS=0
           DO LS=NLF,NLT
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            MG = IRTLM_FI(NIN)%P(1,II)
            IF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
             NLS=NLS+1
             LIST(NLS)=IS
            ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
             TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VY*DT1,EDGE_L(IS))
             TZINF = MAX(PREC,TZINF)
             YMIN = MIN(Y1,Y2,Y3,Y4)-TZINF
             YMAX = MAX(Y1,Y2,Y3,Y4)+TZINF
             IF (YMIN<=YI.AND.YMAX>=YI) THEN
               NLS=NLS+1
               LIST(NLS)=IS
             ELSE
               TIME_SFI(NIN)%P(II)=ZERO                                    
             ENDIF
            ENDIF
           ENDDO
C
c      write(iout,*)'i24optcd 4'
           DO LS=NLF,NLS
            IS=LIST(LS)
            I=JS+IS
            II = CAND_N(I)-NSN
            MG = IRTLM_FI(NIN)%P(1,II)
            IF(MG == MSEGLO(CAND_E(I)))THEN
c secnd in contact on this main keep candidate
             CAND_N(I) = -CAND_N(I)
             COUNT_CAND = COUNT_CAND+1
             IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
               CT = CT + 1
               IF(ISEDGE_FI(NIN)%P(II)==1) CT = CT+3
             ELSE
CC--           Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
               L = CAND_E(I)
               IX1(IS)=IRECT(1,L)
               IX2(IS)=IRECT(2,L)
               IX3(IS)=IRECT(3,L)
               IX4(IS)=IRECT(4,L)
               IF(ISEDGE_FI(NIN)%P(II)==1) THEN
                 CT = CT + 4
                 IF (IS2SE_FI(NIN)%P(1,II) > 0) THEN
                   IE = IS2SE_FI(NIN)%P(1,II)
                 ELSE
                   IE = IS2SE_FI(NIN)%P(2,II)
                 ENDIF
                 DO KK=1,4
                   COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,IRTSE_FI(NIN)%P(KK,IE))*
     .             (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                 ENDDO
               ELSE
                 CT = CT + 1
                 COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,II)*
     .           (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
               ENDIF
              ENDIF  
            ELSE
c secnd not in contact
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
c PMAX_GAP is not used here (node not in contact)
             TZINF = MAX(GAPV(IS)+DGAPLOAD,ONEP01*VX*DT1,EDGE_L(IS))
             TZINF = MAX(PREC,TZINF)
             XMIN = MIN(X1,X2,X3,X4)-TZINF
             XMAX = MAX(X1,X2,X3,X4)+TZINF
             IF (XMIN<=XI.AND.XMAX>=XI) THEN
               CAND_N(I) = -CAND_N(I)
               COUNT_CAND = COUNT_CAND+1
               IF ((IDTMINS /= 2).AND.(IDTMINS_INT == 0)) THEN
                   CT = CT + 1
                   IF(ISEDGE_FI(NIN)%P(II)==1) CT = CT+3
               ELSE
CC--             Count of contact connections for AMS (+ additional connections related to contact on type2 - T2MAIN_SMS(1) > 1)
                   IF(ISEDGE_FI(NIN)%P(II)==1) THEN
                     CT = CT + 4
                     IF (IS2SE_FI(NIN)%P(1,II) > 0) THEN
                       IE = IS2SE_FI(NIN)%P(1,II)
                     ELSE
                       IE = IS2SE_FI(NIN)%P(2,II)
                     ENDIF
                     DO KK=1,4
                       COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,IRTSE_FI(NIN)%P(KK,IE))*
     .                 (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                     ENDDO
                   ELSE
                     CT = CT + 1
                     COUNT_CONNEC_SMS = COUNT_CONNEC_SMS + T2MAIN_SMS_FI(NIN)%P(1,II)*
     .               (T2MAIN_SMS(1,IX1(IS))+T2MAIN_SMS(1,IX2(IS))+T2MAIN_SMS(1,IX3(IS))+T2MAIN_SMS(1,IX4(IS)))
                   ENDIF
               ENDIF  
             ELSE
               TIME_SFI(NIN)%P(II)=ZERO   
             ENDIF
            ENDIF
           ENDDO
          END IF
       ELSE
         CALL SYNC_DATA(NLS2)
       ENDIF
       JS = JS + NSEG
      ENDDO
c      write(iout,*)'i24optcd 5'

#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
      COUNT_REMSLV(NIN)=COUNT_REMSLV(NIN)+CT
      LSKYI_SMS_NEW = LSKYI_SMS_NEW + COUNT_CONNEC_SMS
#include "lockoff.inc"
C
C ---------------------------------------------------------------------------
C T24E2E TREATMENT for SMP coherency in force computation / Initialize ISPT2
C This treatment was initially made in I24COR3 which is too late
C ---------------------------------------------------------------------------
C Care - SPMD Treatments are made in SPMD_EXCH_I24 due to communication
C ---------------------------------------------------------------------------
      IF (IEDG4 > 0 .AND.NSPMD == 1)THEN
      DO I = NSNF,NSNL 
         ISPT2(I)=0
      ENDDO
         CALL I24ISPT2_INI_OPTTRI(
     1               NSNF ,NSNL  ,NSV    ,CAND_N ,NSN   ,IRTSE  ,
     2               IS2SE ,ISPT2 ,ISEGPT ,IRTLM  ,NSNE  ,
     3               NRTSE ,IEDG4 ,NIN) 
      ENDIF

      RETURN
      END
