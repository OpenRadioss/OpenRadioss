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
Chd|  I7CORK3                       source/interfaces/int07/i7cork3.F
Chd|-- called by -----------
Chd|        I10FKU3                       source/interfaces/int10/i10ke3.F
Chd|        I10KE3                        source/interfaces/int10/i10ke3.F
Chd|        I7FKU3                        source/interfaces/int07/i7ke3.F
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|        IMP_I10MAINF                  source/interfaces/int10/i10ke3.F
Chd|        IMP_I7MAINF                   source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
      SUBROUTINE I7CORK3(JLT   ,X     ,IRECT ,NSV   ,CAND_E  ,
     1                CAND_N  ,STF   ,STFN  ,X1    ,X2     ,
     2                X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3                Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4                XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     5                IX2     ,IX3   ,IX4   ,NSVG  ,IGAP   ,
     6                GAP     ,GAP_S ,GAP_M ,GAPV  ,MS     ,
     7                VXI     ,VYI   ,VZI   ,MSI   ,V      ,
     8                IGSTI   ,KMIN  ,KMAX  ,GAPMAX,GAPMIN ,
     9                NIN     ,ITY   ,NSN   )
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .      JLT,IDT, NOINT,IGAP , IGSTI,NIN,ITY,NSN
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .          NSVG(MVSIZ)
C     REAL
        my_real
     .     GAP, X(3,*), STF(*), STFN(*),GAP_S(*),GAP_M(*),
     .     MS(*), V(3,*)
C     REAL
        my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     KMIN, KMAX, GAPMAX,GAPMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J  ,IL, L, NN, IG,JFT,NI,IX
C-----------------------------------------------
C
        IF(IMACH/=3) THEN
          IF(IGAP==0)THEN
            DO I=1,JLT
              GAPV(I)=GAP
            ENDDO
          ELSE
            DO I=1,JLT
              GAPV(I)=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
              GAPV(I)=MIN(GAPV(I),GAPMAX)
              GAPV(I)=MAX(GAPMIN,GAPV(I))
            END DO
          ENDIF
C
          DO I=1,JLT
            IG = NSV(CAND_N(I))
            NSVG(I) = IG
            XI(I) = X(1,IG)
            YI(I) = X(2,IG)
            ZI(I) = X(3,IG)
            VXI(I) = V(1,IG)
            VYI(I) = V(2,IG)
            VZI(I) = V(3,IG)
            MSI(I)= MS(IG)
C
            L  = CAND_E(I)
C
            IX1(I)=IRECT(1,L)
            X1(I)=X(1,IX1(I))
            Y1(I)=X(2,IX1(I))
            Z1(I)=X(3,IX1(I))
C
            IX2(I)=IRECT(2,L)
            X2(I)=X(1,IX2(I))
            Y2(I)=X(2,IX2(I))
            Z2(I)=X(3,IX2(I))
C
            IX3(I)=IRECT(3,L)
            X3(I)=X(1,IX3(I))
            Y3(I)=X(2,IX3(I))
            Z3(I)=X(3,IX3(I))
C
            IX4(I)=IRECT(4,L)
            X4(I)=X(1,IX4(I))
            Y4(I)=X(2,IX4(I))
            Z4(I)=X(3,IX4(I))
C
          ENDDO
          IF(IGSTI<=1)THEN
            DO I=1,JLT
              L  = CAND_E(I)
              STIF(I)=STF(L)*ABS(STFN(CAND_N(I)))
            ENDDO
          ELSEIF(IGSTI==2)THEN
            DO I=1,JLT
              L  = CAND_E(I)
              STIF(I)=HALF*(STF(L)+ABS(STFN(CAND_N(I))))
              STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
            ENDDO
          ELSEIF(IGSTI==3)THEN
            DO I=1,JLT
              L  = CAND_E(I)
              STIF(I)=MAX(STF(L),ABS(STFN(CAND_N(I))))
              STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
            ENDDO
          ELSEIF(IGSTI==4)THEN
            DO I=1,JLT
              L  = CAND_E(I)
              STIF(I)=MIN(STF(L),ABS(STFN(CAND_N(I))))
              STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
            ENDDO
          ELSEIF(IGSTI==5)THEN
            DO I=1,JLT
              L  = CAND_E(I)
              J  = CAND_N(I)
              STIF(I)=STF(L)*ABS(STFN(J))/
     .                MAX(EM30,(STF(L)+ABS(STFN(J))))
              STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
            ENDDO
          ENDIF
        ELSE
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
            ENDDO
          ENDIF
C
          IF(ITY==7) THEN
            DO I=1,JLT
              NI = CAND_N(I)
              L  = CAND_E(I)
              IF(NI<=NSN)THEN
                IG = NSV(NI)
                NSVG(I) = IG
C             KINI(I) = KINET(IG)
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
C qz          KINI(I) = KINFI(NIN)%P(NN)
                XI(I) = XFI(NIN)%P(1,NN)
                YI(I) = XFI(NIN)%P(2,NN)
                ZI(I) = XFI(NIN)%P(3,NN)
                VXI(I)= VFI(NIN)%P(1,NN)
                VYI(I)= VFI(NIN)%P(2,NN)
                VZI(I)= VFI(NIN)%P(3,NN)
                MSI(I)= MSFI(NIN)%P(NN)
C
              END IF
C
              IX=IRECT(1,L)
              IX1(I)=IX
              X1(I)=X(1,IX)
              Y1(I)=X(2,IX)
              Z1(I)=X(3,IX)
C
              IX=IRECT(2,L)
              IX2(I)=IX
              X2(I)=X(1,IX)
              Y2(I)=X(2,IX)
              Z2(I)=X(3,IX)
C
              IX=IRECT(3,L)
              IX3(I)=IX
              X3(I)=X(1,IX)
              Y3(I)=X(2,IX)
              Z3(I)=X(3,IX)
C
              IX=IRECT(4,L)
              IX4(I)=IX
              X4(I)=X(1,IX)
              Y4(I)=X(2,IX)
              Z4(I)=X(3,IX)
C
            END DO
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
            ELSEIF(IGSTI==4)THEN
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
     .                  MAX(EM30,(STF(L)+STIF(I)))
                STIF(I)=MAX(KMIN,MIN(STIF(I),KMAX))
              ENDDO
            ENDIF
          ELSE
C type7 KINET en -
            DO I=1,JLT
              NI = CAND_N(I)
              L  = CAND_E(I)
              IF(NI<=NSN)THEN
                IG = NSV(NI)
                NSVG(I) = IG
C           KINI(I) = KINET(IG)
                XI(I) = X(1,IG)
                YI(I) = X(2,IG)
                ZI(I) = X(3,IG)
                VXI(I) = V(1,IG)
                VYI(I) = V(2,IG)
                VZI(I) = V(3,IG)
                MSI(I)= MS(IG)
                STIF(I)=STF(L)*ABS(STFN(NI))
              ELSE
                NN = NI - NSN
                NSVG(I) = -NN
C           KINI(I) = KINFI(NIN)%P(NN)
                XI(I) = XFI(NIN)%P(1,NN)
                YI(I) = XFI(NIN)%P(2,NN)
                ZI(I) = XFI(NIN)%P(3,NN)
                VXI(I)= VFI(NIN)%P(1,NN)
                VYI(I)= VFI(NIN)%P(2,NN)
                VZI(I)= VFI(NIN)%P(3,NN)
                MSI(I)= MSFI(NIN)%P(NN)
                STIF(I)=STF(L)*ABS(STIFI(NIN)%P(NN))
C
              END IF
C
              IX=IRECT(1,L)
              IX1(I)=IX
              X1(I)=X(1,IX)
              Y1(I)=X(2,IX)
              Z1(I)=X(3,IX)
C
              IX=IRECT(2,L)
              IX2(I)=IX
              X2(I)=X(1,IX)
              Y2(I)=X(2,IX)
              Z2(I)=X(3,IX)
C
              IX=IRECT(3,L)
              IX3(I)=IX
              X3(I)=X(1,IX)
              Y3(I)=X(2,IX)
              Z3(I)=X(3,IX)
C
              IX=IRECT(4,L)
              IX4(I)=IX
              X4(I)=X(1,IX)
              Y4(I)=X(2,IX)
              Z4(I)=X(3,IX)
C
            END DO
          END IF
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I7CORP3                       source/interfaces/int07/i7cork3.F
Chd|-- called by -----------
Chd|        I7FORCF3                      source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
      SUBROUTINE I7CORP3(JLT   ,X     ,IRECT ,CAND_E  ,CAND_N  ,
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IRECT(4,*), CAND_E(*), CAND_N(*),
     .           JLT,JLT_NEW,NIN,NSN,LREM
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .          NSVG(MVSIZ)
C     REAL
C     REAL
        my_real
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     X(3,*), STIF(*),N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J  ,NI, L, NN, NS ,NE
C-----------------------------------------------
C
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
            IX1(JLT_NEW)  = IRECT(1,L)
            IX2(JLT_NEW)  = IRECT(2,L)
            IX3(JLT_NEW)  = IRECT(3,L)
            IX4(JLT_NEW)  = IRECT(4,L)
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
Chd|  I7CORKP3                      source/interfaces/int07/i7cork3.F
Chd|-- called by -----------
Chd|        I10FKU3                       source/interfaces/int10/i10ke3.F
Chd|        I7FKU3                        source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F
Chd|====================================================================
      SUBROUTINE I7CORKP3(
     1                    JLT   ,XI      ,YI     ,ZI     ,D      ,
     2                    DXI   ,DYI     ,DZI    ,NSVG   ,NIN    ,
     3                    IUPD    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .           JLT,NIN,IUPD,NSVG(MVSIZ)
C     REAL
C     REAL
        my_real
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .     DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I ,J  ,NI, L, NN, NS ,NE
C-----------------------------------------------
C
C
        DO I=1,JLT
          NI = NSVG(I)
          IF(NI<0)THEN
            NN = -NI
            NS=IND_INT(NIN)%P(NN)
C----------displacement
            DXI(I)=DFI(1,NS)
            DYI(I)=DFI(2,NS)
            DZI(I)=DFI(3,NS)
          ELSE
            DXI(I) = D(1,NI)
            DYI(I) = D(2,NI)
            DZI(I) = D(3,NI)
          END IF
C----------update
          IF(IUPD>0.AND.NI<0)THEN
            XI(I) = XI(I) + DXI(I)
            YI(I) = YI(2) + DYI(I)
            ZI(I) = ZI(3) + DZI(I)
          END IF
C
        END DO
C
        RETURN
      END
