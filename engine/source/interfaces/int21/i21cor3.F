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
Chd|  I21COR3                       source/interfaces/int21/i21cor3.F
Chd|-- called by -----------
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|-- calls ---------------
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I21COR3(JLT   ,NIN    ,X       ,IRECT ,NSN   ,
     2                NSV      ,CAND_E ,CAND_N  ,STF   ,STFN  ,
     3                XM0      ,NOD_NORMAL,IRTLM ,CSTS ,MSR   ,
     4                MS       ,V      ,XI      ,YI    ,ZI    ,
     5                IX1      ,IX2    ,IX3     ,IX4   ,NSVG  ,
     6                IGSTI    ,STIF   ,KMIN    ,KMAX  ,IGAP  ,
     7                GAP      ,GAP_S  ,GAPV    ,GAPMAX,GAPMIN,
     8                NX       ,NY     ,NZ      ,PENE  ,VXM   ,
     9                VYM      ,VZM    ,VXI     ,VYI   ,VZI   ,
     A                MSI      ,ITRIA  ,LB      ,LC    ,IADM  ,
     B                RCURV    ,ANGLM  ,NRADM   ,ANGLT ,RCURVI,
     C                ANGLMI   ,FXT    ,FYT     ,FZT   ,FTXSAV,
     D                FTYSAV   ,FTZSAV ,GAP_S0  ,AREA_S0,GAP0 ,
     E                AREA0    ,INTTH  ,TEMP    ,TEMPI  ,IROT ,
     F                XG       ,ROT    ,AS      ,BS     ,ASI  ,
     G                BSI      ,XP     ,YP      ,ZP     ,NODNX_SMS,
     H                NSMS     ,MSTR   ,PENI    ,IFPEN  ,ILEV ,
     I                X1       ,Y1     ,Z1      ,X2     ,Y2   ,
     J                Z2       ,X3     ,Y3      ,Z3     ,X4   ,
     K                Y4       ,Z4     ,DRAD    ,PENRAD ,TINT ,
     L                TEMPM    ,IFORM  ,H1      ,H2     ,H3   ,
     N                H4       ,DIST   ,ITAB    ,NOINT  ,DEPTH,
     M                INVN     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_GLOB_MOD
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
#include      "com08_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .        IRTLM(2,*), MSR(*), NODNX_SMS(*),     
     .        JLT, NOINT, IGAP , NSN, NIN, IGSTI, IADM, NRADM, INTTH,
     .        IROT, MSTR, IFPEN(*), ILEV, IFORM,INVN
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), ITRIA(MVSIZ), NSMS(MVSIZ),ITAB(*)
C     REAL
      my_real
     .   X(3,*), STF(*), STFN(*),GAP_S(*),
     .   XM0(3,*), CSTS(2,*), MS(*), V(3,*), NOD_NORMAL(3,*),
     .   GAP, KMIN, KMAX, GAPMAX, GAPMIN, ANGLT,
     .   RCURV(*), ANGLM(*), RCURVI(MVSIZ), ANGLMI(MVSIZ),
     .   FTXSAV(*),  FTYSAV(*),  FTZSAV(*),
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ),
     .   GAP_S0(*), AREA_S0(*), GAP0(*), AREA0(*), TEMP(*), TEMPI(*),
     .   XG(3), ROT(9), AS(*), BS(*), PENI(*), DRAD, TINT, TEMPM(*),DEPTH
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), 
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), PENE(MVSIZ),
     .   STIF(MVSIZ) ,GAPV(MVSIZ),
     .   VXM, VYM, VZM, 
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .   LB(MVSIZ), LC(MVSIZ), LS, ASI(MVSIZ), BSI(MVSIZ),
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),
     .   PENRAD(*),H1(MVSIZ), H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),
     .   DIST(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, IG,JFT, IX,  NI,
     .        N1, N2, N3, N4,I1,I2,I3,I4,ND,NG
      my_real
     .   XT1(MVSIZ), XT2(MVSIZ), XT3(MVSIZ),
     .   YT1(MVSIZ), YT2(MVSIZ), YT3(MVSIZ),
     .   ZT1(MVSIZ), ZT2(MVSIZ), ZT3(MVSIZ),
     .   X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .   NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .   NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .   NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .   NX0(MVSIZ), NY0(MVSIZ), NZ0(MVSIZ), LA(MVSIZ),
     .   HX(MVSIZ), HY(MVSIZ), HZ(MVSIZ),
     .   NN, HN,TEMP0 , TEMP1, TEMP2, TEMP3, 
     .   TEMP4,TOL,TOLFIX
C-----------------------------------------------
      DO I=1,JLT
C
         J=CAND_N(I)
         L=ABS(IRTLM(1,J))
C
         ITRIA(I)=IRTLM(2,J)
C
         LB(I)=CSTS(1,J)
         LC(I)=CSTS(2,J)
         LA(I) = ONE-LB(I)-LC(I)

         IX1(I) = IRECT(1,L)
         IX2(I) = IRECT(2,L)
         IX3(I) = IRECT(3,L)
         IX4(I) = IRECT(4,L)

      ENDDO

      IF(IGAP.EQ.0)THEN
       DO I=1,JLT
         GAPV(I)=GAP
       ENDDO
      ELSE
         DO I=1,JLT
           GAPV(I)=GAP_S(CAND_N(I))
           GAPV(I)=MIN(GAPV(I),GAPMAX)
           GAPV(I)=MAX(GAPMIN,GAPV(I))
         END DO
      ENDIF
C
      IF(IGAP==2)THEN
         DO I=1,JLT
           GAP0(I) =GAP_S0 (CAND_N(I))
           AREA0(I)=AREA_S0(CAND_N(I))
         END DO
      ELSE
         DO I=1,JLT
           AREA0(I)=AREA_S0(CAND_N(I))
         END DO
      END IF

      IF(INTTH/=0)THEN

C     SECONDARY TEMPERATURE
         DO I=1,JLT             
           IG = NSV(CAND_N(I))
           NSVG(I) = IG
           TEMPI(I) = TEMP(IG)
           ASI(I)   = AS(CAND_N(I))
           BSI(I)   = BS(CAND_N(I))
         END DO 

C     MAIN TEMPERATURE
         IF(INTTH==2)THEN ! Case of MAIN temperature variable over MAIN surface

            DO I=1,JLT
               I1 =  IX1(I)
               ND = MSR(I1)
               IF(ND > 0) THEN                 
                  TEMP1 = TEMP(ND)
               ELSE
                  TEMP1 = NMTEMP(NIN)%P(-ND)
               ENDIF

               I2 =  IX2(I)
               ND = MSR(I2)
               IF(ND > 0) THEN                 
                  TEMP2 = TEMP(ND)
               ELSE
                  TEMP2 = NMTEMP(NIN)%P(-ND)
               ENDIF

               I3 =  IX3(I)
               ND = MSR(I3)
               IF(ND > 0) THEN                 
                  TEMP3 = TEMP(ND)
               ELSE
                  TEMP3 = NMTEMP(NIN)%P(-ND)
               ENDIF

               I4 =  IX4(I)
               ND = MSR(I4)
               IF(ND > 0) THEN                 
                  TEMP4 = TEMP(ND)
               ELSE
                  TEMP4 = NMTEMP(NIN)%P(-ND)
               ENDIF
C
               IF(IX3(I).NE.IX4(I))THEN
                 TEMP0 = FOURTH*(TEMP1+TEMP2+TEMP3+TEMP4) 
               ELSE
                 TEMP0 = TEMP3 
               ENDIF
C             
               IF(ITRIA(I)==1.OR.ITRIA(I)==-1)THEN
                 TEMPM(I) = LB(I)*TEMP1+LC(I)*TEMP2+LA(I)*TEMP0
               ELSEIF(ITRIA(I)==2.OR.ITRIA(I)==-2)THEN
                 TEMPM(I) = LB(I)*TEMP2+LC(I)*TEMP3+LA(I)*TEMP0
               ELSEIF(ITRIA(I)==3.OR.ITRIA(I)==-3)THEN
                 TEMPM(I) = LB(I)*TEMP3+LC(I)*TEMP4+LA(I)*TEMP0
               ELSEIF(ITRIA(I)==4.OR.ITRIA(I)==-4)THEN
                 TEMPM(I) = LB(I)*TEMP4+LC(I)*TEMP1+LA(I)*TEMP0
               ENDIF
C
           END DO
         ELSE    ! Case of MAIN temperature is constant 
            DO I=1,JLT
               TEMPM(I) = TINT
            END DO
         ENDIF
      END IF

      IF(IFORM==1)THEN 
        DO I=1,JLT
          IF(IX3(I).NE.IX4(I))THEN
C
           IF(ITRIA(I)==1.OR.ITRIA(I)==-1)THEN
             H1(I) = LB(I) + LA(I)/4
             H2(I) = LC(I) + LA(I)/4
             H3(I) = LA(I)/4
             H4(I) = LA(I)/4
           ELSEIF(ITRIA(I)==2.OR.ITRIA(I)==-2)THEN
             H2(I) = LB(I) + LA(I)/4
             H3(I) = LC(I) + LA(I)/4
             H1(I) = LA(I)/4
             H4(I) = LA(I)/4
           ELSEIF(ITRIA(I)==3.OR.ITRIA(I)==-3)THEN
             H3(I) = LB(I) + LA(I)/4
             H4(I) = LC(I) + LA(I)/4
             H1(I) = LA(I)/4
             H2(I) = LA(I)/4
           ELSEIF(ITRIA(I)==4.OR.ITRIA(I)==-4)THEN
             H4(I) = LB(I) + LA(I)/4
             H1(I) = LC(I) + LA(I)/4
             H2(I) = LA(I)/4
             H3(I) = LA(I)/4
           ENDIF
C
          ELSE
             H1(I) = LB(I) 
             H2(I) = LC(I) 
             H3(I) = LA(I)
             H4(I) = ZERO
         ENDIF
       ENDDO
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
      END DO
C
      IF(IROT==0)THEN
        DO I=1,JLT
C
         X1(I)=XM0(1,IX1(I))+XG(1)
         Y1(I)=XM0(2,IX1(I))+XG(2)
         Z1(I)=XM0(3,IX1(I))+XG(3)
C
         X2(I)=XM0(1,IX2(I))+XG(1)
         Y2(I)=XM0(2,IX2(I))+XG(2)
         Z2(I)=XM0(3,IX2(I))+XG(3)
C
         X3(I)=XM0(1,IX3(I))+XG(1)
         Y3(I)=XM0(2,IX3(I))+XG(2)
         Z3(I)=XM0(3,IX3(I))+XG(3)
C
         X4(I)=XM0(1,IX4(I))+XG(1)
         Y4(I)=XM0(2,IX4(I))+XG(2)
         Z4(I)=XM0(3,IX4(I))+XG(3)
C
        END DO
C
        DO I=1,JLT
C
         NX1(I)=NOD_NORMAL(1,IX1(I))
         NY1(I)=NOD_NORMAL(2,IX1(I))
         NZ1(I)=NOD_NORMAL(3,IX1(I))
C
         NX2(I)=NOD_NORMAL(1,IX2(I))
         NY2(I)=NOD_NORMAL(2,IX2(I))
         NZ2(I)=NOD_NORMAL(3,IX2(I))
C
         NX3(I)=NOD_NORMAL(1,IX3(I))
         NY3(I)=NOD_NORMAL(2,IX3(I))
         NZ3(I)=NOD_NORMAL(3,IX3(I))
C
         NX4(I)=NOD_NORMAL(1,IX4(I))
         NY4(I)=NOD_NORMAL(2,IX4(I))
         NZ4(I)=NOD_NORMAL(3,IX4(I))
C
        END DO
C
      ELSE
C
        DO I=1,JLT
C
         X1(I)= XG(1)
     .         +ROT(1)*XM0(1,IX1(I))
     .         +ROT(4)*XM0(2,IX1(I))
     .         +ROT(7)*XM0(3,IX1(I))
         Y1(I)= XG(2)
     .         +ROT(2)*XM0(1,IX1(I))
     .         +ROT(5)*XM0(2,IX1(I))
     .         +ROT(8)*XM0(3,IX1(I))
         Z1(I)= XG(3)
     .         +ROT(3)*XM0(1,IX1(I))
     .         +ROT(6)*XM0(2,IX1(I))
     .         +ROT(9)*XM0(3,IX1(I))
C
         X2(I)= XG(1)
     .         +ROT(1)*XM0(1,IX2(I))
     .         +ROT(4)*XM0(2,IX2(I))
     .         +ROT(7)*XM0(3,IX2(I))
         Y2(I)= XG(2)
     .         +ROT(2)*XM0(1,IX2(I))
     .         +ROT(5)*XM0(2,IX2(I))
     .         +ROT(8)*XM0(3,IX2(I))
         Z2(I)= XG(3)
     .         +ROT(3)*XM0(1,IX2(I))
     .         +ROT(6)*XM0(2,IX2(I))
     .         +ROT(9)*XM0(3,IX2(I))
C
         X3(I)= XG(1)
     .         +ROT(1)*XM0(1,IX3(I))
     .         +ROT(4)*XM0(2,IX3(I))
     .         +ROT(7)*XM0(3,IX3(I))
         Y3(I)= XG(2)
     .         +ROT(2)*XM0(1,IX3(I))
     .         +ROT(5)*XM0(2,IX3(I))
     .         +ROT(8)*XM0(3,IX3(I))
         Z3(I)= XG(3)
     .         +ROT(3)*XM0(1,IX3(I))
     .         +ROT(6)*XM0(2,IX3(I))
     .         +ROT(9)*XM0(3,IX3(I))
C
         X4(I)= XG(1)
     .         +ROT(1)*XM0(1,IX4(I))
     .         +ROT(4)*XM0(2,IX4(I))
     .         +ROT(7)*XM0(3,IX4(I))
         Y4(I)= XG(2)
     .         +ROT(2)*XM0(1,IX4(I))
     .         +ROT(5)*XM0(2,IX4(I))
     .         +ROT(8)*XM0(3,IX4(I))
         Z4(I)= XG(3)
     .         +ROT(3)*XM0(1,IX4(I))
     .         +ROT(6)*XM0(2,IX4(I))
     .         +ROT(9)*XM0(3,IX4(I))
C
        END DO
C
        DO I=1,JLT
C
         NX1(I)= ROT(1)*NOD_NORMAL(1,IX1(I))
     .          +ROT(4)*NOD_NORMAL(2,IX1(I))
     .          +ROT(7)*NOD_NORMAL(3,IX1(I))
         NY1(I)= ROT(2)*NOD_NORMAL(1,IX1(I))
     .          +ROT(5)*NOD_NORMAL(2,IX1(I))
     .          +ROT(8)*NOD_NORMAL(3,IX1(I))
         NZ1(I)= ROT(3)*NOD_NORMAL(1,IX1(I))
     .          +ROT(6)*NOD_NORMAL(2,IX1(I))
     .          +ROT(9)*NOD_NORMAL(3,IX1(I))
C
         NX2(I)= ROT(1)*NOD_NORMAL(1,IX2(I))
     .          +ROT(4)*NOD_NORMAL(2,IX2(I))
     .          +ROT(7)*NOD_NORMAL(3,IX2(I))
         NY2(I)= ROT(2)*NOD_NORMAL(1,IX2(I))
     .          +ROT(5)*NOD_NORMAL(2,IX2(I))
     .          +ROT(8)*NOD_NORMAL(3,IX2(I))
         NZ2(I)= ROT(3)*NOD_NORMAL(1,IX2(I))
     .          +ROT(6)*NOD_NORMAL(2,IX2(I))
     .          +ROT(9)*NOD_NORMAL(3,IX2(I))
C
         NX3(I)= ROT(1)*NOD_NORMAL(1,IX3(I))
     .          +ROT(4)*NOD_NORMAL(2,IX3(I))
     .          +ROT(7)*NOD_NORMAL(3,IX3(I))
         NY3(I)= ROT(2)*NOD_NORMAL(1,IX3(I))
     .          +ROT(5)*NOD_NORMAL(2,IX3(I))
     .          +ROT(8)*NOD_NORMAL(3,IX3(I))
         NZ3(I)= ROT(3)*NOD_NORMAL(1,IX3(I))
     .          +ROT(6)*NOD_NORMAL(2,IX3(I))
     .          +ROT(9)*NOD_NORMAL(3,IX3(I))
C
         NX4(I)= ROT(1)*NOD_NORMAL(1,IX4(I))
     .          +ROT(4)*NOD_NORMAL(2,IX4(I))
     .          +ROT(7)*NOD_NORMAL(3,IX4(I))
         NY4(I)= ROT(2)*NOD_NORMAL(1,IX4(I))
     .          +ROT(5)*NOD_NORMAL(2,IX4(I))
     .          +ROT(8)*NOD_NORMAL(3,IX4(I))
         NZ4(I)= ROT(3)*NOD_NORMAL(1,IX4(I))
     .          +ROT(6)*NOD_NORMAL(2,IX4(I))
     .          +ROT(9)*NOD_NORMAL(3,IX4(I))
C
        END DO
C
      END IF
C
       DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
         X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
         NX0(I)= FOURTH*(NX1(I)+NX2(I)+NX3(I)+NX4(I))
         NY0(I)= FOURTH*(NY1(I)+NY2(I)+NY3(I)+NY4(I))
         NZ0(I)= FOURTH*(NZ1(I)+NZ2(I)+NZ3(I)+NZ4(I))
        ELSE
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
         NX0(I)= NX3(I)
         NY0(I)= NY3(I)
         NZ0(I)= NZ3(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
C
         IF(ITRIA(I)==1.OR.ITRIA(I)==-1)THEN
           XT1(I)=X1(I)
           YT1(I)=Y1(I)
           ZT1(I)=Z1(I)
           XT2(I)=X2(I)
           YT2(I)=Y2(I)
           ZT2(I)=Z2(I)
           XT3(I)=X0(I)
           YT3(I)=Y0(I)
           ZT3(I)=Z0(I)
           NX(I)=LB(I)*NX1(I)+LC(I)*NX2(I)+LA(I)*NX0(I)
           NY(I)=LB(I)*NY1(I)+LC(I)*NY2(I)+LA(I)*NY0(I)
           NZ(I)=LB(I)*NZ1(I)+LC(I)*NZ2(I)+LA(I)*NZ0(I)
         ELSEIF(ITRIA(I)==2.OR.ITRIA(I)==-2)THEN
           XT1(I)=X2(I)
           YT1(I)=Y2(I)
           ZT1(I)=Z2(I)
           XT2(I)=X3(I)
           YT2(I)=Y3(I)
           ZT2(I)=Z3(I)
           XT3(I)=X0(I)
           YT3(I)=Y0(I)
           ZT3(I)=Z0(I)
           NX(I)=LB(I)*NX2(I)+LC(I)*NX3(I)+LA(I)*NX0(I)
           NY(I)=LB(I)*NY2(I)+LC(I)*NY3(I)+LA(I)*NY0(I)
           NZ(I)=LB(I)*NZ2(I)+LC(I)*NZ3(I)+LA(I)*NZ0(I)
         ELSEIF(ITRIA(I)==3.OR.ITRIA(I)==-3)THEN
           XT1(I)=X3(I)
           YT1(I)=Y3(I)
           ZT1(I)=Z3(I)
           XT2(I)=X4(I)
           YT2(I)=Y4(I)
           ZT2(I)=Z4(I)
           XT3(I)=X0(I)
           YT3(I)=Y0(I)
           ZT3(I)=Z0(I)
           NX(I)=LB(I)*NX3(I)+LC(I)*NX4(I)+LA(I)*NX0(I)
           NY(I)=LB(I)*NY3(I)+LC(I)*NY4(I)+LA(I)*NY0(I)
           NZ(I)=LB(I)*NZ3(I)+LC(I)*NZ4(I)+LA(I)*NZ0(I)
         ELSEIF(ITRIA(I)==4.OR.ITRIA(I)==-4)THEN
           XT1(I)=X4(I)
           YT1(I)=Y4(I)
           ZT1(I)=Z4(I)
           XT2(I)=X1(I)
           YT2(I)=Y1(I)
           ZT2(I)=Z1(I)
           XT3(I)=X0(I)
           YT3(I)=Y0(I)
           ZT3(I)=Z0(I)
           NX(I)=LB(I)*NX4(I)+LC(I)*NX1(I)+LA(I)*NX0(I)
           NY(I)=LB(I)*NY4(I)+LC(I)*NY1(I)+LA(I)*NY0(I)
           NZ(I)=LB(I)*NZ4(I)+LC(I)*NZ1(I)+LA(I)*NZ0(I)
         END IF
C
      ENDDO
C
      DO I=1,JLT

C3 pas la meme distance que dans i21dst3 : DIST <= Pi(I)
c       LB(I) = MIN(ONE,MAX(LB(I),ZERO))
c       LC(I) = MIN(ONE,MAX(LC(I),ZERO))
c       LA(I) = MIN(ONE,MAX(LA(I),ZERO))
c       LS=ONE/MAX(EM20,LA(I)+LB(I)+LC(I))
c       LB(I) = LB(I)*LS
c       LC(I) = LC(I)*LS
c       LA(I) = LA(I)*LS

        XP(I)=(LB(I)*XT1(I)+LC(I)*XT2(I)+LA(I)*XT3(I))
        YP(I)=(LB(I)*YT1(I)+LC(I)*YT2(I)+LA(I)*YT3(I))
        ZP(I)=(LB(I)*ZT1(I)+LC(I)*ZT2(I)+LA(I)*ZT3(I))

        HX(I)=XI(I)-XP(I)
        HY(I)=YI(I)-YP(I)
        HZ(I)=ZI(I)-ZP(I)

        HN   =HX(I)*NX(I)+HY(I)*NY(I)+HZ(I)*NZ(I)
        NX(I)=HX(I)*SIGN(ONE,HN)
        NY(I)=HY(I)*SIGN(ONE,HN)
        NZ(I)=HZ(I)*SIGN(ONE,HN)

        DIST(I)=SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I))
        NN=ONE/MAX(EM20,DIST(I))
        NX(I)=NX(I)*NN
        NY(I)=NY(I)*NN
        NZ(I)=NZ(I)*NN
        
        DIST(I)=DIST(I)*SIGN(ONE,HN)
C3 DIST <= GAPV(I) a la precision machine pres.
        PENE(I)=MAX(ZERO,GAPV(I)-DIST(I))
      ENDDO

       
C
      DO I=1,JLT
       L  = CAND_E(I)
       STIF(I)=STF(L)*ABS(STFN(CAND_N(I)))
      ENDDO
C
      IF(INTTH/=0)THEN
        DO I=1,JLT
         L  = CAND_E(I)
C PENRAD mesure si Gap < Dist < Dradiation
         PENRAD(I)=DIST(I)-GAPV(I)
C Out of the gap (Gap < dist < Dradiation)
C peut-etre inutile (PENE=0 et IFPEN==1 => PENRAD <= 0)
C        IF(PENRAD(I) > ZERO) STIF(I)=ZERO
        ENDDO
      END IF
C---------------------
C      PENE INITIALE
C---------------------
      IF(ILEV==0)THEN
       IF(TT/=ZERO)THEN
        DO I=1,JLT
          IF(PENE(I)==ZERO)THEN
Cetait bug (pas la meme distance que dans i21dst3 : DIST <= Pi(I))
            FTXSAV(CAND_N(I))=ZERO
            FTYSAV(CAND_N(I))=ZERO
            FTZSAV(CAND_N(I))=ZERO
            PENI(CAND_N(I))  =ZERO
            IFPEN(CAND_N(I)) =0
          END IF
        END DO
       END IF        
      ELSEIF(TT/=ZERO)THEN
        DO I=1,JLT
          IF(PENE(I)==ZERO)THEN
Cetait bug (pas la meme distance que dans i21dst3 : DIST <= Pi(I))
            FTXSAV(CAND_N(I))=ZERO
            FTYSAV(CAND_N(I))=ZERO
            FTZSAV(CAND_N(I))=ZERO
            PENI(CAND_N(I))  =ZERO
            IFPEN(CAND_N(I)) =0
          ELSEIF(IFPEN(CAND_N(I))==1)THEN
C no force but viscosity at 1st impact 
C (due to possible impact from the rear)
            PENI(CAND_N(I))    =PENE(I)
          END IF
        END DO
      END IF        
C
      IF(INVN > 0) THEN
        TOLFIX = (ONE-EM02)*DEPTH
        DO I=1,JLT
          TOL =  GAPV(I) + TOLFIX
          IF(IFPEN(CAND_N(I))==1)THEN
C check if penetrations are important at 1st impact => detect if normals are inverted GAPV+DEPTH-0.01*DEPTH <= PENE <= GAPV+DEPTH
             IF(PENE(I).GT.TOL.AND.STIF(I).GT.ZERO) THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,I10,A)')
     .            ' **CONTACT PROBLEM IN INTERFACE ',NOINT,'**'
                WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 21.'
                WRITE(ISTDO,'(A)')'Maximum penetration is reached'
                WRITE(ISTDO,'(A)')'You may need to check if contact normals 
     .of tools are oriented toward the blank,'
                WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 21.'
                WRITE(IOUT, '(A)')'Maximum penetration is reached'
                WRITE(IOUT, '(A)')'You may need to check if contact normals 
     .of tools are oriented toward the blank,'

                NG = NSVG(I)
                NI = ITAB(NG)

                WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',NI
                WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .            ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))
#include "lockoff.inc"
c               TSTOP = TT

                MSTOP = 2
             ENDIF
           ENDIF
         ENDDO
      ENDIF
C
      DO I=1,JLT
        FXT(I)=FTXSAV(CAND_N(I))
        FYT(I)=FTYSAV(CAND_N(I))
        FZT(I)=FTZSAV(CAND_N(I))
      END DO
C
      IF(IADM.NE.0)THEN
        DO I=1,JLT
         L  = CAND_E(I)
         RCURVI(I)=RCURV(L)/NRADM
         ANGLMI(I)=ANGLM(L)/MAX(EM20,ANGLT)
        END DO
      END IF
C
      IF(IDTMINS==2)THEN
        DO I=1,JLT
          NSMS(I)=NODNX_SMS(NSVG(I))+NODNX_SMS(MSTR)
        ENDDO
        IF(IDTMINS_INT/=0)THEN
          DO I=1,JLT
           IF(NSMS(I).EQ.0)NSMS(I)=-1
          ENDDO
        END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        DO I=1,JLT
         NSMS(I)=-1
        ENDDO
      ENDIF

      RETURN
      END

