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
Chd|  I8FOR3                        source/interfaces/inter3d/i8for3.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I8FOR3(LFT   ,LLT   ,NFT   ,
     2                  E     ,MSR   ,NSV   ,IRTL  ,STF   ,
     .                  NSVGLO,NSV2   ,ILOC,
     3                  STFN  ,IBC   ,ICODT ,FSAV  ,IGIMP ,
     4                  X     ,V     ,MS    ,FMAX  ,NSN   ,
     5                  FSKYI ,ISKY  ,FCONT ,RCONTACT,IFORM,
     6                  FTSAVX,FTSAVY,FTSAVZ,VISC  ,FNOR  ,
     7                  DEPTH ,DIST  ,GAPN  ,SLOPEN,STIFN ,
     8                  FNCONT,FTCONT,ITAB  ,IFT0, 
     9                  IX1   ,IX2   ,IX3   ,IX4,
     A                  XI    ,YI    ,ZI,
     B                  N1    ,N2    ,N3,
     C                  ANS   ,SSC   ,TTC,
     D                  H1    ,H2    ,H3    ,H4,
     E                  XFACE ,STIF  ,FNI,
     F                  FXI   ,FYI   ,FZI,
     G                  FX1   ,FY1   ,FZ1,
     H                  FX2   ,FY2   ,FZ2,
     I                  FX3   ,FY3   ,FZ3,
     J                  FX4   ,FY4   ,FZ4,
     K                  THK   ,H3D_DATA,NINSKID,
     L                  NINTERSKID,PSKIDS,IRECT,NIN,
     M                  TAGNCONT ,KLOADPINTER,LOADPINTER ,LOADP_HYD_INTER,
     O                  IFLINEAR ,FRIC_LAST,FNOR_LAST,DISTLIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBC, IGIMP, NSN,LFT, LLT, NFT, IFORM,IFT0,NINSKID ,NINTERSKID,NIN
      INTEGER MSR(*), NSV(*), IRTL(*), ICODT(*), ISKY(*),ITAB(*)
      INTEGER NSVGLO(*),NSV2(*),ILOC(*),IRECT(4,*)
C     REAL
      INTEGER IX1(*), IX2(*), IX3(*), IX4(*),
     .  TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .  KLOADPINTER(NINTER+1),LOADPINTER(NINTER*NLOADP_HYD),
     .  LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER  , INTENT(IN) :: IFLINEAR
      my_real 
     .  E(*), STF(*), STFN(*), FSAV(*), X(3,*),V(3,*),MS(*),
     .  FSKYI(LSKYI,NFSKYI),FCONT(3,*),FMAX, RCONTACT(*),
     .  FTSAVX(*), FTSAVY(*), FTSAVZ(*), VISC,SLOPEN(*),
     .  FNOR,DEPTH,DIST(*),GAPN(*),STIFN(*),FNCONT(3,*),FTCONT(3,*),
     .  PSKIDS(NINTERSKID,*)
      my_real
     .   XI(*), YI(*), ZI(*), N1(*), N2(*), N3(*), ANS(*), SSC(*),
     .   TTC(*), THK(*), H1(*), H2(*), H3(*), H4(*), XFACE(*), STIF(*),
     .   FXI(*), FYI(*), FZI(*), FNI(*), FX1(*), FX2(*), FX3(*), FX4(*),
     .   FY1(*), FY2(*), FY3(*), FY4(*), FZ1(*), FZ2(*), FZ3(*), FZ4(*)
      my_real  , INTENT(IN) :: FRIC_LAST,FNOR_LAST,DISTLIN(NSN)
      TYPE(H3D_DATABASE) :: H3D_DATA

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, J3, J2, J1, IG,
     .   I3, I2, I1,JL,JG,J,NN, PP, PPL,K
      INTEGER NISKYL
      my_real
     .   NX,NY,NZ,LX,LY,LZ,NX2,CC2,ST,LI2(NSN),
     .   VX(NSN),VY(NSN),VZ(NSN),VV(NSN),VN(NSN),
     .   NN1(NSN),NN2(NSN),NN3(NSN),FNNI(NSN),NNX, 
     .   FELAST, FTRY, DELTAG, DT1INV, VIS2,PEN(NSN),FFAC,
     .   FNLIM, FTLIM,
     .   FNXI(NSN),FNYI(NSN),FNZI(NSN),FNX1(NSN),FNY1(NSN),
     .   FNZ1(NSN),FNX2(NSN),FNY2(NSN),FNZ2(NSN),FNX3(NSN),
     .   FNY3(NSN),FNZ3(NSN),FNX4(NSN),FNY4(NSN),FNZ4(NSN),
     .   FTXI(NSN),FTYI(NSN),FTZI(NSN),FTX1(NSN),FTY1(NSN),
     .   FTZ1(NSN),FTX2(NSN),FTY2(NSN),FTZ2(NSN),FTX3(NSN),
     .   FTY3(NSN),FTZ3(NSN),FTX4(NSN),FTY4(NSN),FTZ4(NSN)
C---------------------------------------------
      FTLIM = FMAX
      FNLIM = FNOR

C--------------Before Nj change----       
       IF(FNOR /=ZERO) THEN 
         DO I=LFT,LLT
            NN1(I) = N1(I)
            NN2(I) = N2(I)
            NN3(I) = N3(I)  
         END DO 
       END IF 
C--------------Pene computation----     
       IF(DEPTH > ZERO) THEN 
           DO I=LFT,LLT
             IL=I+NFT
             L=IRTL(IL)
             PEN(I) = ZERO
             IF(L > 0) THEN
              PEN(I) = (DEPTH - DIST(I) + GAPN(L))*ABS(XFACE(I))
             ENDIF
           END DO 
        ELSE
            DO I=LFT,LLT
               PEN(I) = ONE
            ENDDO
        END IF 

C-------Output Skid line for type 8 ----
      IF(NINSKID > 0) THEN
         DO I=LFT,LLT
            IL=I+NFT
            L=IRTL(IL)
            IF(L > 0.AND.PEN(I)>ZERO) THEN
              DO J=1,4
                 NN=MSR(IRECT(J,L))
                 PSKIDS(NINSKID,NN) = ONE
              ENDDO
            ENDIF
         ENDDO
      ENDIF

C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINTLOADP > 0) THEN
         DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
            PP = LOADPINTER(K)
            PPL = LOADP_HYD_INTER(PP)
            DO I=LFT,LLT
               IL=I+NFT
               L=IRTL(IL)
               IF(L > 0.AND.PEN(I)>ZERO) THEN
                 DO J=1,4
                    NN=MSR(IRECT(J,L))
                    TAGNCONT(PPL,NN) = 1
                 ENDDO
               ENDIF
            ENDDO
         ENDDO

      ENDIF

        DO I=LFT,LLT
         FXI(I) = ZERO
         FYI(I) = ZERO
         FZI(I) = ZERO
        ENDDO

C
C-------------------------------
C     RESTRAINING FORCE 
C-------------------------------
      SELECT CASE(IFORM)
C
      CASE(1)
C-------------------
C     VISCOOUS FORMULATION
C-------------------
      DO I=LFT,LLT
        IF(PEN(I)>ZERO) THEN    
          IL=I+NFT
          IG=NSV(IL)
          I1 = NSVGLO(MAX(1,NSV2(IL)-1))
          I2 = NSVGLO(MIN(NSN,NSV2(IL)+1))
c
          LX = HALF*(X(1,I2)-X(1,I1))
          LY = HALF*(X(2,I2)-X(2,I1))
          LZ = HALF*(X(3,I2)-X(3,I1))
c
          NX = N2(I)*LZ - N3(I)*LY
          NY = N3(I)*LX - N1(I)*LZ
          NZ = N1(I)*LY - N2(I)*LX
c
c (vitesse SECONDARY = 0)
          VX(I) =  V(1,IX1(I))*H1(I) + V(1,IX2(I))*H2(I)
     .         + V(1,IX3(I))*H3(I) + V(1,IX4(I))*H4(I)  
          VY(I) =  V(2,IX1(I))*H1(I) + V(2,IX2(I))*H2(I)
     .         + V(2,IX3(I))*H3(I) + V(2,IX4(I))*H4(I)  
          VZ(I) =  V(3,IX1(I))*H1(I) + V(3,IX2(I))*H2(I)
     .         + V(3,IX3(I))*H3(I) + V(3,IX4(I))*H4(I)  
          VV(I) = VX(I)*NX + VY(I)*NY + VZ(I)*NZ
          IF(VV(I)<ZERO)THEN
            NX=-NX
            NY=-NY
            NZ=-NZ
            VV(I)=-VV(I)
          ENDIF
C  
          N1(I)=NX
          N2(I)=NY
          N3(I)=NZ
        ENDIF
      END DO
C
      DO 150 I=LFT,LLT
        IF(PEN(I)>ZERO) THEN    
          IL=I+NFT
          IG=NSV2(IL)
          L=IRTL(IL)
          CC2 = STF(L)
     .  *FOURTH*( MS(IX1(I))+MS(IX2(I))+MS(IX3(I))+MS(IX4(I)) )
          NX2 = MAX(EM20,N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I))
          LI2(I)=NX2
          FNI(I)= VV(I)*SQRT(CC2/NX2)*ABS(XFACE(I))
          IF(FRIC_LAST/= ZERO) THEN
             FTLIM = FMAX + (DISTLIN(IG)/DISTLIN(NSN))*(FRIC_LAST-FMAX)
          ENDIF
          IF(FNI(I)>FTLIM)THEN
            FNI(I)= FTLIM
          ENDIF
C  
          FNI(I)= - FNI(I)
          FXI(I)=N1(I)*FNI(I)
          FYI(I)=N2(I)*FNI(I)
          FZI(I)=N3(I)*FNI(I)
        ENDIF
       
 150  CONTINUE
C
      CASE(2)
C-------------------
C     INCREMENTAL FORMULATION FOR TANGENTIAL FORCE
C-------------------
      ! A Reecrire pour le parallele
      DO I=LFT,LLT
        IL=I+NFT
        IG=NSV(IL)
        IF(PEN(I)>ZERO) THEN    
        !IF the proc MAIN handles the face
        I1 = NSVGLO(MAX(1,NSV2(IL)-1))
        I2 = NSVGLO(MIN(NSN,NSV2(IL)+1))
c
        LX = HALF*(X(1,I2)-X(1,I1))
        LY = HALF*(X(2,I2)-X(2,I1))
        LZ = HALF*(X(3,I2)-X(3,I1))
c
        NX = N2(I)*LZ - N3(I)*LY
        NY = N3(I)*LX - N1(I)*LZ
        NZ = N1(I)*LY - N2(I)*LX
c
c (vitesse SECONDARY = 0)
        VX(I) = V(1,IX1(I))*H1(I) + V(1,IX2(I))*H2(I)
     .        + V(1,IX3(I))*H3(I) + V(1,IX4(I))*H4(I)  
        VY(I) = V(2,IX1(I))*H1(I) + V(2,IX2(I))*H2(I)
     .        + V(2,IX3(I))*H3(I) + V(2,IX4(I))*H4(I)  
        VZ(I) = V(3,IX1(I))*H1(I) + V(3,IX2(I))*H2(I)
     .        + V(3,IX3(I))*H3(I) + V(3,IX4(I))*H4(I)  
C
        VV(I) = VX(I)*NX + VY(I)*NY + VZ(I)*NZ
        IF(VV(I)<ZERO)THEN
          NX=-NX
          NY=-NY
          NZ=-NZ
          VV(I)=-VV(I)
        ENDIF
C
        N1(I)=NX
        N2(I)=NY
        N3(I)=NZ
        ENDIF
      ENDDO
C
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      VIS2=VISC*VISC
C
      DO I=LFT,LLT
        IL=I+NFT
        ! Global ID of the salve node
        IG=NSV2(IL)
        L=IRTL(IL)
C  
        IF(PEN(I)>ZERO) THEN    
          ST  = STF(L)
          CC2 = VIS2*STF(L)
     .    * FOURTH*( MS(IX1(I))+MS(IX2(I))+MS(IX3(I))+MS(IX4(I)) )
C         
          FXI(I)=(FTSAVX(IG)+ST*VX(I)*DT1)*ABS(XFACE(I))
          FYI(I)=(FTSAVY(IG)+ST*VY(I)*DT1)*ABS(XFACE(I))
          FZI(I)=(FTSAVZ(IG)+ST*VZ(I)*DT1)*ABS(XFACE(I))
C         
          NX2    =MAX(EM20,N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I))
          LI2(I)=NX2
          NX2    =ONE/NX2
          FELAST =(FXI(I)*N1(I)+FYI(I)*N2(I)+FZI(I)*N3(I))*NX2
C         
          FTRY   =FELAST+SQRT(CC2*NX2)*VV(I)*ABS(XFACE(I))

          IF(FRIC_LAST/= ZERO) THEN
             FTLIM = FMAX + (DISTLIN(IG)/DISTLIN(NSN))*(FRIC_LAST-FMAX)
          ENDIF

          FNI(I) =SIGN(MIN(ABS(FTRY),FTLIM),FTRY)

C         
C slidinnng (per unit length)  
          DELTAG =(FTRY-FNI(I))/MAX(EM20,ST+SQRT(CC2)*DT1INV)
          FELAST =FELAST-ST*DELTAG
C         
C save eeelastic force
          FTSAVX(IG)=FELAST*N1(I)
          FTSAVY(IG)=FELAST*N2(I)
          FTSAVZ(IG)=FELAST*N3(I)
C         
          FNI(I)= - FNI(I)
          FXI(I)= FNI(I)*N1(I)
          FYI(I)= FNI(I)*N2(I)
          FZI(I)= FNI(I)*N3(I)
        ENDIF
 
      END DO
C
      END SELECT

C------- For Post-precessing      
        DO I=LFT,LLT
         FTXI(I)= FXI(I)
         FTYI(I)= FYI(I)
         FTZI(I)= FZI(I)
        ENDDO
C-------------------------------
C     NORMAL FORCE 
C-------------------------------
c       SLOPE = ZERO
c       IF(FNOR/=0) SLOPE = FNOR/MAX(DEPTH,EM20)
c      
       FNNI = ZERO
       IF(FNOR /=ZERO) THEN 
         DO I=LFT,LLT
          IL=I+NFT
          !Global SECONDARY index
          IG=NSV2(IL)
          L=IRTL(IL)
          IF(IRTL(IL) > 0) THEN 
c            PEN(I) = (DEPTH - DIST(I) + GAPN(L))*ABS(XFACE(I))
C    
            IF(FNOR_LAST/= ZERO) THEN
               FNLIM = FNOR + (DISTLIN(IG)/DISTLIN(NSN))*(FNOR_LAST-FNOR)
            ENDIF
            IF(PEN(I)>=DEPTH) THEN
              FNNI(I)= FNLIM*SQRT(LI2(I))
            ELSEIF(PEN(I)>ZERO) THEN
              FNNI(I)= SLOPEN(IG)*PEN(I)*SQRT(LI2(I))
C----restraining force reducing              
             IF(IFT0==0 .AND. SLOPEN(IG)<STF(L)) THEN
              FFAC = PEN(I)/DEPTH
              FXI(I)= FFAC*FXI(I)
              FYI(I)= FFAC*FYI(I)
              FZI(I)= FFAC*FZI(I)
             END IF
            ENDIF
          FNXI(I)= - NN1(I)*FNNI(I)
          FNYI(I)= - NN2(I)*FNNI(I)
          FNZI(I)= - NN3(I)*FNNI(I)
          ENDIF
         ENDDO
C------- For Post-precessing      
        DO I=LFT,LLT
         FTXI(I)= FXI(I)
         FTYI(I)= FYI(I)
         FTZI(I)= FZI(I)
        ENDDO
C-----add normal forces
        DO I=LFT,LLT
         FXI(I)= FXI(I) + FNXI(I)
         FYI(I)= FYI(I) + FNYI(I)
         FZI(I)= FZI(I) + FNZI(I) 
        ENDDO
       ELSE
        DO I=LFT,LLT
          FNXI(I)= ZERO
          FNYI(I)= ZERO
          FNZI(I)= ZERO
        ENDDO
       ENDIF
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C---------------------------------
      DO 155 I=LFT,LLT
        FSAV(1)=FSAV(1)+FNXI(I)*DT12
        FSAV(2)=FSAV(2)+FNYI(I)*DT12
        FSAV(3)=FSAV(3)+FNZI(I)*DT12

        FSAV(4)=FSAV(4)+FTXI(I)*DT12
        FSAV(5)=FSAV(5)+FTYI(I)*DT12
        FSAV(6)=FSAV(6)+FTZI(I)*DT12

        FSAV(8)=FSAV(8)+ABS(FNXI(I))*DT12
        FSAV(9)=FSAV(9)+ABS(FNYI(I))*DT12
        FSAV(10)=FSAV(10)+ABS(FNZI(I))*DT12
        FSAV(11)=FSAV(11)+FNI(I)*DT12

        FSAV(12)=FSAV(12)+ABS(FXI(I))*DT12
        FSAV(13)=FSAV(13)+ABS(FYI(I))*DT12
        FSAV(14)=FSAV(14)+ABS(FZI(I))*DT12
        FSAV(15) = FSAV(15) +SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))*DT12
 155  CONTINUE
C
      DO 160 I=LFT,LLT
      FX1(I)=FXI(I)*H1(I)
      FY1(I)=FYI(I)*H1(I)
      FZ1(I)=FZI(I)*H1(I)
C
      FX2(I)=FXI(I)*H2(I)
      FY2(I)=FYI(I)*H2(I)
      FZ2(I)=FZI(I)*H2(I)
C
      FX3(I)=FXI(I)*H3(I)
      FY3(I)=FYI(I)*H3(I)
      FZ3(I)=FZI(I)*H3(I)
C
      FX4(I)=FXI(I)*H4(I)
      FY4(I)=FYI(I)*H4(I)
      FZ4(I)=FZI(I)*H4(I)
C
      FNX1(I)=FNXI(I)*H1(I)
      FNY1(I)=FNYI(I)*H1(I)
      FNZ1(I)=FNZI(I)*H1(I)
C
      FNX2(I)=FNXI(I)*H2(I)
      FNY2(I)=FNYI(I)*H2(I)
      FNZ2(I)=FNZI(I)*H2(I)
C
      FNX3(I)=FNXI(I)*H3(I)
      FNY3(I)=FNYI(I)*H3(I)
      FNZ3(I)=FNZI(I)*H3(I)
C
      FNX4(I)=FNXI(I)*H4(I)
      FNY4(I)=FNYI(I)*H4(I)
      FNZ4(I)=FNZI(I)*H4(I)
C
      FTX1(I)=FTXI(I)*H1(I)
      FTY1(I)=FTYI(I)*H1(I)
      FTZ1(I)=FTZI(I)*H1(I)
C
      FTX2(I)=FTXI(I)*H2(I)
      FTY2(I)=FTYI(I)*H2(I)
      FTZ2(I)=FTZI(I)*H2(I)
C
      FTX3(I)=FTXI(I)*H3(I)
      FTY3(I)=FTYI(I)*H3(I)
      FTZ3(I)=FTZI(I)*H3(I)
C
      FTX4(I)=FTXI(I)*H4(I)
      FTY4(I)=FTYI(I)*H4(I)
      FTZ4(I)=FTZI(I)*H4(I)
C



 160  CONTINUE
C
      IF(IPARIT==0)THEN
        DO 180 I=LFT,LLT
        J3=3*IX1(I)
        J2=J3-1
        J1=J2-1
        E(J1)=E(J1)+FX1(I)
        E(J2)=E(J2)+FY1(I)
        E(J3)=E(J3)+FZ1(I)
c        STIFN(J1) = STIFN(J1) + SLOPE*ABS(H1(I))
C
        J3=3*IX2(I)
        J2=J3-1
        J1=J2-1
        E(J1)=E(J1)+FX2(I)
        E(J2)=E(J2)+FY2(I)
        E(J3)=E(J3)+FZ2(I)
c        STIFN(J1) = STIFN(J1) + SLOPE*ABS(H2(I))
C
        J3=3*IX3(I)
        J2=J3-1
        J1=J2-1
        E(J1)=E(J1)+FX3(I)
        E(J2)=E(J2)+FY3(I)
        E(J3)=E(J3)+FZ3(I)
c        STIFN(J1) = STIFN(J1) + SLOPE*ABS(H3(I))
C
        J3=3*IX4(I)
        J2=J3-1
        J1=J2-1
        E(J1)=E(J1)+FX4(I)
        E(J2)=E(J2)+FY4(I)
        E(J3)=E(J3)+FZ4(I)
c        STIFN(J1) = STIFN(J1) + SLOPE*ABS(H4(I))
C
        IL=I+NFT
        IG=NSV(IL)
        I3=3*IG
        I2=I3-1
        I1=I2-1
        E(I1)=E(I1)-FXI(I)
        E(I2)=E(I2)-FYI(I)
        E(I3)=E(I3)-FZI(I)
c        STIFN(I1) = STIFN(I1) + SLOPE
 180    CONTINUE
C
      ELSE
C
#include "lockon.inc"
         NISKYL = NISKY
         NISKY = NISKY + 5 * LLT
#include "lockoff.inc"
C
        IF(KDTINT==0)THEN
         DO 190 I=LFT,LLT
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          ISKY(NISKYL) = IX1(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          ISKY(NISKYL) = IX2(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          ISKY(NISKYL) = IX3(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          ISKY(NISKYL) = IX4(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=-FXI(I)
          FSKYI(NISKYL,2)=-FYI(I)
          FSKYI(NISKYL,3)=-FZI(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          IL=I+NFT
          ISKY(NISKYL) = NSV(IL)
 190     CONTINUE
        ELSE
         DO I=LFT,LLT
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX1(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX2(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX3(I)
          FSKYI(NISKYL,2)=FY3(I)
          FSKYI(NISKYL,3)=FZ3(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX3(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX4(I)
          FSKYI(NISKYL,2)=FY4(I)
          FSKYI(NISKYL,3)=FZ4(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          FSKYI(NISKYL,5)=ZERO
          ISKY(NISKYL) = IX4(I)
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=-FXI(I)
          FSKYI(NISKYL,2)=-FYI(I)
          FSKYI(NISKYL,3)=-FZI(I)
          FSKYI(NISKYL,4)=ZERO !SLOPE
          FSKYI(NISKYL,5)=ZERO
          IL=I+NFT
          ISKY(NISKYL) = NSV(IL)
         ENDDO
        ENDIF
      ENDIF
      IF(NADMESH/=0)THEN
#include "lockon.inc"
           DO I=1,LLT
             IF(XFACE(I)/=ZERO)THEN
                RCONTACT(IX1(I))=ZERO
                RCONTACT(IX2(I))=ZERO
                RCONTACT(IX3(I))=ZERO
                RCONTACT(IX4(I))=ZERO
             END IF
           ENDDO
#include "lockoff.inc"
      END IF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
#include "lockon.inc"
           DO I=1,LLT
            FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)
            FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)
            FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)
            FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)
            FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)
            FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)
            FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)
            FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)
            FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)
            FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)
            FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)
            FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)
            FCONT(1,NSV(I+NFT))=FCONT(1,NSV(I+NFT))- FXI(I)
            FCONT(2,NSV(I+NFT))=FCONT(2,NSV(I+NFT))- FYI(I)
            FCONT(3,NSV(I+NFT))=FCONT(3,NSV(I+NFT))- FZI(I)
C
           ENDDO
#include "lockoff.inc"
        ENDIF
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .   (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
#include "lockon.inc"
           DO I=1,LLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FNX1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FNY1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FNZ1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FNX2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FNY2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FNZ2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FNX3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FNY3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FNZ3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FNX4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FNY4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FNZ4(I)

            FNCONT(1,NSV(I+NFT))=FNCONT(1,NSV(I+NFT))- FNXI(I)
            FNCONT(2,NSV(I+NFT))=FNCONT(2,NSV(I+NFT))- FNYI(I)
            FNCONT(3,NSV(I+NFT))=FNCONT(3,NSV(I+NFT))- FNZI(I)
C
            FTCONT(1,IX1(I)) =FTCONT(1,IX1(I)) + FTX1(I)
            FTCONT(2,IX1(I)) =FTCONT(2,IX1(I)) + FTY1(I)
            FTCONT(3,IX1(I)) =FTCONT(3,IX1(I)) + FTZ1(I)
            FTCONT(1,IX2(I)) =FTCONT(1,IX2(I)) + FTX2(I)
            FTCONT(2,IX2(I)) =FTCONT(2,IX2(I)) + FTY2(I)
            FTCONT(3,IX2(I)) =FTCONT(3,IX2(I)) + FTZ2(I)
            FTCONT(1,IX3(I)) =FTCONT(1,IX3(I)) + FTX3(I)
            FTCONT(2,IX3(I)) =FTCONT(2,IX3(I)) + FTY3(I)
            FTCONT(3,IX3(I)) =FTCONT(3,IX3(I)) + FTZ3(I)
            FTCONT(1,IX4(I)) =FTCONT(1,IX4(I)) + FTX4(I)
            FTCONT(2,IX4(I)) =FTCONT(2,IX4(I)) + FTY4(I)
            FTCONT(3,IX4(I)) =FTCONT(3,IX4(I)) + FTZ4(I)

            FTCONT(1,NSV(I+NFT))=FTCONT(1,NSV(I+NFT))- FTXI(I)
            FTCONT(2,NSV(I+NFT))=FTCONT(2,NSV(I+NFT))- FTYI(I)
            FTCONT(3,NSV(I+NFT))=FTCONT(3,NSV(I+NFT))- FTZI(I)
           ENDDO
#include "lockoff.inc"
      ENDIF
C
      IF(IBC==0) RETURN
       DO 200 I=LFT,LLT
       IF(IBC==0.OR.XFACE(I)==ZERO)GOTO 200
       IL=I+NFT
       IG=NSV(IL)
       CALL IBCOFF(IBC,ICODT(IG))
 200   CONTINUE
C
      RETURN
      END
