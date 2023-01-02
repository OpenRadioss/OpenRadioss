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
Chd|  PFORC3                        source/elements/beam/pforc3.F 
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        M1LAWP                        source/materials/mat/mat001/m1lawp.F
Chd|        M2LAWP                        source/materials/mat/mat002/m2lawp.F
Chd|        PBILAN                        source/elements/beam/pbilan.F 
Chd|        PCOOR3                        source/elements/beam/pcoor3.F 
Chd|        PCURV3                        source/elements/beam/pcurv3.F 
Chd|        PDEFO3                        source/elements/beam/pdefo3.F 
Chd|        PDLEN3                        source/elements/beam/pdlen3.F 
Chd|        PEVEC3                        source/elements/beam/pevec3.F 
Chd|        PFCUM3                        source/elements/beam/pfcum3.F 
Chd|        PFCUM3P                       source/elements/beam/pfcum3.F 
Chd|        PFINT3                        source/elements/beam/pfint3.F 
Chd|        PMAIN3                        source/elements/beam/pmain3.F 
Chd|        PMCUM3                        source/elements/beam/pmcum3.F 
Chd|        PMCUM3P                       source/elements/beam/pmcum3.F 
Chd|        PPXPY3                        source/elements/beam/ppxpy3.F 
Chd|        SIGEPS44P                     source/materials/mat/mat044/sigeps44p.F
Chd|        THERMEXPPG                    source/elements/beam/thermexpp.F
Chd|        THERMEXPPI                    source/elements/beam/thermexpp.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE PFORC3(ELBUF_STR,
     1                  JFT     ,JLT    ,NEL     ,MTN     ,ISMSTR  , 
     2                  PM      ,NCC    ,X       ,
     3                  F       ,M      ,V       ,R       ,
     4                  GEO     ,PARTSAV,DT2T    ,NELTST  ,ITYPTST , 
     5                  STIFN   ,STIFR  ,FSKY    ,IADP    ,OFFSET  , 
     6                  IPARTP  ,TANI   ,FX1     ,FX2     ,FY1     , 
     7                  FY2     ,FZ1    ,FZ2     ,MX1     ,MX2     , 
     8                  MY1     ,MY2    ,MZ1     ,MZ2     ,IGEO    ,
     9                  IPM     ,BUFMAT ,NPT     ,NPF     ,TF      ,
     A                  GRESAV  ,GRTH   ,IGRTH   ,MSP     ,DMELP   ,
     C                  IOUTPRT ,ITASK  ,JTHE    ,TEMP    ,FTHE    ,
     D                  FTHESKY ,IEXPAN ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE H3D_MOD
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCC(NIXP,*),IADP(2,*),IPARTP(*),IGEO(NPROPGI,*),
     .        IPM(NPROPMI,*),NPF(*),GRTH(*),IGRTH(*)
      INTEGER JFT,JLT,NELTST,ITYPTST,OFFSET,NEL,JTHE,
     .        MTN,ISMSTR,NPT,IOUTPRT,ITASK,IEXPAN
      my_real DT2T ,
     .   PM(NPROPM,*), X(*), F(*), M(*), V(*), R(*),GEO(NPROPG,*),TF(*),
     .   BUFMAT(*),PARTSAV(*),STIFN(*),STIFR(*),FSKY(*),TANI(15,*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   GRESAV(*),MSP(*),DMELP(*),TEMP(*),FTHE(*),
     .   FTHESKY(*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG,IGTYP,NUPARAM,NUVAR,NFUNC,IFUNC(100),IFUNC_ALPHA
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),
     .   NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),OFF(MVSIZ),AL(MVSIZ),EXX(MVSIZ),
     .   EXY(MVSIZ),EXZ(MVSIZ),KXX(MVSIZ),KYY(MVSIZ),KZZ(MVSIZ),
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),M1(MVSIZ),M2(MVSIZ),M3(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .   Y3(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),RX1G(MVSIZ),RX2G(MVSIZ),
     .   RY1G(MVSIZ),RY2G(MVSIZ),RZ1G(MVSIZ),RZ2G(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),VX1G(MVSIZ),VX2G(MVSIZ),
     .   VY1G(MVSIZ),VY2G(MVSIZ),VZ1G(MVSIZ),VZ2G(MVSIZ),
     .   F11(MVSIZ), F12(MVSIZ), F21(MVSIZ),  
     .   F22(MVSIZ), F31(MVSIZ), F32(MVSIZ), 
     .   M11(MVSIZ), M12(MVSIZ), M21(MVSIZ),
     .   M22(MVSIZ), M31(MVSIZ), M32(MVSIZ),TEMPEL(MVSIZ),DTEMP(MVSIZ),
     .   FSCAL_ALPHA,ETH(MVSIZ),DEINTTH,ALPHA,DF
C
      my_real ::  KC,PHIX, CA,CB, AREA, FPHI(MVSIZ,2),DIE(MVSIZ)
      my_real ,DIMENSION(:) ,POINTER :: UVAR
      TYPE(G_BUFEL_),POINTER :: GBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C-----------------------------------------------      
       my_real FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
C-----------------------------------------------
      CALL PCOOR3(X      ,NCC    ,MAT    ,PID    ,NGL    ,  
     .            NC1   ,NC2    ,NC3    ,X1     ,X2 ,
     .            X3    ,Y1     ,Y2     ,Y3     ,Z1 ,
     .            Z2    ,Z3     )
      CALL PEVEC3(GBUF%SKEW,R      ,AL     ,NC1    ,NC2     ,
     .            NC3      ,X1     ,X2     ,X3     ,Y1      ,
     .            Y2       ,Y3     ,Z1     ,Z2     ,Z3      ,
     .            RX1G     ,RX2G   ,RY1G   ,RY2G   ,RZ1G    ,
     .            RZ2G     ,E1X    ,E1Y    ,E1Z    ,E2X     ,
     .            E2Y      ,E2Z    ,E3X    ,E3Y    ,E3Z     )
      IF (ISMSTR /= 0) CALL PPXPY3(GBUF%LENGTH  ,AL)
      CALL PDLEN3(JFT    ,JLT    ,PM        ,GEO        ,GBUF%OFF,
     .            DT2T   ,NELTST ,ITYPTST,STI    ,STIR   ,
     .            MSP    ,DMELP  ,GBUF%G_DT ,GBUF%DT    ,AL      ,
     .            MAT    ,PID    ,NGL       )
      CALL PDEFO3(V     ,EXX    ,EXY     ,EXZ    ,AL      ,
     .            NC1   ,NC2    ,NC3     ,E1X    ,E1Y     ,
     .            E1Z   ,E2X    ,E2Y     ,E2Z    ,E3X     ,
     .            E3Y   ,E3Z    ,VX1G    ,VX2G   ,VY1G    ,
     .            VY2G  ,VZ1G   ,VZ2G    )
      CALL PCURV3(R     ,GEO    ,GBUF%OFF,OFF    ,EXX     ,
     .            EXY   ,EXZ    ,KXX     ,KYY    ,KZZ     ,
     .            AL    ,NC1    ,NC2     ,NC3    ,RX1G    ,
     .            RX2G  ,RY1G   ,RY2G    ,RZ1G   ,RZ2G    ,
     .            E1X	  ,E1Y    ,E1Z     ,E2X    ,E2Y     ,
     .            E2Z	  ,E3X    ,E3Y     ,E3Z    ,PID     )
C---
      IGTYP = IGEO(11,PID(1))
      NUPARAM = IPM(9,MAT(1))
C---
       DIE(JFT:JLT)   = ZERO
      IF(JTHE > 0) THEN 
         DO I=JFT,JLT 
           TEMPEL(I)    = HALF *( TEMP(NC1(I)) + TEMP(NC2(I)))  
           DIE(I) = GBUF%EINT(I) + GBUF%EINT(NEL + I)
         ENDDO
      ENDIF
  
      IF (IEXPAN > 0 .AND. JTHE > 0) THEN
        IF (TT == ZERO) GBUF%TEMP(JFT:JLT) = TEMPEL(JFT:JLT)
        DTEMP(JFT:JLT) = TEMPEL(JFT:JLT) - GBUF%TEMP(JFT:JLT)
        GBUF%TEMP(JFT:JLT) = TEMPEL(JFT:JLT)
c      
        DO I=JFT,JLT 
             IFUNC_ALPHA = IPM(219, MAT(I))
             FSCAL_ALPHA = PM(191, MAT(I))
             ALPHA = FSCAL_ALPHA*FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,DF)
             ETH(I) = ALPHA*DTEMP(I)
             DEINTTH = - HALF*GBUF%FOR(I)*ETH(I)*AL(I)*OFF(I) 
             GBUF%EINTTH(I) = GBUF%EINTTH(I)  + DEINTTH
!!             GBUF%EINT(I) = GBUF%EINT(I) + DEINTTH
        ENDDO 
      ENDIF   
C---
      IF (IGTYP == 3) THEN
C----   Poutre formulation globale
c
         
        SELECT CASE(MTN)
c
          CASE (1)
            CALL M1LAWP(
     .           PM     ,GBUF%FOR  ,GBUF%MOM  ,GBUF%EINT  ,GEO,
     .           OFF    ,EXX       ,EXY       ,EXZ        ,KXX,
     .           KYY    ,KZZ       ,AL        ,F1         ,F2 ,
     .           F3     ,M1        ,M2        ,M3         ,NEL,
     .           MAT    ,PID       )
c
          CASE (2)  ! Johnson-Cook
            CALL M2LAWP(
     .           PM     ,GBUF%FOR   ,GBUF%MOM  ,GBUF%EINT  ,GEO,        
     .           OFF    ,GBUF%PLA   ,EXX       ,EXY        ,EXZ,
     .           KXX    ,KYY        ,KZZ       ,AL         ,F1 ,
     .           F2     ,F3         ,M1        ,M2         ,M3 ,
     .           NEL    ,MAT        ,PID       ,NGL        )
c
          CASE (44)  ! Cowper-Symonds
            NUVAR = GBUF%G_NUVAR
            UVAR  => GBUF%VAR
            NFUNC = IPM(10,MAT(1))
            DO I=1,NFUNC
              IFUNC(I) = IPM(10+I,MAT(1))
            ENDDO
            BUFLY   => ELBUF_STR%BUFLY(1)
            NUVAR   =  BUFLY%NVAR_MAT
            CALL SIGEPS44P(
     .           NEL      ,NGL      ,MAT      ,PID      ,NUPARAM  ,BUFMAT  ,
     .           IPM      ,GEO      ,OFF      ,GBUF%PLA ,GBUF%EINT,AL      ,
     .           EXX      ,EXY      ,EXZ      ,KXX      ,KYY      ,KZZ     ,
     .           F1       ,F2       ,F3       ,M1       ,M2       ,M3      ,
     .           GBUF%FOR ,GBUF%MOM ,PM       ,NUVAR    ,UVAR     ,NFUNC   , 
     .           IFUNC    ,TF       ,NPF      )
c
        END SELECT
C----
      ELSEIF (IGTYP == 18) THEN  
C----   Poutre a section intergree
        CALL PMAIN3(ELBUF_STR,
     1              JFT       ,JLT         ,NEL       ,NPT      ,MTN     ,
     2               MAT     ,PID     ,NGL     ,PM      ,IPM     ,
     3              GEO       ,IGEO        ,OFF       ,GBUF%FOR ,GBUF%MOM,
     4              GBUF%EINT ,GBUF%LENGTH ,GBUF%EPSD ,BUFMAT   ,NPF     ,
     5              TF        ,EXX         ,EXY       ,EXZ      ,KXX     ,
     6              KYY       ,KZZ         ,F1        ,F2       ,F3      ,
     7              M1        ,M2          ,M3        ,JTHE    ,TEMPEL)
      ENDIF
C  
C      Expanson
C
      IF(IEXPAN > 0 .AND. JTHE > 0) THEN
          IF (IGTYP == 3) THEN   
            CALL THERMEXPPG(NEL    ,MAT       ,PID          ,PM     ,GEO  ,
     .                      OFF    ,ETH       ,GBUF%FOR     ,GBUF%EINT ) 
          
          ELSEIF(IGTYP == 18) THEN
           CALL THERMEXPPI(ELBUF_STR,   
     1              NEL     ,NPT      ,MAT       ,PID       ,PM       , 
     2              GEO      ,AL      ,ETH       ,OFF      ,GBUF%FOR  ,
     3              GBUF%EINT)
          ENDIF
C          
          DO I=JFT,JLT
             DEINTTH = - HALF*GBUF%FOR(I)*ETH(I)*AL(I)*OFF(I) 
             GBUF%EINT(I) = GBUF%EINT(I) + DEINTTH
          ENDDO
      ENDIF
      IF(JTHE > 0) THEN 
         DO I=JFT,JLT 
           DIE(I) = (GBUF%EINT(I) + GBUF%EINT(NEL + I) - DIE(I))*PM(90,MAT(I))
         ENDDO
      ENDIF
            
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG = MOD(NCYCLE,NCPRI)
      IF (IOUTPRT > 0)     
     .  CALL PBILAN(PM     ,V      ,GBUF%EINT ,GEO     ,PARTSAV,
     2              IPARTP ,TANI   ,GBUF%FOR  ,GBUF%MOM,GRESAV ,
     3              GRTH   ,IGRTH  ,GBUF%OFF  ,NEL     ,AL     ,
     4              NC1    ,NC2    ,NC3       ,E1X     ,E1Y    ,
     5              E1Z    ,E2X    ,E2Y       ,E2Z     ,MAT    ,
     6              PID    ,VX1G   ,VX2G      ,VY1G    ,VY2G   ,
     7              VZ1G   ,VZ2G   ,X1        ,X2      ,Y1     ,
     8              Y2     ,Z1     ,Z2        ,ITASK   ,H3D_DATA)
C----------------------------
C     FORCES INTERNES
C----------------------------
      CALL PFINT3(GBUF%FOR ,GBUF%MOM ,GEO     ,GBUF%OFF   ,OFF,
     .            AL       ,F1       ,F2      ,F3         ,M1         ,
     .            M2       ,M3       ,STI     ,STIR       ,NEL,
     .            PID      ,F11      ,F12     ,F21        ,F22,
     .            F31      ,F32      ,M11     ,M12        ,M21,
     .            M22      ,M31      ,M32     )
C-------------------------
c     Thermique des coques 
C--------------------------
C
       IF (JTHE > 0) THEN
        DO I=JFT,JLT
          CA = PM(75,MAT(I))
          CB = PM(76,MAT(I))
          AREA =GEO(1,PID(I)) 
          KC = (CA + CB*TEMPEL(I))*DT2T *AREA/AL(I)   
          PHIX = KC*(TEMP(NC2(I)) - TEMP(NC1(I)))
C
C force thermique nodale
C
          FPHI(I,1) = HALF * DIE(I) + PHIX ! 
          FPHI(I,2) = HALF * DIE(I) - PHIX
         ENDDO
       ENDIF
C-------------------------
C     ASSEMBLAGE
C-------------------------
      IF (IPARIT == 0) THEN
        CALL PFCUM3(F       ,STI     ,STIFN   ,FX1     ,FX2     ,
     .              FY1   ,FY2   ,FZ1    ,FZ2  ,NC1,
     .              NC2   ,NC3   ,E1X    ,E1Y  ,E1Z,
     .              E2X   ,E2Y   ,E2Z    ,E3X  ,E3Y,
     .              E3Z   ,F11   ,F12    ,F21  ,F22,
     .              F31   ,F32   ,FPHI ,FTHE )
        CALL PMCUM3(M,STIR,STIFR,MX1,MX2,
     .              MY1 ,MY2  ,MZ1   ,MZ2  ,NC1,
     .              NC2 ,NC3  ,E1X   ,E1Y  ,E1Z,
     .              E2X ,E2Y  ,E2Z   ,E3X  ,E3Y,
     .              E3Z ,M11  ,M12   ,M21  ,M22,
     .              M31 ,M32  )
      ELSE
        CALL PFCUM3P(STI,FSKY,FSKY,IADP,FX1,
     .               FX2 ,FY1  ,FY2  ,FZ1  ,FZ2,
     .               NC1 ,NC2  ,NC3  ,E1X  ,E1Y,
     .               E1Z ,E2X  ,E2Y  ,E2Z  ,E3X,
     .               E3Y ,E3Z  ,F11  ,F12  ,F21,
     .               F22 ,F31  ,F32  , FPHI,
     .               FTHESKY)
     
        CALL PMCUM3P(STIR,FSKY,FSKY,IADP,MX1,
     .               MX2  ,MY1  ,MY2  ,MZ1  ,MZ2,
     .               NC1  ,NC2  ,NC3  ,E1X  ,E1Y,
     .               E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X,
     .               E3Y  ,E3Z  ,M11  ,M12  ,M21,
     .               M22  ,M31  ,M32  )
      ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE PFORC3
