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
Cgw          CFLUID                          /resol/prelecfluid.F
Chd|====================================================================
Chd|  PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        VFLUID                        source/elements/solid/solide/prelecflow.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE PRELECFLOW(ELBUF_TAB,NGROUNC,IGROUNC,IPARG  ,NBSDVOIS,
     1                      NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,PHI1 ,
     2                      PHI2  ,POR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C----6---------------------------------------------------------------7---------8
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "com_xfem1.inc"
#include      "rnur_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NBSDVOIS(*),IGROUNC(*),
     .   NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),NGROUNC
C     REAL
      my_real
     .   PHI1(*),PHI2(*),POR(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDXOF(MVSIZ)
      INTEGER I,II,J,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK,IPLA,
     .        LENCOM,KAD,NEL,NF1,IG
      my_real,
     .   DIMENSION(:), POINTER :: UVAR
C======================================================================|
C   
      LENCOM = 0    
      IF(NSPMD > 1)LENCOM=NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1) 
C Boucle parallele dynamique SMP
C
!$OMP DO 

       DO IG = 1, NGROUNC
            NG = IGROUNC(IG)
            IF(IPARG(8,NG).EQ.1)GOTO 150
            ITY   =IPARG(5,NG)
            IF (IDDW.GT.0) CALL STARTIMEG(NG)
            MLW     = IPARG(1,NG)
C MLW = 777 ---->cell open foam       
           IF (MLW /= 77 ) GOTO 150
C           
           CALL INITBUF (IPARG    ,NG      ,
     2        MLW     ,NEL     ,NFT     ,KAD     ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
C   
            ISOLNOD = IPARG(28,NG)
            
C---------------------------------
            LFT   = 1 
            LLT   = MIN(NVSIZ,NEL)
            JFT=LFT
            JLT=LLT
            NF1 = NFT+1
            IAD = KAD
            JSPH=0
C----6---------------------------------------------------------------7---------8
        IF(ITY.EQ.1 .AND. JLAG.EQ.1)THEN
C-------------------------
C          STANDARD 8 NODE SOLID ELEMENT (JHBE = 1,2,101,102)
C-------------------------
           UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
           CALL VFLUID(UVAR , NEL, MLW, PHI1, PHI2 ,POR)
        ENDIF
C-------------------------
        IF (IDDW.GT.0) CALL STOPTIMEG(NG)

 150     CONTINUE 
      ENDDO           
C     
      CALL MY_BARRIER
C
C-----------------------------
C       ECHANGES SPMD  SUR ELTS VOISINS
C-----------------------------       
      IF (NSPMD > 1) THEN       
cc        LENCOMN=NBRCVOIS(MAXPROC+1)+NBSDVOIS(MAXPROC+1)
C
C Partie non parallele smt
C
!$OMP SINGLE
       CALL SPMD_E1VOIS(PHI1     ,NERCVOIS,NESDVOIS,LERCVOIS,
     1                     LESDVOIS,LENCOM  )
C       
       CALL SPMD_E1VOIS(PHI2     ,NERCVOIS,NESDVOIS,LERCVOIS,
     1                     LESDVOIS,LENCOM  ) 
C       
       CALL SPMD_E1VOIS(POR     ,NERCVOIS,NESDVOIS,LERCVOIS,
     1                     LESDVOIS,LENCOM  )
C Fin Partie non parallele smt
!$OMP END SINGLE       
      ENDIF
      END SUBROUTINE PRELECFLOW
C --- fluid structure       
Chd|====================================================================
Chd|  VFLUID                        source/elements/solid/solide/prelecflow.F
Chd|-- called by -----------
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VFLUID (UVAR, NEL, MLW , PHI1, PHI2,PHI3)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MLW
C     REAL
      my_real
     .   DT2T
C
C     REAL
      my_real
     .  PHI1(*),PHI2(*),UVAR(*),PHI3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,IE
       my_real
     .  VOL0
C======================================================================|
      DO I=LFT,LLT
        IE =NFT+I
        
        VOL0 = UVAR(I + 2*NEL)
C        
C density, energy, alpha  
        PHI1(IE) = UVAR(I)
        PHI2(IE) = UVAR(I + NEL) 
        PHI3(IE) = UVAR(I + 20*NEL)
C       
        UVAR(I) =  PHI1(IE)*VOL0
        UVAR(I + NEL) = PHI2(IE)*VOL0
      ENDDO    
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
      RETURN
      END SUBROUTINE VFLUID
