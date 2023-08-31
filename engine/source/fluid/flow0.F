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
Chd|  FLOW0                         source/fluid/flow0.F          
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        DAASOLV                       source/fluid/daasolv.F        
Chd|        DAASOLVP                      source/fluid/daasolvp.F       
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FLOW0(IFLOW  , RFLOW  , WIFLOW , WRFLOW, X      ,
     .                 V      , A      , NPC    , TF    , SENSOR_TAB,
     .                 NBGAUGE, LGAUGE , GAUGE  , NSENSOR, 
     .                 IGRV   , AGRV   , NFUNCT , PYTHON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
      USE ANIM_MOD
      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr07_c.inc"
#include      "flowcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR,NBGAUGE,NFUNCT
      INTEGER IFLOW(*), WIFLOW(*), NPC(*),LGAUGE(3,*)
      INTEGER IGRV(NIGRV,*)
      my_real
     .        RFLOW(*), WRFLOW(*), X(3,*), V(3,*), A(3,*), TF(*),
     .        GAUGE(LLGAUGE,*), AGRV(LFACGRV,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, NNN, NEL_LOC,
     .        II1, II2, II3, II4, II5, II6, II7, IR1, IR2, IR3, IR4, 
     .        IR5, IR6, IR7, IR8, IR9, IR10, IR11, II8, II9, II10, II11, IADIP, IADH, IADG, 
     .        II12, II13, II14, IPINT, NDIM, JFORM, FREESURF
C-----------------------------------------------------------------------------------
C Description des tableaux ITYP=1
C     II1  -> IFLOW   : parametres entiers
C     II2  -> IBUF    : correspondances local-global noeuds surface
C     II3  -> ELEM    : connectivite locale des triangles
C     II4  -> IINOUT  : variables entieres pour les surfaces entrees-sorties
C     II5  -> IBUFI   : correspondance local-global noeuds internes
C     II6  -> ITAGEL  : tag des elements des surfaces d'entrees-sorties
C----- tableaux SPMD
C     II7  -> IBUFR   : indice ligne noeud local (0 si pas sur ce proc)
C     II8  -> IBUFC   : indice colonne noeud local (0 si pas sur ce proc)
C     II9  -> IBUFL   : liste noeuds surface sur processeur courant pour X, V, A
C     II10 -> CNP     : attribution processeur pour noeuds surface pour X, V, A
C     II11 -> IBUFIL  : liste noeuds internes sur processeur courant pour X, V, A
C     II12 -> CNPI    : attribution processeur pour noeuds internes pour X, V, A
C     II13 -> IBUFELR : indice ligne elem local (0 si pas sur ce proc)
C     II14 -> IBUFELC : indice colonne elem local (0 si pas sur ce proc)
C
C     IR1  -> RFLOW  : parametres reels
C     IR2  -> PHI    : potentiel
C     IR3  -> PRES   : pression
C     IR4  -> U      : vitesse
C     IR5  -> RINOUT : variables reelles pour les surfaces entrees-sorties
C-----------------------------------------------------------------------------------
C Description des tableaux ITYP=3
C     II1  -> IFLOW   : parametres entiers
C     II2  -> IBUF    : correspondances local-global noeuds surface
C     II3  -> ELEM    : connectivite locale des triangles/quadrangles+flag
C     II4  -> IBUF_L  : correspondances local-global noeuds surface (a supprimer en SPMD)
C     II5  -> SHELL_GA: correspondances local shell-gauge
C     II6  -> CNP     : SPMD nombre de processeur pour chaque noeud
C engine
C     II7  -> IPIV    : lapack resolution nel > nelmax
C     II8  -> IBUFELR : SPMD
C     II9  -> IBUFELC : SPMD
C
C     IR1  -> RFLOW  : parametres reels
C     IR2  -> NORMAL : normale
C     IR3  -> TA     : arrival time
C     IR4  -> AREA   : element area
C     IR5  -> COSG   : direction cosine
C     IR6  -> DIST   : distance charge
C     IR7  -> MFLE   : fluide mass matrix (inverse) nel < nelmax
C     IR7  -> MFLE   : C**t B + B**t C  nel >= nelmax
C     IR8  -> ACCF   : acceleration point fluide
C     IR9  -> PS     : scattered pressure
C     IR10 -> PTI    : incident pressure time integral
C     IR11 -> CMAT   : Matrice C (nel >= nelmax)
C-----------------------------------------------------------------------------------
      IPINT=0
      IF ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.
     .   MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15) 
     .   IPINT=1
C
      IADI=0
      IADR=0
      DO I=1,NFLOW
         ITYP=IFLOW(IADI+2)
         NNO=IFLOW(IADI+5)
         NEL=IFLOW(IADI+6)
         II1=IADI+1
         II2=II1+NIFLOW
         II3=II2+NNO
         IR1=IADR+1
         IR2=IR1+NRFLOW
         IF (ITYP==1) THEN
            NINOUT=IFLOW(IADI+4)
            NNN=IFLOW(IADI+7)
C
            II4=II3+3*NEL
            II5=II4+NINOUT*NIIOFLOW
            II6=II5+NNN
            IF (NSPMD > 1) THEN
               II7=II6+NEL   
               II8=II7+NNO   
               II9=II8+NNO
               II10=II9+NNO  
               II11=II10+NNO
               II12=II11+NNN
               II13=II12+NNN 
               II14=II13+NEL 
            ELSE
               II7=II6+NEL
               II8=II7
               II9=II8
               II10=II9+NNO
               II11=II10
               II12=II11+NNN
               II13=II12
               II14=II13
            ENDIF
            IR3=IR2+NNO+NNN
            IR4=IR3+NNO+NNN
            IR5=IR4+3*(NNO+NNN)
C
            IADIP=IFLOW(IADI+10)
            IADH=IFLOW(IADI+11)
            IADG=IFLOW(IADI+20)
C
            CALL INCPFLOW(
     .      NNO,         NEL,         NINOUT,      NNN,         IFLOW(II1),
     .      IFLOW(II2),  IFLOW(II3),  IFLOW(II4),  IFLOW(II5),  IFLOW(II7),
     .      IFLOW(II8),  IFLOW(II9),  RFLOW(IR1),  RFLOW(IR2),  RFLOW(IR3),
     .      RFLOW(IR4),  RFLOW(IR5),  X,           V,           A,        
     .      NPC,         TF        ,  NSENSOR    , SENSOR_TAB ,
     .      IFLOW(II10), IFLOW(II6),  IFLOW(II13), IFLOW(II14), WIFLOW(IADIP), 
     .      WRFLOW(IADH),WRFLOW(IADG),IFLOW(II11), IFLOW(II12), IPINT,
     .      PYTHON) 
C
         ELSEIF(ITYP == 3) THEN
            JFORM    = IFLOW(IADI+4)
            FREESURF = IFLOW(IADI+25)
            IF(JFORM==1) THEN
               II4=II3+3*NEL
               II5=II4+NNO
               II6=II5
               NDIM=3
            ELSEIF(JFORM==2) THEN
               II4=II3+5*NEL
               II5=II4+NNO
               II6=II5+NBGAUGE
               NDIM=5
            ENDIF

            IR3 = IR2+NEL*3
            IR4 = IR3+NEL*FREESURF
            IR5 = IR4+NEL
            IR6 = IR5+NEL*FREESURF
            IR7 = IR6+NEL*FREESURF
            IR8 = IR7+NEL*NEL
            IR9 = IR8+NEL
            IR10= IR9+NEL
            IR11= IR10+NEL

            IF(NSPMD == 1) THEN
              II7=II6
              CALL DAASOLV(NDIM, NNO, NEL,
     .                     IFLOW(II1), IFLOW(II2), IFLOW(II3), IFLOW(II5), 
     .                     RFLOW(IR1), RFLOW(IR2), RFLOW(IR3), RFLOW(IR4), RFLOW(IR5), RFLOW(IR6), 
     .                     RFLOW(IR7), RFLOW(IR8), RFLOW(IR9), RFLOW(IR10),X, V, A, NPC, TF,  
     .                     NBGAUGE, LGAUGE, GAUGE, NSENSOR, SENSOR_TAB, IGRV, AGRV,
     .                     RFLOW(IR11),IFLOW(II7), NFUNCT, PYTHON)
            ELSE
              II7=II6+NNO
              II8=II7+NEL
              II9=II8+NEL
              NEL_LOC =IFLOW(IADI+20)
              IADIP =IFLOW(IADI+10)
              IADH  =IFLOW(IADI+11)
              CALL DAASOLVP(NDIM, NNO, NEL, NEL_LOC,
     .                      IFLOW(II1), IFLOW(II2), IFLOW(II3), IFLOW(II4), IFLOW(II5), IFLOW(II6), 
     .                      RFLOW(IR1), RFLOW(IR2), RFLOW(IR3), RFLOW(IR4), RFLOW(IR5), RFLOW(IR6), 
     .                      RFLOW(IR7), RFLOW(IR8), RFLOW(IR9), RFLOW(IR10),X, V, A, NPC, TF,  
     .                      NBGAUGE, LGAUGE, GAUGE, NSENSOR, SENSOR_TAB, IGRV, AGRV,
     .                      RFLOW(IR11),WIFLOW(IADIP), WRFLOW(IADH), IFLOW(II8), IFLOW(II9),
     .                      NFUNCT, PYTHON)
            ENDIF
         ENDIF
         IADR=IADR+IFLOW(IADI+15)
         IADI=IADI+IFLOW(IADI+14)
      ENDDO
C
      RETURN
      END
      
