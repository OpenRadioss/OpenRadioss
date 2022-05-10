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
Chd|  DMASANI0                      source/output/anim/generate/dmasani0.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE DMASANI0(X ,D       ,ELBUF_TAB,GEO     ,IPARG   ,
     2              MAS     ,PM      ,EL2FA    ,IPART   ,IPARTSP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   MAS(*) ,PM(NPROPM,*),GEO(NPROPG,*),X(3,*),D(3,*)
      INTEGER IPARG(NPARG,*),EL2FA(*),IPART(LIPART1,*),
     .        IPARTSP(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   OFF, VALUE
      INTEGER I, NG, NEL, N, MT,IALEL,NN1,NN2,IPRT
      REAL R4
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C=======================================================================
      NN1 = 1
      NN2 = NN1 + (NUMSPH+MAXPJET)
C-----------------------------------------------
C       face ext
C-----------------------------------------------
      DO 490 NG=1,NGROUP
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )            
        IF (ITY == 51) THEN
C-----------------------------------------------
C          PARTICULES SPH.
C-----------------------------------------------
           LFT=1
           LLT=NEL
           GBUF => ELBUF_TAB(NG)%GBUF
           IALEL=IPARG(7,NG)+IPARG(11,NG)
           DO I=LFT,LLT
             N = I + NFT
             IF(EL2FA(NN1+N).NE.0)THEN
               IF(IALEL.EQ.0)THEN
                 IPRT=IPARTSP(N)
                 MT  =IPART(1,IPRT)
                 VALUE=PM(89,MT)*GBUF%VOL(I)
               ELSE
                 OFF = MIN(GBUF%OFF(I),UN)
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF
               ENDIF
               MAS(EL2FA(NN1+N)) = VALUE
             ENDIF
           ENDDO
        ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES GROUPES
C-----------------------------------------------
 490  CONTINUE
C-----------------------------------------------
      RETURN
      END
