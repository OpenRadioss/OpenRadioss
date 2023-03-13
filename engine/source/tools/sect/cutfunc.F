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
Chd|  CUTFUNC                       source/tools/sect/cutfunc.F   
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE CUTFUNC(FUNC,IVOIS,AL,NODCUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real FUNC(*), AL(*)
      INTEGER IVOIS(2,*),NODCUT,I 
C-----------------------------------------------
      REAL R4
C-----------------------------------------------
      DO I=1,NODCUT
         R4 =AL(I)*FUNC(IVOIS(2,I))+(ONE-AL(I))*FUNC(IVOIS(1,I)) 
         CALL WRITE_R_C(R4,1)
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CUTFUNCE                      source/tools/sect/cutfunc.F   
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE CUTFUNCE(NC   ,NUMEL,ELBUF_TAB,IFUNC  ,
     .                    IPARG,PM   ,IXS    )
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
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PM(NPROPM,*)
      INTEGER IPARG(NPARG,*),NC(5,*),IXS(NIXS,*)
      INTEGER NUMEL,IFUNC
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
      INTEGER I,J,N,IC,IL,IL_OLD,NG,NEL,MLW,JTURB,MT,G_PLA,II(6)
C     REAL
      my_real
     .   OFF,  P, VONM2, VONM, S1, S2, S12, S3, VALUE     
      REAL R4
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C=======================================================================
      IL_OLD = -1
      DO IC=1,NUMEL
       IL = NC(5,IC)
       IF(IL/=IL_OLD)THEN
         IL_OLD = IL
C
         DO 490 NG=1,NGROUP
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IF (ITY/=1)      GOTO 490
          IF (NEL+NFT<IL) GOTO 490
C-----------------------------------------------
C         SOLID ELEMENT
C-----------------------------------------------
           GBUF => ELBUF_TAB(NG)%GBUF
           LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
           MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
           I = IL - NFT
           LLT = NEL
           JTURB=IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))
C
           N = I + NFT
           OFF = GBUF%OFF(I)
!
           DO J=1,6
             II(J) = NEL*(J-1)
           ENDDO
!
c-----------
           IF (IFUNC==1 .AND. GBUF%G_PLA > 0) THEN
             VALUE = GBUF%PLA(I)
c-----------
           ELSEIF (IFUNC == 2) THEN
             VALUE = GBUF%RHO(I)
c-----------
           ELSEIF (IFUNC == 3) THEN
             VALUE = GBUF%EINT(I)
c-----------
           ELSEIF(IFUNC==4 .AND. JTHE > 0) THEN
             VALUE = GBUF%TEMP(I)
c-----------
           ELSEIF(IFUNC==6.OR.IFUNC==7)THEN
             P = - (GBUF%SIG(II(1)+I)
     .            + GBUF%SIG(II(2)+I)
     .            + GBUF%SIG(II(3)+I)) / THREE
             VALUE = P    
             IF (IFUNC==7) THEN
               S1 = GBUF%SIG(II(1)+I) + P
               S2 = GBUF%SIG(II(2)+I) + P
               S3 = GBUF%SIG(II(3)+I) + P
               VONM2 = THREE*(GBUF%SIG(II(4)+I)**2 +      
     .                        GBUF%SIG(II(5)+I)**2 +      
     .                        GBUF%SIG(II(6)+I)**2 +      
     .                   HALF*(S1*S1+S2*S2+S3*S3))              
              VONM= SQRT(VONM2)
              VALUE = VONM
            ENDIF
c-----------
           ELSEIF (IFUNC==8 . AND. JTURB/=0) THEN
C            ENERGIE TURBULENTE     
             VALUE = GBUF%RK(I) 
           ELSEIF (IFUNC==9) THEN
C           VISCOSITE TURBULENTE
            IF((MLW==6 .OR. MLW==17).AND.JTURB/=0)THEN
                MT=IXS(1,N)
                VALUE=PM(81,MT)*GBUF%RK(I)**2/
     .                  MAX(EM15,GBUF%RE(I))
               ELSEIF (MLW==46 .OR. MLW==47)THEN
                 VALUE = MBUF%VAR(I)
               ELSE
              VALUE = ZERO
               ENDIF
C
           ELSEIF(IFUNC==10)THEN
C            VORTICITE
             IF(MLW==6 .OR. MLW==17)THEN
               VALUE = LBUF%VK(I)     
             ELSEIF(MLW==46 .OR. MLW==47)THEN
               VALUE = MBUF%VAR(I)      
             ELSE
               VALUE = ZERO
             ENDIF
C
           ELSEIF(IFUNC>=14.AND.IFUNC<=19)THEN
             VALUE = GBUF%SIG(II(IFUNC - 13) + I)
           ELSE
             VALUE = ZERO
           ENDIF
           GOTO 500
C
 490     CONTINUE
 500     CONTINUE
       ENDIF
C-----------------------------------------------
       R4 = VALUE
       CALL WRITE_R_C(R4,1)
      ENDDO
C-----------------------------------------------
      RETURN
      END
