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
Chd|  HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        STACK_VAR_MOD                 share/modules1/stack_var_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_PROPERTIES(IGEO,NSPHSOL,NPLY,NSUB,NISUB,LSUBMODEL)
C============================================================================
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE STACK_VAR_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr21_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),NSPHSOL
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER ,DIMENSION (NUMGEO + NUMSTACK) :: NPLY,NSUB,NISUB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, IGTYP, ISMSTR, NIP, J, IR1X, IR1Y, IR1Z, IREP,
     .   IR2X, IR2Y, IR2Z, ISHEAR, IRX, IROT, IMODE, IP, ISTRAIN,I8PT,
     .   ISK,ITU,IRB,IHON,IHBE,IPLAST,ITHK,IBID,IHBEOUTP,K,N,LAMINATE,
     .   IGFLU, IDS, NSHELL, NSHSUP, NSHINF, FLGBADI, NBADI,UID,
     .   NSST_D, NSST_DS, NPSH, ICPRE, ICSTR ,NPTS,ISEN,ISORTH,
     .   NSPHDIR, ID_SENS,D1,D2,D3,N1,N2,N3,INTRULE,NN,NS,NIS, NUMS,
     .   NLAM,NINTS,IFRAM,CPT,INUM,ID,IS,LAMINAT
      DATA NSHELL /0/, NSHSUP /0/, NSHINF /0/
      my_real
     .    FN, FT, DX, ANGL,PUN,HTEST,HCLOS,CVIS,RBID,VX,VY,VZ,
     .    KNOT1,KNOT2,KNOT3,R5(5),DN
      CHARACTER IDTITL*nchartitle,MESS*40,KEY*ncharline, 
     .          STRING*ncharfield,CHROT*7,SOLVERKEYWORD*ncharline
      INTEGER ISH3N,ITET4,IPLAS,NPT,ISROT,NP,LAMIN,NSUB_STACK,NISUB_STACK,
     .        NPLY_STACK,NPLY_SUB
      LOGICAL IS_AVAILABLE
     
!!      TYPE(STACK_INFO_ ) , DIMENSION(:), POINTER :: STACK_INFO
C=======================================================================

      ALLOCATE(NUMGEOSTACK(NUMGEO + NUMSTACK))
      NUMGEOSTACK(:) = 0     ! global variable - STACK_VAR_MOD
      NPROP_STACK = 0        ! global variable - STACK_VAR_MOD
C
      NSUB (:) = 0
      NISUB(:) = 0
      NPLY (:) = 0
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C START BROWSING MODEL PARTS
C--------------------------------------------------
      CALL HM_OPTION_START('PROPERTY')
C--------------------------------------------------
C BROWSING MODEL PROPERTIES 1->HM_NUMGEO
C--------------------------------------------------
      NUMELTG6 = 0
      I = 0
      DO CPT=1,HM_NUMGEO
        I = I + 1
        KEY = ''
        SOLVERKEYWORD = ''
        IDTITL = ''
C--------------------------------------------------
C EXTRACT DATAS OF /PART/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = IG,
     .                       KEYWORD2 = KEY)
C--------------------------------------------------
        SELECT CASE(KEY(1:LEN_TRIM(KEY)))
          CASE ('TYPE0','VOID')
C--------------------------------------------------
C         HM READING PROPERTY TYPE0 (VOID)
C--------------------------------------------------
          CASE ('TYPE1','TYPE01','SHELL','TYPE9','TYPE09','SH_ORTH','TYPE11','SH_SANDW',
     .          'SH_FABR','TYPE16')
C--------------------------------------------------
C         HM READING PROPERTY TYPE1 (SHELL)
C--------------------------------------------------
           CALL HM_GET_INTV('Ishell',IHBE,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INTV('ISMSTR',ISMSTR,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INTV('ISH3',ISH3N,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INTV('CVIS',CVIS,IS_AVAILABLE,LSUBMODEL)
           IF(ISH3N==31.AND.NUMELTG>0) NUMELTG6 = 1
           
          CASE ('TYPE17','STACK')
C--------------------------------------------------
C      HM READING PROPERTY TYPE17 (SHELL)
C-------------------------------------------------- 
            IGTYP=17
            NPROP_STACK = NPROP_STACK + 1
            NUMGEOSTACK(I) = NPROP_STACK  
C
           CALL HM_GET_INTV('laminateconfig'    ,LAMIN, IS_AVAILABLE, LSUBMODEL)
c
           NSUB_STACK = 0
           NISUB_STACK = 0 
           NPLY_STACK = 0
           IF (LAMIN  > 0) THEN
             CALL HM_GET_INTV('sublaminateidlistmax' ,NSUB_STACK,  IS_AVAILABLE, LSUBMODEL)
             CALL HM_GET_INTV('interfacepairsize'    ,NISUB_STACK,  IS_AVAILABLE, LSUBMODEL)
            
             DO IS = 1,NSUB_STACK
                CALL HM_GET_INT_ARRAY_INDEX('plyidlistmax',NPLY_SUB,IS,IS_AVAILABLE,LSUBMODEL)
                NPLY_STACK = NPLY_STACK + NPLY_SUB
             END DO
c
           ELSE  ! property defined by a list of plies
             CALL HM_GET_INTV('plyidlistmax' ,NPLY_STACK ,IS_AVAILABLE ,LSUBMODEL)
           END IF  
           NPLY(NPROP_STACK) = NPLY_STACK
           NSUB(NPROP_STACK) = NSUB_STACK
           NISUB(NPROP_STACK)= NISUB_STACK 
C--------------------------------------------------
C          HM READING PROPERTY TYPE14,6 (SOLID)
C--------------------------------------------------
          CASE ('TYPE6','SOL_ORTH')
           CALL HM_GET_INTV('Itetra4',ITET4,IS_AVAILABLE,LSUBMODEL)
           IF(ITET4 == 0)  ITET4 = ITET4_D
           IF(ITET4 == 1)IISROT = 1
           CALL HM_GET_INTV('Ndir',NSPHDIR,IS_AVAILABLE,LSUBMODEL)
           IGEO(1,I) =IG
           IGEO(37,I)=NSPHDIR
           IF(NSPHDIR/=0)NSPHSOL=1
C--------------------------------------------------
C          HM READING PROPERTY TYPE51 (SHELL)
C--------------------------------------------------
          CASE ('TYPE51')
            NPROP_STACK    = NPROP_STACK + 1
            NUMGEOSTACK(I) = NPROP_STACK
c  
           CALL HM_GET_INTV('laminateconfig'    ,LAMINATE, IS_AVAILABLE, LSUBMODEL)
           IF (LAMINATE > 0) THEN
             CALL HM_GET_INTV('sublaminateidlistmax' ,NLAM,  IS_AVAILABLE, LSUBMODEL)
             CALL HM_GET_INTV('interfacepairsize'    ,NINTS,  IS_AVAILABLE, LSUBMODEL)
             NSUB(NPROP_STACK)  = NLAM
             NISUB(NPROP_STACK) = NINTS
             DO IS = 1,NLAM
               CALL HM_GET_INT_ARRAY_2INDEXES('plyidlistmax',NP,IS,1,IS_AVAILABLE,LSUBMODEL)
               NPLY(NPROP_STACK) = NPLY(NPROP_STACK) + NP
             END DO
           ELSE
             CALL HM_GET_INTV('plyidlistmax'      ,NP,  IS_AVAILABLE, LSUBMODEL)
             NPLY(NPROP_STACK) = NPLY(NPROP_STACK) + NP
           END IF
C-------------------------------------------------
C         HM READING PROPERTY TYPE14 (SOLID)
C--------------------------------------------------
          CASE ('TYPE14','SOLID')
           CALL HM_GET_INTV('I_rot',ITET4,IS_AVAILABLE,LSUBMODEL)
           IF(ITET4 == 0)  ITET4 = ITET4_D
           IF(ITET4 == 1)IISROT = 1
           CALL HM_GET_INTV('Ndir',NSPHDIR,IS_AVAILABLE,LSUBMODEL)
           IGEO(1,I) =IG
           IGEO(37,I)=NSPHDIR
           IF(NSPHDIR/=0)NSPHSOL=1
C--------------------------------------------------
          CASE ('PCOMPP')
            IGTYP=52
C--------------------------------------------------
          CASE ('TYPE29','TYPE30','TYPE31',
     .          'USER1' ,'USER2' ,'USER3')
            IISROT = 1
C--------------------------------------------------
        END SELECT
      ENDDO
c-----------------------------------------------------------------------
c     stack object prelecture
c-----------------------------------------------------------------------
C
      IF(NUMSTACK > 0) THEN
           CALL HM_OPTION_START('/STACK')
           DO 700 I=1,NUMSTACK 
            CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = IG,KEYWORD2 = KEY)
            NPROP_STACK = NPROP_STACK + 1
            NUMGEOSTACK(NUMGEO + I) = NPROP_STACK
            CALL HM_GET_INTV('laminateconfig'    ,LAMINATE, IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INTV('laminateconfig'    ,LAMIN   , IS_AVAILABLE, LSUBMODEL)
                   
            IF (LAMINATE > 0) THEN                                                            
              CALL HM_GET_INTV('sublaminateidlistmax' ,NLAM,  IS_AVAILABLE, LSUBMODEL)        
              CALL HM_GET_INTV('interfacepairsize'    ,NINTS,  IS_AVAILABLE, LSUBMODEL) 
              NSUB(NPROP_STACK)  = NLAM                                                       
              NISUB(NPROP_STACK) = NINTS                                                      
              DO IS = 1,NLAM                                                                  
                CALL HM_GET_INT_ARRAY_2INDEXES('plyidlistmax',NP,IS,1,IS_AVAILABLE,LSUBMODEL) 
                NPLY(NPROP_STACK) = NPLY(NPROP_STACK) + NP                                    
              END DO                                                                          
            ELSE                                                                              
              CALL HM_GET_INTV('plyidlistmax'      ,NP,  IS_AVAILABLE, LSUBMODEL)             
              NPLY(NPROP_STACK) = NPLY(NPROP_STACK) + NP                                   
            END IF                                                                            
C          
 700      CONTINUE
      ENDIF
      
c-----------
C
C Table allocation for type17,51 and 52
C 
      IF(NPROP_STACK > 0) THEN
         ALLOCATE(STACK_INFO(NPROP_STACK))
         DO NUMS=1,NPROP_STACK
            NN  = NPLY(NUMS)
            NS  = NSUB(NUMS)
            NIS = NISUB(NUMS)
             NULLIFY(STACK_INFO(NUMS)%PID,STACK_INFO(NUMS)%MID,
     .                STACK_INFO(NUMS)%MID_IP,
     .                STACK_INFO(NUMS)%SUB,STACK_INFO(NUMS)%ISUB,
     .                STACK_INFO(NUMS)%THK,STACK_INFO(NUMS)%ANG,
     .                STACK_INFO(NUMS)%POS,STACK_INFO(NUMS)%DIR,
     .                STACK_INFO(NUMS)%THKLY,STACK_INFO(NUMS)%WEIGHT)
    
             IF(NN > 0 ) THEN
                 ALLOCATE(STACK_INFO(NUMS)%PID(NN),STACK_INFO(NUMS)%MID(NN),
     .                     STACK_INFO(NUMS)%MID_IP(NN),
     .                     STACK_INFO(NUMS)%THK(NN),STACK_INFO(NUMS)%ANG(NN),
     .                     STACK_INFO(NUMS)%POS(NN),STACK_INFO(NUMS)%DIR(NN),
     .                     STACK_INFO(NUMS)%THKLY(NN),STACK_INFO(NUMS)%WEIGHT(NN)) 
                 STACK_INFO(NUMS)%PID(1:NN)= 0     
                 STACK_INFO(NUMS)%MID(1:NN)= 0
                 STACK_INFO(NUMS)%MID_IP(1:NN)= 0
                 STACK_INFO(NUMS)%THK(1:NN)= ZERO
                 STACK_INFO(NUMS)%ANG(1:NN)= ZERO
                 STACK_INFO(NUMS)%POS(1:NN)= ZERO
                 STACK_INFO(NUMS)%THKLY(1:NN)= ONE   
                 STACK_INFO(NUMS)%WEIGHT(1:NN)= ONE 
             ENDIF
             IF(NS  > 0)THEN
               ALLOCATE(STACK_INFO(NUMS)%SUB(2*NS))
               STACK_INFO(NUMS)%SUB(1:2*NS)= 0  
             ENDIF  
             IF(NIS > 0)THEN
              ALLOCATE(STACK_INFO(NUMS)%ISUB(3*NIS))
              STACK_INFO(NUMS)%ISUB(1:3*NIS)= 0
             ENDIF 
          ENDDO 
         ELSE
            ALLOCATE(STACK_INFO(0))
         ENDIF ! NPROP_STACK       
      RETURN
      END
