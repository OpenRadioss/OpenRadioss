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
Chd|  INITVARS_AUTO                 source/elements/elbuf_init/initvars_auto.F
Chd|-- called by -----------
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE INITVARS_AUTO(
     .                         ELBUF_STR,MLAW_TAG ,PROP_TAG ,FAIL_TAG ,VISC_TAG ,
     .                         GMAT     ,GLAW     ,IGTYP    ,JMULT    ,IFAIL    , 
     .                         IEOS     ,MAXEOS   ,EOS_TAG  ,ISNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tablen_c.inc"
#include      "inter22.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGTYP,GMAT,GLAW,JMULT,IFAIL,IEOS,MAXEOS,ISNOD
      TYPE(ELBUF_STRUCT_),TARGET  :: ELBUF_STR
      TYPE(MLAW_TAG_)    ,TARGET ,DIMENSION(NUMMAT)     :: MLAW_TAG
      TYPE(EOS_TAG_)     ,TARGET ,DIMENSION(0:MAXEOS)   :: EOS_TAG
      TYPE(PROP_TAG_)    ,TARGET ,DIMENSION(0:MAXPROP)  :: PROP_TAG
      TYPE(FAIL_TAG_)    ,TARGET ,DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE(VISC_TAG_)    ,TARGET ,DIMENSION(0:MAXVISC ) :: VISC_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILAY,NLAY,NPG,NPTT,NPTR,NPTS,IL,IR,IS,IT
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(MLAW_TAG_)    , POINTER :: MTAG
      TYPE(EOS_TAG_)     , POINTER :: ETAG
      TYPE(PROP_TAG_)    , POINTER :: PTAG
      TYPE(FAIL_TAG_)    , POINTER :: FTAG 
C-----------------------------------------------------------------------
c     GMAT - globat material number
c     GLAW - globat material law model
C=======================================================================

      
      NULLIFY(ETAG)
      
      GBUF => ELBUF_STR%GBUF
      PTAG => PROP_TAG(IGTYP)     
      IF(IEOS>0)ETAG => EOS_TAG(IEOS)
      NLAY =  ELBUF_STR%NLAY
      NPTT =  ELBUF_STR%NPTT
      NPG  =  ELBUF_STR%NPTR*ELBUF_STR%NPTS
      
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
c---------------------
c     global material variables                                            
c---------------------
      IF (JMULT > 0) THEN
        IF (MLAW_TAG(GMAT)%L_FRAC > 0) THEN                                   
          DO ILAY = 1,NLAY                                            
            ELBUF_STR%BUFLY(ILAY)%L_FRAC = MLAW_TAG(GMAT)%L_FRAC             
          ENDDO                                                     
        ENDIF                                                       
        DO ILAY = 1,JMULT                                             
          BUFLY => ELBUF_STR%BUFLY(ILAY)
          IF (GLAW == 151) THEN
            MTAG  => MLAW_TAG(GMAT)
          ELSE
            MTAG  => MLAW_TAG(BUFLY%IMAT)
          ENDIF
          IF (GBUF%G_GAMA   == 0) GBUF%G_GAMA   = MTAG%G_GAMA                 
          IF (GBUF%G_EINT   == 0) GBUF%G_EINT   = MTAG%G_EINT              
          IF (GBUF%G_EINS   == 0) GBUF%G_EINS   = MTAG%G_EINS              
          IF (GBUF%G_RHO    == 0) GBUF%G_RHO    = MTAG%G_RHO               
          IF (GBUF%G_DELTAX == 0) GBUF%G_DELTAX = MTAG%G_DELTAX            
          IF (GBUF%G_EPSD   == 0) GBUF%G_EPSD   = MTAG%G_EPSD              
          IF (GBUF%G_EPSQ   == 0) GBUF%G_EPSQ   = MTAG%G_EPSQ              	  
          IF (GBUF%G_PLA    == 0) GBUF%G_PLA    = MTAG%G_PLA               
          IF (GBUF%G_RK     == 0) GBUF%G_RK     = MTAG%G_RK                
          IF (GBUF%G_RE     == 0) GBUF%G_RE     = MTAG%G_RE                
          IF (GBUF%G_TB     == 0) GBUF%G_TB     = MTAG%G_TB                
          IF (GBUF%G_TEMP   == 0) GBUF%G_TEMP   = MTAG%G_TEMP              
          IF (GBUF%G_BFRAC  == 0) GBUF%G_BFRAC  = MTAG%G_BFRAC             
          IF (GBUF%G_SIGI   == 0) GBUF%G_SIGI   = MTAG%G_SIGI             
          IF (GBUF%G_IERR   == 0) GBUF%G_IERR   = MTAG%G_IERR             
          IF (GBUF%G_DAMDL  == 0) GBUF%G_DAMDL  = MTAG%G_DAMDL            
          IF (GBUF%G_SEQ    == 0) GBUF%G_SEQ    = MTAG%G_SEQ
          IF (GBUF%G_ABURN  == 0) GBUF%G_ABURN  = MTAG%G_ABURN 
          IF (GBUF%G_MU     == 0) GBUF%G_MU     = MTAG%G_MU
          IF (GBUF%G_DMG    == 0) GBUF%G_DMG    = MTAG%G_DMG
          IF (GBUF%G_MAXFRAC== 0) GBUF%G_MAXFRAC= MTAG%G_MAXFRAC    
          IF (GBUF%G_MAXEPS == 0) GBUF%G_MAXEPS = MTAG%G_MAXEPS
          IF (GBUF%G_PLANL  == 0) GBUF%G_PLANL  = MTAG%G_PLANL
          IF (GBUF%G_EPSDNL == 0) GBUF%G_EPSDNL = MTAG%G_EPSDNL
          IF (GBUF%G_TSAIWU == 0) GBUF%G_TSAIWU = MTAG%G_TSAIWU            
          BUFLY%NVAR_MAT= MTAG%NUVAR
          BUFLY%NVARTMP = MTAG%NVARTMP
          IF(BUFLY%NVAR_EOS==0 .AND. IEOS>0 )BUFLY%NVAR_EOS=ETAG%NVAR
        ENDDO                                      
      ELSE
        IF (NLAY > 1) THEN
          DO ILAY = 1,NLAY  
            MTAG  => MLAW_TAG(ELBUF_STR%BUFLY(ILAY)%IMAT)
            IF (GBUF%G_GAMA   == 0) GBUF%G_GAMA   = MTAG%G_GAMA                 
            IF (GBUF%G_EINT   == 0) GBUF%G_EINT   = MTAG%G_EINT                
            IF (GBUF%G_EINS   == 0) GBUF%G_EINS   = MTAG%G_EINS                
            IF (GBUF%G_RHO    == 0) GBUF%G_RHO    = MTAG%G_RHO                 
            IF (GBUF%G_DELTAX == 0) GBUF%G_DELTAX = MTAG%G_DELTAX              
            IF (GBUF%G_EPSD   == 0) GBUF%G_EPSD   = MTAG%G_EPSD                
            IF (GBUF%G_EPSQ   == 0) GBUF%G_EPSQ   = MTAG%G_EPSQ              	
            IF (GBUF%G_PLA    == 0) GBUF%G_PLA    = MTAG%G_PLA                 
            IF (GBUF%G_RK     == 0) GBUF%G_RK     = MTAG%G_RK                  
            IF (GBUF%G_RE     == 0) GBUF%G_RE     = MTAG%G_RE                  
            IF (GBUF%G_TB     == 0) GBUF%G_TB     = MTAG%G_TB                  
            IF (GBUF%G_TEMP   == 0) GBUF%G_TEMP   = MTAG%G_TEMP                
            IF (GBUF%G_BFRAC  == 0) GBUF%G_BFRAC  = MTAG%G_BFRAC               
            IF (GBUF%G_SIGI   == 0) GBUF%G_SIGI   = MTAG%G_SIGI             
            IF (GBUF%G_IERR   == 0) GBUF%G_IERR   = MTAG%G_IERR             
            IF (GBUF%G_DAMDL  == 0) GBUF%G_DAMDL  = MTAG%G_DAMDL            
            IF (GBUF%G_SEQ    == 0) GBUF%G_SEQ    = MTAG%G_SEQ
            IF (GBUF%G_ABURN  == 0) GBUF%G_ABURN  = MTAG%G_ABURN  
            IF (GBUF%G_MU     == 0) GBUF%G_MU     = MTAG%G_MU 
            IF (GBUF%G_DMG    == 0) GBUF%G_DMG    = MTAG%G_DMG
            IF (GBUF%G_DAMDL  == 0) GBUF%G_DAMDL  = MTAG%G_DAMDL 
            IF (GBUF%G_IERR   == 0) GBUF%G_IERR   = MTAG%G_IERR
            IF (GBUF%G_MAXFRAC== 0) GBUF%G_MAXFRAC= MTAG%G_MAXFRAC    
            IF (GBUF%G_MAXEPS == 0) GBUF%G_MAXEPS = MTAG%G_MAXEPS    
            IF (GBUF%G_PLANL  == 0) GBUF%G_PLANL  = MTAG%G_PLANL
            IF (GBUF%G_EPSDNL == 0) GBUF%G_EPSDNL = MTAG%G_EPSDNL  
            IF (GBUF%G_TSAIWU == 0) GBUF%G_TSAIWU = MTAG%G_TSAIWU      
          ENDDO 
        ELSE IF (GMAT > 0) THEN
          MTAG  => MLAW_TAG(GMAT)
          IF (GBUF%G_GAMA   == 0) GBUF%G_GAMA   = MTAG%G_GAMA                   
          IF (GBUF%G_EINT   == 0) GBUF%G_EINT   = MTAG%G_EINT                
          IF (GBUF%G_EINS   == 0) GBUF%G_EINS   = MTAG%G_EINS                
          IF (GBUF%G_RHO    == 0) GBUF%G_RHO    = MTAG%G_RHO                 
          IF (GBUF%G_DELTAX == 0) GBUF%G_DELTAX = MTAG%G_DELTAX              
          IF (GBUF%G_EPSD   == 0) GBUF%G_EPSD   = MTAG%G_EPSD                
          IF (GBUF%G_EPSQ   == 0) GBUF%G_EPSQ   = MTAG%G_EPSQ              	
          IF (GBUF%G_PLA    == 0) GBUF%G_PLA    = MTAG%G_PLA                 
          IF (GBUF%G_RK     == 0) GBUF%G_RK     = MTAG%G_RK                  
          IF (GBUF%G_RE     == 0) GBUF%G_RE     = MTAG%G_RE                  
          IF (GBUF%G_TB     == 0) GBUF%G_TB     = MTAG%G_TB                  
          IF (GBUF%G_TEMP   == 0) GBUF%G_TEMP   = MTAG%G_TEMP                
          IF (GBUF%G_BFRAC  == 0) GBUF%G_BFRAC  = MTAG%G_BFRAC               
          IF (GBUF%G_SIGI   == 0) GBUF%G_SIGI   = MTAG%G_SIGI             
          IF (GBUF%G_IERR   == 0) GBUF%G_IERR   = MTAG%G_IERR             
          IF (GBUF%G_DAMDL  == 0) GBUF%G_DAMDL  = MTAG%G_DAMDL            
          IF (GBUF%G_SEQ    == 0) GBUF%G_SEQ    = MTAG%G_SEQ
          IF (GBUF%G_ABURN  == 0) GBUF%G_ABURN  = MTAG%G_ABURN  
          IF (GBUF%G_MU     == 0) GBUF%G_MU     = MTAG%G_MU 
          IF (GBUF%G_PLANL  == 0) GBUF%G_PLANL  = MTAG%G_PLANL
          IF (GBUF%G_EPSDNL == 0) GBUF%G_EPSDNL = MTAG%G_EPSDNL
          IF (GBUF%G_TSAIWU == 0) GBUF%G_TSAIWU = MTAG%G_TSAIWU
          IF (GBUF%G_MAXFRAC== 0) GBUF%G_MAXFRAC= MTAG%G_MAXFRAC   
          IF (GBUF%G_MAXEPS == 0) GBUF%G_MAXEPS = MTAG%G_MAXEPS               
          IF (GBUF%G_DMG    == 0) GBUF%G_DMG    = MTAG%G_DMG
          IF (GBUF%G_NUVAR  == 0) GBUF%G_NUVAR  = MTAG%G_NUVAR 
c
          IF (IGTYP == 2 .or. IGTYP == 3) THEN   ! truss / beam with Material
            IF (GBUF%G_NUVAR  == 0) GBUF%G_NUVAR  = MTAG%NUVAR 
          ENDIF
        ENDIF    ! GMAT > 0     
      ENDIF                                                         
C                                                
      
      IF (INT22 > 0) GBUF%G_TAG22 = 1  
      GBUF%G_DT  = 1

c
c     equation of state (eos)                                           
c     
      IF (IEOS > 0)THEN
        IF (GBUF%G_MU    == 0) GBUF%G_MU    = ETAG%G_MU   
        IF (GBUF%G_TB    == 0) GBUF%G_TB    = ETAG%G_TB
        IF (GBUF%G_ABURN == 0) GBUF%G_ABURN = ETAG%G_ABURN  
        IF (GBUF%G_BFRAC == 0) GBUF%G_BFRAC = ETAG%G_BFRAC
        
        DO IL = 1, NLAY                                                    
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN                     
            NPTT = ELBUF_STR%BUFLY(IL)%NPTT                      
          ELSE                                                       
            NPTT = ELBUF_STR%NPTT                                
          ENDIF                                                      
          DO IR = 1,NPTR                                                  
            DO IS = 1,NPTS                                                
              DO IT = 1,NPTT                                         
                IF(ELBUF_STR%BUFLY(IL)%NVAR_EOS == 0)THEN
                  ELBUF_STR%BUFLY(IL)%NVAR_EOS = ETAG%NVAR                                          
                ENDIF                                              
              ENDDO                                                           
            ENDDO                                                            
          ENDDO                                                           
        ENDDO                                                        

        
      ENDIF

c
c     global property variables                                            
c
      IF (GBUF%G_OFF    == 0) GBUF%G_OFF    = PTAG%G_OFF                     
      IF (IFAIL > 0) GBUF%G_NOFF = 1
      IF (GBUF%G_NOFF   == 0) GBUF%G_NOFF   = PTAG%G_NOFF
      GBUF%G_GAMA = MAX(GBUF%G_GAMA, PTAG%G_GAMA)                    
      GBUF%G_EINT = MAX(GBUF%G_EINT, PTAG%G_EINT)                  
      IF (GBUF%G_SMSTR  == 0) GBUF%G_SMSTR  = PTAG%G_SMSTR                     
      IF (GBUF%G_HOURG  == 0) GBUF%G_HOURG  = PTAG%G_HOURG                     
      IF (GBUF%G_FOR    == 0) GBUF%G_FOR    = PTAG%G_FOR                    
      IF (GBUF%G_MOM    == 0) GBUF%G_MOM    = PTAG%G_MOM                    
      IF (GBUF%G_SIG    == 0) GBUF%G_SIG    = PTAG%G_SIG   
      IF (GBUF%G_STRA   == 0) GBUF%G_STRA   = PTAG%G_STRA     
      IF (GBUF%G_THK    == 0) GBUF%G_THK    = PTAG%G_THK                    
      IF (GBUF%G_VOL    == 0) GBUF%G_VOL    = PTAG%G_VOL                     
      IF (GBUF%G_QVIS   == 0) GBUF%G_QVIS   = PTAG%G_QVIS      
      IF (GBUF%G_FORPG  == 0) GBUF%G_FORPG  = PTAG%G_FORPG*NPG      
      IF (GBUF%G_MOMPG  == 0) GBUF%G_MOMPG  = PTAG%G_MOMPG*NPG 
C
      IF (GBUF%G_FORPGPINCH  == 0) GBUF%G_FORPGPINCH  = PTAG%G_FORPGPINCH*NPG      
      IF (GBUF%G_MOMPGPINCH  == 0) GBUF%G_MOMPGPINCH  = PTAG%G_MOMPGPINCH*NPG 
      IF (GBUF%G_EPGPINCHXZ  == 0) GBUF%G_EPGPINCHXZ  = PTAG%G_EPGPINCHXZ*NPG
      IF (GBUF%G_EPGPINCHYZ  == 0) GBUF%G_EPGPINCHYZ  = PTAG%G_EPGPINCHYZ*NPG
      IF (GBUF%G_EPGPINCHZZ  == 0) GBUF%G_EPGPINCHZZ  = PTAG%G_EPGPINCHZZ*NPG
C      
      IF (GBUF%G_STRPG  == 0) GBUF%G_STRPG  = PTAG%G_STRPG*NPG   
      IF (GBUF%G_FORTH  == 0) GBUF%G_FORTH  = PTAG%G_FORTH
      IF (GBUF%G_EINTTH == 0) GBUF%G_EINTTH = PTAG%G_EINTTH  
      IF (GBUF%G_FILL   == 0) GBUF%G_FILL   = PTAG%G_FILL
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      IF (GBUF%G_AREA        == 0) GBUF%G_AREA         = PTAG%G_AREA
      IF (GBUF%G_SKEW        == 0) GBUF%G_SKEW         = PTAG%G_SKEW
      IF (GBUF%G_LENGTH      == 0) GBUF%G_LENGTH       = PTAG%G_LENGTH
      IF (GBUF%G_TOTDEPL     == 0) GBUF%G_TOTDEPL      = PTAG%G_TOTDEPL
      IF (GBUF%G_TOTROT      == 0) GBUF%G_TOTROT       = PTAG%G_TOTROT
      IF (GBUF%G_FOREP       == 0) GBUF%G_FOREP        = PTAG%G_FOREP
      IF (GBUF%G_MOMEP       == 0) GBUF%G_MOMEP        = PTAG%G_MOMEP
      IF (GBUF%G_DEP_IN_TENS == 0) GBUF%G_DEP_IN_TENS  = PTAG%G_DEP_IN_TENS
      IF (GBUF%G_DEP_IN_COMP == 0) GBUF%G_DEP_IN_COMP  = PTAG%G_DEP_IN_COMP
      IF (GBUF%G_ROT_IN_TENS == 0) GBUF%G_ROT_IN_TENS  = PTAG%G_ROT_IN_TENS
      IF (GBUF%G_ROT_IN_COMP == 0) GBUF%G_ROT_IN_COMP  = PTAG%G_ROT_IN_COMP
      IF (GBUF%G_POSX        == 0) GBUF%G_POSX         = PTAG%G_POSX
      IF (GBUF%G_POSY        == 0) GBUF%G_POSY         = PTAG%G_POSY
      IF (GBUF%G_POSZ        == 0) GBUF%G_POSZ         = PTAG%G_POSZ
      IF (GBUF%G_POSXX       == 0) GBUF%G_POSXX        = PTAG%G_POSXX
      IF (GBUF%G_POSYY       == 0) GBUF%G_POSYY        = PTAG%G_POSYY
      IF (GBUF%G_POSZZ       == 0) GBUF%G_POSZZ        = PTAG%G_POSZZ
      IF (GBUF%G_YIELD       == 0) GBUF%G_YIELD        = PTAG%G_YIELD
      IF (GBUF%G_LENGTH_ERR  == 0) GBUF%G_LENGTH_ERR   = PTAG%G_LENGTH_ERR
      IF (GBUF%G_DV          == 0) GBUF%G_DV           = PTAG%G_DV
      IF (GBUF%G_DFS         == 0) GBUF%G_DFS          = PTAG%G_DFS
      IF (GBUF%G_SKEW_ERR    == 0) GBUF%G_SKEW_ERR     = PTAG%G_SKEW_ERR
      IF (GBUF%G_E6          == 0) GBUF%G_E6           = PTAG%G_E6
      IF (GBUF%G_RUPTCRIT    == 0) GBUF%G_RUPTCRIT     = PTAG%G_RUPTCRIT
      IF (GBUF%G_MASS        == 0) GBUF%G_MASS         = PTAG%G_MASS
      IF (GBUF%G_V_REPCVT    == 0) GBUF%G_V_REPCVT     = PTAG%G_V_REPCVT
      IF (GBUF%G_VR_REPCVT   == 0) GBUF%G_VR_REPCVT    = PTAG%G_VR_REPCVT
      IF (GBUF%G_NUVAR       == 0) GBUF%G_NUVAR        = PTAG%G_NUVAR
      IF (GBUF%G_NUVARN      == 0) GBUF%G_NUVARN       = PTAG%G_NUVARN
      IF (GBUF%G_DEFINI      == 0) GBUF%G_DEFINI       = PTAG%G_DEFINI
      IF (GBUF%G_FORINI      == 0) GBUF%G_FORINI       = PTAG%G_FORINI
      IF (GBUF%G_INIFRIC     == 0) GBUF%G_INIFRIC      = PTAG%G_INIFRIC
      IF (GBUF%G_SKEW_ID     == 0) GBUF%G_SKEW_ID      = PTAG%G_SKEW_ID
C
      IF (IGTYP == 23 .AND. GMAT > 0) THEN   ! spring with Material
        MTAG  => MLAW_TAG(GMAT)
        IF (GBUF%G_TOTDEPL     == 0) GBUF%G_TOTDEPL      = MTAG%G_TOTDEPL
        IF (GBUF%G_TOTROT      == 0) GBUF%G_TOTROT       = MTAG%G_TOTROT
        IF (GBUF%G_DEP_IN_TENS == 0) GBUF%G_DEP_IN_TENS  = MTAG%G_DEP_IN_TENS
        IF (GBUF%G_DEP_IN_COMP == 0) GBUF%G_DEP_IN_COMP  = MTAG%G_DEP_IN_COMP
        IF (GBUF%G_ROT_IN_TENS == 0) GBUF%G_ROT_IN_TENS  = MTAG%G_ROT_IN_TENS
        IF (GBUF%G_ROT_IN_COMP == 0) GBUF%G_ROT_IN_COMP  = MTAG%G_ROT_IN_COMP
        IF (GBUF%G_POSX        == 0) GBUF%G_POSX         = MTAG%G_POSX
        IF (GBUF%G_POSY        == 0) GBUF%G_POSY         = MTAG%G_POSY
        IF (GBUF%G_POSZ        == 0) GBUF%G_POSZ         = MTAG%G_POSZ
        IF (GBUF%G_POSXX       == 0) GBUF%G_POSXX        = MTAG%G_POSXX
        IF (GBUF%G_POSYY       == 0) GBUF%G_POSYY        = MTAG%G_POSYY
        IF (GBUF%G_POSZZ       == 0) GBUF%G_POSZZ        = MTAG%G_POSZZ
        IF (GBUF%G_YIELD       == 0) GBUF%G_YIELD        = MTAG%G_YIELD
        IF (GBUF%G_RUPTCRIT    == 0) GBUF%G_RUPTCRIT     = MTAG%G_RUPTCRIT
        IF (GBUF%G_MASS        == 0) GBUF%G_MASS         = MTAG%G_MASS
        IF (GBUF%G_SKEW_ID     == 0) GBUF%G_SKEW_ID      = MTAG%G_SKEW_ID
      ENDIF
C
      IF ((IGTYP == 23).AND.(GLAW == 114)) THEN   ! 1D seatbelt material
        MTAG  => MLAW_TAG(GMAT)
        IF (GBUF%G_DFS == 0)         GBUF%G_DFS          = MTAG%G_DFS
        IF (GBUF%G_SLIPRING_ID == 0) GBUF%G_SLIPRING_ID  = MTAG%G_SLIPRING_ID
        IF (GBUF%G_SLIPRING_FRAM_ID == 0) GBUF%G_SLIPRING_FRAM_ID  = MTAG%G_SLIPRING_FRAM_ID
        IF (GBUF%G_SLIPRING_STRAND == 0) GBUF%G_SLIPRING_STRAND  = MTAG%G_SLIPRING_STRAND
        IF (GBUF%G_RETRACTOR_ID== 0) GBUF%G_RETRACTOR_ID = MTAG%G_RETRACTOR_ID
        IF (GBUF%G_RINGSLIP    == 0) GBUF%G_RINGSLIP     = MTAG%G_RINGSLIP
        IF (GBUF%G_ADD_NODE    == 0) GBUF%G_ADD_NODE     = MTAG%G_ADD_NODE
        IF (GBUF%G_UPDATE      == 0) GBUF%G_UPDATE       = MTAG%G_UPDATE
        IF (GBUF%G_FRAM_FACTOR == 0) GBUF%G_FRAM_FACTOR  = MTAG%G_FRAM_FACTOR
        IF (GBUF%G_INTVAR      == 0) GBUF%G_INTVAR       = MTAG%G_INTVAR
      ENDIF
C
      IF ((IGTYP == 9).AND.(GLAW == 119)) THEN   ! 2D seatbelt material
        MTAG  => MLAW_TAG(GMAT)
        IF (GBUF%G_SLIPRING_ID == 0) GBUF%G_SLIPRING_ID  = MTAG%G_SLIPRING_ID
        IF (GBUF%G_SLIPRING_FRAM_ID == 0) GBUF%G_SLIPRING_FRAM_ID  = MTAG%G_SLIPRING_FRAM_ID
        IF (GBUF%G_INTVAR    == 0) GBUF%G_INTVAR     = MTAG%G_INTVAR
        IF (GBUF%G_ADD_NODE    == 0) GBUF%G_ADD_NODE     = MTAG%G_ADD_NODE
        IF (GBUF%G_UPDATE    == 0) GBUF%G_UPDATE     = MTAG%G_UPDATE
        IF (GBUF%G_POSX    == 0) GBUF%G_POSX     = MTAG%G_POSX
        IF (GBUF%G_POSY    == 0) GBUF%G_POSY     = MTAG%G_POSY
      ENDIF
C
      IF(ISNOD == 10 .AND. IDT1TET10 > 1)THEN
        GBUF%G_DT_PITER=30
      END IF
c
c     local variables  : defined per layer                             
c      
      DO ILAY = 1,NLAY                                              
        BUFLY => ELBUF_STR%BUFLY(ILAY)
        IF (JMULT > 0 .and. GLAW == 151) THEN
          MTAG  => MLAW_TAG(GMAT)
        ELSE
          MTAG  => MLAW_TAG(BUFLY%IMAT)
        ENDIF 
        ETAG  => EOS_TAG(BUFLY%IEOS)
        IF(IEOS > 0)THEN
          IF (BUFLY%L_MU    == 0) BUFLY%L_MU    = ETAG%L_MU   
          IF (BUFLY%L_TB    == 0) BUFLY%L_TB    = ETAG%L_TB
          IF (BUFLY%L_ABURN == 0) BUFLY%L_ABURN = ETAG%L_ABURN  
          IF (BUFLY%L_BFRAC == 0) BUFLY%L_BFRAC = ETAG%L_BFRAC
        ENDIF
        
        IF (IGTYP == 51 .OR. IGTYP == 52) NPTT = BUFLY%NPTT
c       local material variables                               
        IF (BUFLY%L_GAMA   == 0) BUFLY%L_GAMA   = MTAG%L_GAMA     
        IF (BUFLY%L_STRA   == 0) BUFLY%L_STRA   = MTAG%L_STRA   
        IF (BUFLY%L_BFRAC  == 0) BUFLY%L_BFRAC  = MTAG%L_BFRAC     
        IF (BUFLY%L_EINT   == 0) BUFLY%L_EINT   = MTAG%L_EINT                
        IF (BUFLY%L_EINS   == 0) BUFLY%L_EINS   = MTAG%L_EINS                
        IF (BUFLY%L_RHO    == 0) BUFLY%L_RHO    = MTAG%L_RHO                 
        IF (BUFLY%L_DP_DRHO== 0) BUFLY%L_DP_DRHO= MTAG%L_DP_DRHO             
        IF (BUFLY%L_DELTAX == 0) BUFLY%L_DELTAX = MTAG%L_DELTAX              
        IF (BUFLY%L_PLA    == 0) BUFLY%L_PLA    = MTAG%L_PLA                 
        IF (BUFLY%L_TB     == 0) BUFLY%L_TB     = MTAG%L_TB                  
        IF (BUFLY%L_RK     == 0) BUFLY%L_RK     = MTAG%L_RK                  
        IF (BUFLY%L_RE     == 0) BUFLY%L_RE     = MTAG%L_RE                  
        IF (BUFLY%L_EPSA   == 0) BUFLY%L_EPSA   = MTAG%L_EPSA                
        IF (BUFLY%L_EPSD   == 0) BUFLY%L_EPSD   = MTAG%L_EPSD                
        IF (BUFLY%L_EPSQ   == 0) BUFLY%L_EPSQ   = MTAG%L_EPSQ                
        IF (BUFLY%L_EPSF   == 0) BUFLY%L_EPSF   = MTAG%L_EPSF                
        IF (BUFLY%L_SIGL   == 0) BUFLY%L_SIGL   = MTAG%L_SIGL                
        IF (MTAG%G_GAMA > 0 .AND. BUFLY%L_SIGL == 0) BUFLY%L_SIGL = 6        
        IF (BUFLY%L_SIGV   == 0) BUFLY%L_SIGV   = MTAG%L_SIGV                
        IF (BUFLY%L_SIGA   == 0) BUFLY%L_SIGA   = MTAG%L_SIGA                
        IF (BUFLY%L_SIGB   == 0) BUFLY%L_SIGB   = MTAG%L_SIGB                
        IF (BUFLY%L_SIGC   == 0) BUFLY%L_SIGC   = MTAG%L_SIGC                
        IF (BUFLY%L_SIGD   == 0) BUFLY%L_SIGD   = MTAG%L_SIGD                
        IF (BUFLY%L_SIGF   == 0) BUFLY%L_SIGF   = MTAG%L_SIGF                
        IF (BUFLY%L_TEMP   == 0) BUFLY%L_TEMP   = MTAG%L_TEMP                
        IF (BUFLY%L_VK     == 0) BUFLY%L_VK     = MTAG%L_VK                  
        IF (BUFLY%L_SF     == 0) BUFLY%L_SF     = MTAG%L_SF                  
        IF (BUFLY%L_ROB    == 0) BUFLY%L_ROB    = MTAG%L_ROB                 
        IF (BUFLY%L_DAM    == 0) BUFLY%L_DAM    = MTAG%L_DAM                 
        IF (BUFLY%L_DSUM   == 0) BUFLY%L_DSUM   = MTAG%L_DSUM                
        IF (BUFLY%L_DGLO   == 0) BUFLY%L_DGLO   = MTAG%L_DGLO                
        IF (BUFLY%L_CRAK   == 0) BUFLY%L_CRAK   = MTAG%L_CRAK                
        IF (BUFLY%L_ANG    == 0) BUFLY%L_ANG    = MTAG%L_ANG                 
        IF (BUFLY%L_EPE    == 0) BUFLY%L_EPE    = MTAG%L_EPE                 
        IF (BUFLY%L_EPC    == 0) BUFLY%L_EPC    = MTAG%L_EPC                 
        IF (BUFLY%L_XST    == 0) BUFLY%L_XST    = MTAG%L_XST                 
        IF (BUFLY%L_SSP    == 0) BUFLY%L_SSP    = MTAG%L_SSP                 
        IF (BUFLY%L_Z      == 0) BUFLY%L_Z      = MTAG%L_Z                   
        IF (BUFLY%L_FRAC   == 0) BUFLY%L_FRAC   = MTAG%L_FRAC           
        IF (BUFLY%L_DMG    == 0) BUFLY%L_DMG    = MTAG%L_DMG                 
        IF (BUFLY%LY_DIRA  == 0) BUFLY%LY_DIRA  = MTAG%LY_DIRA
        IF (BUFLY%L_SEQ    == 0) BUFLY%L_SEQ    = MTAG%L_SEQ
        IF (BUFLY%L_FAC_YLD== 0) BUFLY%L_FAC_YLD= MTAG%L_FAC_YLD 
        IF (BUFLY%L_ABURN  == 0) BUFLY%L_ABURN  = MTAG%L_ABURN 
        IF (BUFLY%L_MU     == 0) BUFLY%L_MU     = MTAG%L_MU   
        IF (BUFLY%L_PLANL  == 0) BUFLY%L_PLANL  = MTAG%L_PLANL
        IF (BUFLY%L_EPSDNL == 0) BUFLY%L_EPSDNL = MTAG%L_EPSDNL      
        IF (BUFLY%L_DMGSCL == 0) BUFLY%L_DMGSCL = MTAG%L_DMGSCL  
        IF (BUFLY%L_TSAIWU == 0) BUFLY%L_TSAIWU = MTAG%L_TSAIWU  
c       layer property variables
        IF (BUFLY%LY_DMG  == 0) BUFLY%LY_DMG    = PTAG%LY_DMG
        IF (BUFLY%LY_GAMA == 0) BUFLY%LY_GAMA   = PTAG%LY_GAMA               
        IF (BUFLY%LY_DIRA == 0) BUFLY%LY_DIRA   = PTAG%LY_DIRA               
        IF (BUFLY%LY_DIRB == 0) BUFLY%LY_DIRB   = PTAG%LY_DIRB  
        IF (BUFLY%LY_PLAPT== 0 .and. BUFLY%L_PLA > 0) BUFLY%LY_PLAPT  = PTAG%LY_PLAPT
        IF (BUFLY%LY_SIGPT == 0) BUFLY%LY_SIGPT = PTAG%LY_SIGPT
        IF (BUFLY%LY_HOURG == 0) BUFLY%LY_HOURG = PTAG%LY_HOURG
c       local property variables
        IF (BUFLY%L_OFF    == 0) BUFLY%L_OFF    = PTAG%L_OFF                 
        BUFLY%L_EINT = MAX(BUFLY%L_EINT, PTAG%L_EINT)                
        IF (BUFLY%L_GAMA   == 0) BUFLY%L_GAMA   = PTAG%L_GAMA                 
        IF (BUFLY%L_STRA   == 0) BUFLY%L_STRA   = PTAG%L_STRA                
        IF (BUFLY%L_FOR    == 0) BUFLY%L_FOR    = PTAG%L_FOR                
        IF (BUFLY%L_MOM    == 0) BUFLY%L_MOM    = PTAG%L_MOM
        IF (BUFLY%L_SIGL   == 0) BUFLY%L_SIGL   = PTAG%L_SIGL 
        IF (BUFLY%L_SIG    == 0) BUFLY%L_SIG    = PTAG%L_SIG                 
        IF (BUFLY%L_SIGPLY == 0) BUFLY%L_SIGPLY = PTAG%L_SIGPLY                
        IF (BUFLY%L_THK    == 0) BUFLY%L_THK    = PTAG%L_THK                
        IF (BUFLY%L_VOL    == 0) BUFLY%L_VOL    = PTAG%L_VOL
        IF (BUFLY%L_QVIS   == 0) BUFLY%L_QVIS   = PTAG%L_QVIS
        IF (BUFLY%L_FORTH  == 0) BUFLY%L_FORTH  = PTAG%L_FORTH
        IF (BUFLY%L_EINTTH == 0) BUFLY%L_EINTTH = PTAG%L_EINTTH 
        IF (BUFLY%L_DMG    == 0) BUFLY%L_DMG    = PTAG%LY_DMG  
      ENDDO 
C-----------------------------------------------
      RETURN
      END SUBROUTINE INITVARS_AUTO
