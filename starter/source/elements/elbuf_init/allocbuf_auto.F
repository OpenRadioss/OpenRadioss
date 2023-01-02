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
Chd|  ALLOCBUF_AUTO                 source/elements/elbuf_init/allocbuf_auto.F
Chd|-- called by -----------
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALLOCBUF_AUTO(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                         NINTLAY,NEL,NPT,NG,NGROUP,ITY,
     .                         IGTYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                           
C-----------------------------------------------
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAY,NINTLAY,NPTR,NPTS,NPTT,NEL,NPT,NG,NGROUP,ITY,IGTYP
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IL,IR,IS,IT,Stat,NPG,ILAYER,IPT,
     .        LEN1,LEN2,LEN_PLAPT,LEN_SIGPT,NPTTOT
      TYPE(BUF_MAT_)     , POINTER :: MATBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(BUF_VISC_)    , POINTER :: VISCBUF
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE(FAIL_LOC_)    , POINTER :: FLOC
C=======================================================================
      Stat = 0
      IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101) THEN
        NPG = NPTR*NPTS*NPTT
        NPTTOT = NPT
      ELSEIF (ITY == 3 .OR. ITY == 7) THEN
        NPG = NPTR*NPTS
        NPTTOT  = 0
        DO I=1,NLAY
          NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(I)%NPTT
        ENDDO
        IF (NPT == 0) NPTTOT = NPT
      ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6  .OR. ITY == 100)THEN
        NPG = 0
        NPTTOT = 0
      ENDIF
c-------------------------------------------------
c     layer buffer allocation
c-------------------------------------------------
      DO I = 1, NLAY                                                        
        BUFLY => ELBUF_TAB(NG)%BUFLY(I)                                     
c
        MY_ALLOCATE(BUFLY%DMG,NEL*BUFLY%LY_DMG)                          
          BUFLY%DMG = ZERO                                                  
        MY_ALLOCATE(BUFLY%GAMA,NEL*BUFLY%LY_GAMA)                        
          BUFLY%GAMA = ZERO                     
        MY_ALLOCATE(BUFLY%DIRA,NEL*BUFLY%LY_DIRA)                        
          BUFLY%DIRA = ZERO                                                 
        MY_ALLOCATE(BUFLY%DIRB,NEL*BUFLY%LY_DIRB)                        
          BUFLY%DIRB = ZERO        
        MY_ALLOCATE(BUFLY%CRKDIR,NEL*BUFLY%LY_CRKDIR)                        
          BUFLY%CRKDIR = ZERO        
        MY_ALLOCATE(BUFLY%OFFPG,NEL*BUFLY%LY_OFFPG)                        
          BUFLY%OFFPG = 1
        MY_ALLOCATE(BUFLY%OFF,NEL*BUFLY%LY_OFF)                        
          BUFLY%OFF = 1
c
        IF (NPG > 1) THEN
          IF (NLAY > 1) THEN
            LEN_PLAPT = NEL*BUFLY%LY_PLAPT
            LEN_SIGPT = NEL*BUFLY%LY_SIGPT
          ELSE
            IF (IGTYP /= 51 .AND. IGTYP /= 52 ) THEN
              LEN_PLAPT = NEL*BUFLY%LY_PLAPT*NPT
              LEN_SIGPT = NEL*BUFLY%LY_SIGPT*NPT
            ELSE
              LEN_PLAPT = NEL*BUFLY%LY_PLAPT*BUFLY%NPTT
              LEN_SIGPT = NEL*BUFLY%LY_SIGPT*BUFLY%NPTT
            ENDIF
          ENDIF
          MY_ALLOCATE(BUFLY%PLAPT,LEN_PLAPT)                     
            BUFLY%PLAPT = ZERO                 
          MY_ALLOCATE(BUFLY%SIGPT,LEN_SIGPT)              
            BUFLY%SIGPT = ZERO 
        ELSEIF (NPG == 1) THEN
C
C      PLAPT & SIGPT will point to LBUF%PLA and LBUF%SIG after allocation
C
        ENDIF
C
        MY_ALLOCATE(BUFLY%HOURG,NEL*BUFLY%LY_HOURG)                          
          BUFLY%HOURG = ZERO
        MY_ALLOCATE(BUFLY%UELR,NEL*BUFLY%LY_UELR)                          
          BUFLY%UELR = ZERO
        MY_ALLOCATE(BUFLY%UELR1,NEL*BUFLY%LY_UELR1)                          
          BUFLY%UELR1 = ZERO
      ENDDO                                                                 
c---
c-------------------------------------------------
c     LAYER Local variables per integration point
c-------------------------------------------------
          DO I = 1, NLAY
           BUFLY => ELBUF_TAB(NG)%BUFLY(I)
           IF (IGTYP == 51 .OR. IGTYP == 52) THEN
             NPTT = BUFLY%NPTT
           ELSE
             NPTT = ELBUF_TAB(NG)%NPTT
           ENDIF
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT  
                LBUF => ELBUF_TAB(NG)%BUFLY(I)%LBUF(IR,IS,IT) 
c
              MY_ALLOCATE(LBUF%OFF,NEL*BUFLY%L_OFF)              
                  LBUF%OFF = ONE
              MY_ALLOCATE (LBUF%GAMA,NEL*BUFLY%L_GAMA)            
                  LBUF%GAMA = ZERO
              MY_ALLOCATE(LBUF%STRA,NEL*BUFLY%L_STRA)            
                  LBUF%STRA = ZERO
              MY_ALLOCATE(LBUF%FRAC,NEL*BUFLY%L_FRAC)            
                  LBUF%FRAC = ZERO
              MY_ALLOCATE(LBUF%BFRAC,NEL*BUFLY%L_BFRAC)                   
                  LBUF%BFRAC = ZERO
              MY_ALLOCATE(LBUF%EINT,NEL*BUFLY%L_EINT)                     
                  LBUF%EINT = ZERO
              MY_ALLOCATE(LBUF%EINS,NEL*BUFLY%L_EINS)                     
                  LBUF%EINS = ZERO
              MY_ALLOCATE(LBUF%RHO,NEL*BUFLY%L_RHO)              
                  LBUF%RHO  = ZERO
              MY_ALLOCATE(LBUF%DP_DRHO,NEL)                       
                  LBUF%DP_DRHO = ZERO
              MY_ALLOCATE(LBUF%QVIS,NEL*BUFLY%L_QVIS)             
                  LBUF%QVIS = ZERO
              MY_ALLOCATE(LBUF%DELTAX,NEL*BUFLY%L_DELTAX)         
                  LBUF%DELTAX = ZERO
              MY_ALLOCATE(LBUF%VOL,NEL*BUFLY%L_VOL)              
                  LBUF%VOL = ZERO
              MY_ALLOCATE(LBUF%EPSA,NEL*BUFLY%L_EPSA)            
                  LBUF%EPSA = ZERO
              MY_ALLOCATE(LBUF%EPSD,NEL*BUFLY%L_EPSD)            
                  LBUF%EPSD = ZERO
              MY_ALLOCATE(LBUF%EPSQ,NEL*BUFLY%L_EPSQ)            
                  LBUF%EPSQ = ZERO
              MY_ALLOCATE(LBUF%EPSF,NEL*BUFLY%L_EPSF)            
                  LBUF%EPSF = ZERO
              MY_ALLOCATE(LBUF%PLA,NEL*BUFLY%L_PLA)              
                  LBUF%PLA = ZERO
              MY_ALLOCATE(LBUF%TEMP,NEL*BUFLY%L_TEMP)            
                  LBUF%TEMP = ZERO
              MY_ALLOCATE(LBUF%TB,NEL*BUFLY%L_TB)                
                  LBUF%TB = ZERO
              MY_ALLOCATE(LBUF%RK,NEL*BUFLY%L_RK)                
                  LBUF%RK = ZERO
              MY_ALLOCATE(LBUF%RE,NEL*BUFLY%L_RE)                
                  LBUF%RE = ZERO
              MY_ALLOCATE(LBUF%VK,NEL*BUFLY%L_VK)                
                  LBUF%VK = ZERO
              MY_ALLOCATE(LBUF%SF,NEL*BUFLY%L_SF)                
                  LBUF%SF = ZERO
              MY_ALLOCATE(LBUF%ROB,NEL*BUFLY%L_ROB)              
                  LBUF%ROB = ZERO
              MY_ALLOCATE(LBUF%DAM,NEL*BUFLY%L_DAM)              
                  LBUF%DAM = ZERO
              MY_ALLOCATE(LBUF%DSUM,NEL*BUFLY%L_DSUM)            
                  LBUF%DSUM = ZERO
              MY_ALLOCATE(LBUF%DGLO,NEL*BUFLY%L_DGLO)            
                  LBUF%DGLO = ZERO
              MY_ALLOCATE(LBUF%CRAK,NEL*BUFLY%L_CRAK)            
                  LBUF%CRAK = ZERO
              MY_ALLOCATE(LBUF%EPE,NEL*BUFLY%L_EPE)              
                  LBUF%EPE = ZERO
              MY_ALLOCATE(LBUF%ANG,NEL*BUFLY%L_ANG)              
                  LBUF%ANG = ZERO
              MY_ALLOCATE(LBUF%EPC,NEL*BUFLY%L_EPC)              
                  LBUF%EPC = ZERO
              MY_ALLOCATE(LBUF%XST,NEL*BUFLY%L_XST)              
                  LBUF%XST = ZERO
              MY_ALLOCATE(LBUF%SSP,NEL*BUFLY%L_SSP)              
                  LBUF%SSP = ZERO
              MY_ALLOCATE(LBUF%Z,NEL*BUFLY%L_Z)                  
                  LBUF%Z = ZERO
              MY_ALLOCATE(LBUF%VISC,NEL*BUFLY%L_VISC)            
                  LBUF%VISC = ZERO
              MY_ALLOCATE(LBUF%SIGL,NEL*BUFLY%L_SIGL)            
                  LBUF%SIGL = ZERO
              MY_ALLOCATE(LBUF%SIGV,NEL*BUFLY%L_SIGV)            
                  LBUF%SIGV = ZERO
              MY_ALLOCATE(LBUF%SIGA,NEL*BUFLY%L_SIGA)            
                  LBUF%SIGA = ZERO
              MY_ALLOCATE(LBUF%SIGB,NEL*BUFLY%L_SIGB)            
                  LBUF%SIGB = ZERO
              MY_ALLOCATE(LBUF%SIGC,NEL*BUFLY%L_SIGC)            
                  LBUF%SIGC = ZERO
              MY_ALLOCATE(LBUF%SIGD,NEL*BUFLY%L_SIGD)            
                  LBUF%SIGD = ZERO
              MY_ALLOCATE(LBUF%SIGF,NEL*BUFLY%L_SIGF)            
                  LBUF%SIGF = ZERO
              MY_ALLOCATE(LBUF%SIG,NEL*BUFLY%L_SIG)                      
                  LBUF%SIG = ZERO
              MY_ALLOCATE(LBUF%SIGPLY,NEL*BUFLY%L_SIGPLY)                      
                  LBUF%SIGPLY = ZERO
              MY_ALLOCATE(LBUF%FOR,NEL*BUFLY%L_FOR)              
                  LBUF%FOR = ZERO
              MY_ALLOCATE(LBUF%MOM,NEL*BUFLY%L_MOM)              
                  LBUF%MOM = ZERO
              MY_ALLOCATE(LBUF%THK,NEL*BUFLY%L_THK)              
                  LBUF%THK = ZERO
              MY_ALLOCATE(LBUF%SMSTR,NEL*BUFLY%L_SMSTR)
                  LBUF%SMSTR = ZERO
              MY_ALLOCATE(LBUF%DMG,NEL*BUFLY%L_DMG)              
                  LBUF%DMG = ZERO
              MY_ALLOCATE(LBUF%FORTH,NEL*BUFLY%L_FORTH)              
                  LBUF%FORTH = ZERO
              MY_ALLOCATE(LBUF%EINTTH,NEL*BUFLY%L_EINTTH)              
                  LBUF%EINTTH = ZERO
              MY_ALLOCATE(LBUF%SEQ,NEL*BUFLY%L_SEQ)              
                  LBUF%SEQ = ZERO
              MY_ALLOCATE(LBUF%JAC_I,NEL*BUFLY%L_JAC_I)              
                  LBUF%JAC_I = ZERO
              MY_ALLOCATE(LBUF%FAC_YLD,NEL*BUFLY%L_FAC_YLD)              
                  LBUF%FAC_YLD = ONE
              MY_ALLOCATE(LBUF%ABURN,NEL*BUFLY%L_ABURN)                   
                  LBUF%ABURN = ZERO
              MY_ALLOCATE(LBUF%MU,NEL*BUFLY%L_MU)                   
                  LBUF%MU = ZERO
              MY_ALLOCATE(LBUF%PLANL,NEL*BUFLY%L_PLANL)                   
                  LBUF%PLANL = ZERO
              MY_ALLOCATE(LBUF%EPSDNL,NEL*BUFLY%L_EPSDNL)                   
                  LBUF%EPSDNL = ZERO
              MY_ALLOCATE(LBUF%DMGSCL,NEL*BUFLY%L_DMGSCL)                   
                  LBUF%DMGSCL = ONE
              MY_ALLOCATE(LBUF%TSAIWU,NEL*BUFLY%L_TSAIWU)                   
                  LBUF%TSAIWU = ZERO

              MY_ALLOCATE(LBUF%PIJ,NEL*BUFLY%L_PIJ)              
                  LBUF%PIJ = ZERO
C-------------initialization even in DP in Starter only---                  
              MY_ALLOCATE(LBUF%VOL0DP,NEL)              
                  LBUF%VOL0DP = ZERO
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO
c---
C
C layer mean variable (NPG = 1)
C
      DO I = 1, NLAY                                                        
        BUFLY => ELBUF_TAB(NG)%BUFLY(I)                                     
        IF (NPG == 1) THEN
          ILAYER = I
          IF (NLAY > 1) THEN
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = BUFLY%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(1,1,IT)
                BUFLY%PLAPT(1:NEL*BUFLY%LY_PLAPT) => LBUF%PLA
                BUFLY%SIGPT(1:NEL*BUFLY%LY_SIGPT) => LBUF%SIG
              ENDDO
            ELSE
              LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(1,1,1)
              BUFLY%PLAPT(1:NEL*BUFLY%LY_PLAPT) => LBUF%PLA
              BUFLY%SIGPT(1:NEL*BUFLY%LY_SIGPT) => LBUF%SIG
            ENDIF
          ELSE
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = BUFLY%NPTT
              DO IPT = 1,NPTT
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_PLAPT
                LEN2 = IPT*NEL*BUFLY%LY_PLAPT
                BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_SIGPT
                LEN2 = IPT*NEL*BUFLY%LY_SIGPT
                BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG
              ENDDO
            ELSE
              DO IPT = 1,NPT
                LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_PLAPT
                LEN2 = IPT*NEL*BUFLY%LY_PLAPT
                BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA(1:NEL*BUFLY%L_PLA)
                LEN1 = 1+(IPT-1)*NEL*BUFLY%LY_SIGPT
                LEN2 = IPT*NEL*BUFLY%LY_SIGPT
                BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
c-------------------------------------------------
c     Inter layers (ply xfem)
c-------------------------------------------------
      DO I = 1, NINTLAY                                                    
        INTLAY => ELBUF_TAB(NG)%INTLAY(I)                                  
        MY_ALLOCATE(ELBUF_TAB(NG)%INTLAY(I)%EINT,NEL)             
           ELBUF_TAB(NG)%INTLAY(I)%EINT  = ZERO                                         
        MY_ALLOCATE(ELBUF_TAB(NG)%INTLAY(I)%COUNT,NEL)             
           ELBUF_TAB(NG)%INTLAY(I)%COUNT = ZERO                                         
c
        DO IR = 1,NPTR                                                   
          DO IS = 1,NPTS                                                 
            ILBUF => ELBUF_TAB(NG)%INTLAY(I)%ILBUF(IR,IS)              
            MY_ALLOCATE(ILBUF%EPS,NEL*3)                   
              ILBUF%EPS = ZERO                                           
            MY_ALLOCATE (ILBUF%SIG,NEL*3)            
              ILBUF%SIG = ZERO                                         
          ENDDO                                                            
        ENDDO                                                            
      ENDDO                                                             
c---
                                                  
                                                   
                                                                        
                                                  
c-------------------------------------------------
C       global buffer allocation
c-------------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
      MY_ALLOCATE(GBUF%NOFF,NEL*GBUF%G_NOFF)                                      
         ELBUF_TAB(NG)%GBUF%NOFF = ZERO
      MY_ALLOCATE(GBUF%IERR,NEL*GBUF%G_IERR)                                      
        ELBUF_TAB(NG)%GBUF%IERR = ZERO                                                    
      MY_ALLOCATE(GBUF%GAMA,NEL*GBUF%G_GAMA)                                      
        ELBUF_TAB(NG)%GBUF%GAMA = ZERO
      MY_ALLOCATE(GBUF%SMSTR,NEL*GBUF%G_SMSTR)                                    
        ELBUF_TAB(NG)%GBUF%SMSTR = ZERO
      MY_ALLOCATE(GBUF%HOURG,NEL*GBUF%G_HOURG)                                    
        ELBUF_TAB(NG)%GBUF%HOURG = ZERO
      MY_ALLOCATE(GBUF%TAG22,NEL*GBUF%G_TAG22)   
        ELBUF_TAB(NG)%GBUF%TAG22 = ZERO                                                                                  
      MY_ALLOCATE(GBUF%STRA,NEL*GBUF%G_STRA)                           
        ELBUF_TAB(NG)%GBUF%STRA = ZERO                                                    
      MY_ALLOCATE(GBUF%STRW,NEL*GBUF%G_STRW)                                    
        ELBUF_TAB(NG)%GBUF%STRW = ZERO
      MY_ALLOCATE(GBUF%SIGI,NEL*GBUF%G_SIGI)                           
        ELBUF_TAB(NG)%GBUF%SIGI = ZERO                                                    
      MY_ALLOCATE(GBUF%DMG,NEL*GBUF%G_DMG)                               
        ELBUF_TAB(NG)%GBUF%DMG = ZERO                                                     
      MY_ALLOCATE(GBUF%UELR,NEL*GBUF%G_UELR)
        ELBUF_TAB(NG)%GBUF%UELR = ZERO
      MY_ALLOCATE(GBUF%UELR1,NEL*GBUF%G_UELR1)
        ELBUF_TAB(NG)%GBUF%UELR1 = ZERO
      MY_ALLOCATE(GBUF%DAMDL,NEL*GBUF%G_DAMDL)                           
        ELBUF_TAB(NG)%GBUF%DAMDL = ZERO                                                     
      MY_ALLOCATE(GBUF%FOR,NEL*GBUF%G_FOR)                            
        ELBUF_TAB(NG)%GBUF%FOR = ZERO                                          
      MY_ALLOCATE(GBUF%MOM,NEL*GBUF%G_MOM)                            
        ELBUF_TAB(NG)%GBUF%MOM = ZERO                                          
      MY_ALLOCATE(GBUF%THK,NEL*GBUF%G_THK)                         
        ELBUF_TAB(NG)%GBUF%THK = ZERO     
      MY_ALLOCATE(GBUF%THK_I,NEL*GBUF%G_THK_I)                         
        ELBUF_TAB(NG)%GBUF%THK_I = ZERO                                    
      MY_ALLOCATE(GBUF%JAC_I,NEL*GBUF%G_JAC_I)                         
        ELBUF_TAB(NG)%GBUF%JAC_I = ZERO                                    
      MY_ALLOCATE(GBUF%DT,NEL*GBUF%G_DT)                         
        ELBUF_TAB(NG)%GBUF%DT = ZERO
      MY_ALLOCATE(GBUF%ISMS,NEL*GBUF%G_ISMS)                         
        ELBUF_TAB(NG)%GBUF%ISMS = ZERO  
      MY_ALLOCATE(GBUF%STRHG,NEL*GBUF%G_STRHG)                         
        ELBUF_TAB(NG)%GBUF%STRHG = ZERO  
      MY_ALLOCATE(GBUF%BPRELD,NEL*GBUF%G_BPRELD)                         
        ELBUF_TAB(NG)%GBUF%BPRELD = ZERO  
      MY_ALLOCATE(GBUF%COR_NF,NEL*GBUF%G_COR_NF)                                    
        ELBUF_TAB(NG)%GBUF%COR_NF = ZERO
      MY_ALLOCATE(GBUF%COR_FR,NEL*GBUF%G_COR_FR)                                    
        ELBUF_TAB(NG)%GBUF%COR_FR = ZERO
      MY_ALLOCATE(GBUF%COR_XR,NEL*GBUF%G_COR_XR)                                    
        ELBUF_TAB(NG)%GBUF%COR_XR = ZERO
      MY_ALLOCATE(GBUF%MAXFRAC,NEL*GBUF%G_MAXFRAC)                                    
        ELBUF_TAB(NG)%GBUF%MAXFRAC = ZERO
      MY_ALLOCATE(GBUF%MAXEPS,NEL*GBUF%G_MAXEPS)                                    
        ELBUF_TAB(NG)%GBUF%MAXEPS = ZERO
      MY_ALLOCATE(GBUF%GAMA_R,NEL*GBUF%G_GAMA_R)                                      
        ELBUF_TAB(NG)%GBUF%GAMA_R = ZERO
c           Allocate Orthotropic Beta angle (dyna input format)
      MY_ALLOCATE(GBUF%BETAORTH,NEL*GBUF%G_BETAORTH)                                     
        ELBUF_TAB(NG)%GBUF%BETAORTH= ZERO
      MY_ALLOCATE(GBUF%AMU,NEL*GBUF%G_AMU)                                    
        ELBUF_TAB(NG)%GBUF%AMU = ZERO

c
      IF (NPG == 1) THEN
        ELBUF_TAB(NG)%GBUF%FORPG => GBUF%FOR
        ELBUF_TAB(NG)%GBUF%MOMPG => GBUF%MOM
C
C        ELBUF_TAB(NG)%GBUF%FORPGPINCH => GBUF%FORPINCH ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%MOMPGPINCH => GBUF%MOMPINCH ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHXZ => GBUF%EPGPINCHXZ ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHYZ => GBUF%EPGPINCHYZ ! [PM] add later
C        ELBUF_TAB(NG)%GBUF%EPGPINCHZZ => GBUF%EPGPINCHZZ ! [PM] add later
C
        ELBUF_TAB(NG)%GBUF%STRPG => GBUF%STRA
        ELBUF_TAB(NG)%GBUF%STRWPG=> GBUF%STRW
      ELSEIF (NPG > 1) THEN
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%FORPG,NEL*GBUF%G_FORPG)
           ELBUF_TAB(NG)%GBUF%FORPG = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%MOMPG,NEL*GBUF%G_MOMPG)
          ELBUF_TAB(NG)%GBUF%MOMPG = ZERO
C
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%FORPGPINCH,NEL*GBUF%G_FORPGPINCH)
           ELBUF_TAB(NG)%GBUF%FORPGPINCH = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%MOMPGPINCH,NEL*GBUF%G_MOMPGPINCH)
          ELBUF_TAB(NG)%GBUF%MOMPGPINCH = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHXZ,NEL*GBUF%G_EPGPINCHXZ)
          ELBUF_TAB(NG)%GBUF%EPGPINCHXZ = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHYZ,NEL*GBUF%G_EPGPINCHYZ)
          ELBUF_TAB(NG)%GBUF%EPGPINCHYZ = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EPGPINCHZZ,NEL*GBUF%G_EPGPINCHZZ)
          ELBUF_TAB(NG)%GBUF%EPGPINCHZZ = ZERO
C
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%STRPG,NEL*GBUF%G_STRPG)
          ELBUF_TAB(NG)%GBUF%STRPG = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%STRWPG,NEL*GBUF%G_STRWPG)
          ELBUF_TAB(NG)%GBUF%STRWPG = ZERO  
      ENDIF
c
c-------------------------------------------------
c      IF (NPT == 1) THEN  ! NPT = 1 ! global variables are pointing to local ones    
      IF (NPTTOT == 1) THEN  ! NPTTOT = 1 ! global variables are pointing to local ones    
        LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                   
c
        ELBUF_TAB(NG)%GBUF%BFRAC  => LBUF%BFRAC                                      
        ELBUF_TAB(NG)%GBUF%OFF    => LBUF%OFF                                        
        ELBUF_TAB(NG)%GBUF%EINT   => LBUF%EINT                                       
        ELBUF_TAB(NG)%GBUF%EINS   => LBUF%EINS                                       
        ELBUF_TAB(NG)%GBUF%RHO    => LBUF%RHO                                        
        ELBUF_TAB(NG)%GBUF%QVIS   => LBUF%QVIS                                       
        ELBUF_TAB(NG)%GBUF%DELTAX => LBUF%DELTAX                                     
        ELBUF_TAB(NG)%GBUF%VOL    => LBUF%VOL                                        
        ELBUF_TAB(NG)%GBUF%EPSD   => LBUF%EPSD                                       
        ELBUF_TAB(NG)%GBUF%EPSQ   => LBUF%EPSQ                                       
        ELBUF_TAB(NG)%GBUF%PLA    => LBUF%PLA                                        
        ELBUF_TAB(NG)%GBUF%TEMP   => LBUF%TEMP                                       
        ELBUF_TAB(NG)%GBUF%TB     => LBUF%TB                                         
        ELBUF_TAB(NG)%GBUF%RK     => LBUF%RK                                         
        ELBUF_TAB(NG)%GBUF%RE     => LBUF%RE                                         
        ELBUF_TAB(NG)%GBUF%SIG    => LBUF%SIG                                        
        ELBUF_TAB(NG)%GBUF%FORTH  => LBUF%FORTH                                      
        ELBUF_TAB(NG)%GBUF%EINTTH => LBUF%EINTTH
        ELBUF_TAB(NG)%GBUF%SEQ    => LBUF%SEQ
        ELBUF_TAB(NG)%GBUF%ABURN  => LBUF%ABURN   
        ELBUF_TAB(NG)%GBUF%MU     => LBUF%MU                                    
      ELSE  ! NPT > 1                                                                
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%BFRAC,NEL*GBUF%G_BFRAC)                
              ELBUF_TAB(NG)%GBUF%BFRAC = ZERO 	  
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%OFF,NEL*GBUF%G_OFF) 
              ELBUF_TAB(NG)%GBUF%OFF = ONE               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EINT,NEL*GBUF%G_EINT)                      
              ELBUF_TAB(NG)%GBUF%EINT = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EINS,NEL*GBUF%G_EINS)                      
              ELBUF_TAB(NG)%GBUF%EINS = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%RHO,NEL*GBUF%G_RHO)                       
              ELBUF_TAB(NG)%GBUF%RHO = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%QVIS,NEL*GBUF%G_QVIS)                      
              ELBUF_TAB(NG)%GBUF%QVIS = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%DELTAX,NEL*GBUF%G_DELTAX)
              ELBUF_TAB(NG)%GBUF%DELTAX = ZERO             
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%VOL,NEL*GBUF%G_VOL)                       
              ELBUF_TAB(NG)%GBUF%VOL = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EPSD,NEL*GBUF%G_EPSD)                      
              ELBUF_TAB(NG)%GBUF%EPSD = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EPSQ,NEL*GBUF%G_EPSQ)  
              ELBUF_TAB(NG)%GBUF%EPSQ = ZERO 
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%PLA,NEL*GBUF%G_PLA)                       
              ELBUF_TAB(NG)%GBUF%PLA = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%TEMP,NEL*GBUF%G_TEMP)                      
              ELBUF_TAB(NG)%GBUF%TEMP = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%TB,NEL*GBUF%G_TB)                        
              ELBUF_TAB(NG)%GBUF%TB = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%RK,NEL*GBUF%G_RK)                      
              ELBUF_TAB(NG)%GBUF%RK = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%RE,NEL*GBUF%G_RE) 
              ELBUF_TAB(NG)%GBUF%RE = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%SIG,NEL*GBUF%G_SIG)                       
              ELBUF_TAB(NG)%GBUF%SIG = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%FORTH,NEL*GBUF%G_FORTH)                       
              ELBUF_TAB(NG)%GBUF%FORTH = ZERO  
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%EINTTH,NEL*GBUF%G_EINTTH)                       
              ELBUF_TAB(NG)%GBUF%EINTTH = ZERO               
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%SEQ,NEL*GBUF%G_SEQ)                       
              ELBUF_TAB(NG)%GBUF%SEQ = ZERO
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%ABURN,NEL*GBUF%G_ABURN)                
              ELBUF_TAB(NG)%GBUF%ABURN = ZERO 	
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%MU,NEL*GBUF%G_MU)                
              ELBUF_TAB(NG)%GBUF%MU = ZERO 
      ENDIF  ! NPT
c---
      ALLOCATE(ELBUF_TAB(NG)%GBUF%FILL(NEL*GBUF%G_FILL),STAT=Stat)
        ELBUF_TAB(NG)%GBUF%FILL = ONE    
                                                   
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      ALLOCATE (GBUF%AREA(NEL*GBUF%G_AREA)               ,STAT=Stat)
        GBUF%AREA = ZERO
      ALLOCATE (GBUF%SKEW(NEL*GBUF%G_SKEW)               ,STAT=Stat)
        GBUF%SKEW = ZERO
      ALLOCATE (GBUF%LENGTH(NEL*GBUF%G_LENGTH)           ,STAT=Stat)
        GBUF%LENGTH = ZERO
      ALLOCATE (GBUF%TOTDEPL(NEL*GBUF%G_TOTDEPL)         ,STAT=Stat)
        GBUF%TOTDEPL = ZERO
      ALLOCATE (GBUF%TOTROT(NEL*GBUF%G_TOTROT)           ,STAT=Stat)
        GBUF%TOTROT = ZERO
      ALLOCATE (GBUF%FOREP(NEL*GBUF%G_FOREP)             ,STAT=Stat)
        GBUF%FOREP = ZERO
      ALLOCATE (GBUF%MOMEP(NEL*GBUF%G_MOMEP)             ,STAT=Stat)
        GBUF%MOMEP = ZERO
      ALLOCATE (GBUF%DEP_IN_TENS(NEL*GBUF%G_DEP_IN_TENS) ,STAT=Stat)
        GBUF%DEP_IN_TENS = ZERO
      ALLOCATE (GBUF%DEP_IN_COMP(NEL*GBUF%G_DEP_IN_COMP) ,STAT=Stat)
        GBUF%DEP_IN_COMP = ZERO
      ALLOCATE (GBUF%ROT_IN_TENS(NEL*GBUF%G_ROT_IN_TENS) ,STAT=Stat)
        GBUF%ROT_IN_TENS = ZERO
      ALLOCATE (GBUF%ROT_IN_COMP(NEL*GBUF%G_ROT_IN_COMP) ,STAT=Stat)
        GBUF%ROT_IN_COMP = ZERO
      ALLOCATE (GBUF%POSX(NEL*GBUF%G_POSX)               ,STAT=Stat)
        GBUF%POSX = ZERO
      ALLOCATE (GBUF%POSY(NEL*GBUF%G_POSY)               ,STAT=Stat)
        GBUF%POSY = ZERO
      ALLOCATE (GBUF%POSZ(NEL*GBUF%G_POSZ)               ,STAT=Stat)
        GBUF%POSZ = ZERO
      ALLOCATE (GBUF%POSXX(NEL*GBUF%G_POSXX)             ,STAT=Stat)
        GBUF%POSXX = ZERO
      ALLOCATE (GBUF%POSYY(NEL*GBUF%G_POSYY)             ,STAT=Stat)
        GBUF%POSYY = ZERO
      ALLOCATE (GBUF%POSZZ(NEL*GBUF%G_POSZZ)             ,STAT=Stat)
        GBUF%POSZZ = ZERO
      ALLOCATE (GBUF%YIELD(NEL*GBUF%G_YIELD)             ,STAT=Stat)
        GBUF%YIELD = ZERO
      ALLOCATE (GBUF%LENGTH_ERR(NEL*GBUF%G_LENGTH_ERR)   ,STAT=Stat)
        GBUF%LENGTH_ERR = ZERO
      ALLOCATE (GBUF%DV(NEL*GBUF%G_DV)                   ,STAT=Stat)
        GBUF%DV = ZERO
      ALLOCATE (GBUF%DFS(NEL*GBUF%G_DFS)                 ,STAT=Stat)
        GBUF%DFS = ZERO
      ALLOCATE (GBUF%SKEW_ERR(NEL*GBUF%G_SKEW_ERR)       ,STAT=Stat)
        GBUF%SKEW_ERR = ZERO
      ALLOCATE (GBUF%E6(NEL*GBUF%G_E6)                   ,STAT=Stat)
        GBUF%E6 = ZERO
      ALLOCATE (GBUF%RUPTCRIT(NEL*GBUF%G_RUPTCRIT)       ,STAT=Stat)
        GBUF%RUPTCRIT = ZERO
      ALLOCATE (GBUF%MASS(NEL*GBUF%G_MASS)               ,STAT=Stat)
        GBUF%MASS = ZERO
      ALLOCATE (GBUF%V_REPCVT(NEL*GBUF%G_V_REPCVT)       ,STAT=Stat)
        GBUF%V_REPCVT = ZERO
      ALLOCATE (GBUF%VR_REPCVT(NEL*GBUF%G_VR_REPCVT)     ,STAT=Stat)
        GBUF%VR_REPCVT = ZERO
      ALLOCATE (GBUF%VAR(NEL*GBUF%G_NUVAR)               ,STAT=Stat)
        GBUF%VAR = ZERO
      ALLOCATE (GBUF%VARN(NEL*GBUF%G_NUVARN)             ,STAT=Stat)
        GBUF%VARN = ZERO
      ALLOCATE (GBUF%DEFINI(NEL*GBUF%G_DEFINI)           ,STAT=Stat)
        GBUF%DEFINI = ZERO
      ALLOCATE (GBUF%FORINI(NEL*GBUF%G_FORINI)           ,STAT=Stat)
        GBUF%FORINI = ZERO
      ALLOCATE (GBUF%INIFRIC(NEL*GBUF%G_INIFRIC)         ,STAT=Stat)
        GBUF%INIFRIC = ZERO
c---
      ALLOCATE (GBUF%DT_PITER(NEL*GBUF%G_DT_PITER)         ,STAT=Stat)
        GBUF%DT_PITER = ZERO
      ALLOCATE (GBUF%IDT_TSH(NEL*GBUF%G_IDT_TSH)         ,STAT=Stat)
        GBUF%IDT_TSH = 1
C --- needed for batoz shell and /heat/mat 
      IF (NPG == 1) THEN
        ELBUF_TAB(NG)%GBUF%TEMPG => GBUF%TEMP
      ELSEIF (NPG > 1) THEN
        MY_ALLOCATE(ELBUF_TAB(NG)%GBUF%TEMPG,NEL*GBUF%G_TEMPG) 
        ELBUF_TAB(NG)%GBUF%TEMPG = ZERO  
      ENDIF
C
      ALLOCATE (GBUF%SKEW_ID(NEL*GBUF%G_SKEW_ID)                 ,STAT=Stat)
        GBUF%SKEW_ID = ZERO
c---
c seatbelt elements
c---
      ALLOCATE (GBUF%SLIPRING_ID(NEL*GBUF%G_SLIPRING_ID)             ,STAT=Stat)
        GBUF%SLIPRING_ID = 0
      ALLOCATE (GBUF%SLIPRING_FRAM_ID(NEL*GBUF%G_SLIPRING_FRAM_ID)    ,STAT=Stat)
        GBUF%SLIPRING_FRAM_ID = 0
      ALLOCATE (GBUF%SLIPRING_STRAND(NEL*GBUF%G_SLIPRING_STRAND)     ,STAT=Stat)
        GBUF%SLIPRING_STRAND = 0
      ALLOCATE (GBUF%RETRACTOR_ID(NEL*GBUF%G_RETRACTOR_ID)           ,STAT=Stat)
        GBUF%RETRACTOR_ID = 0
      ALLOCATE (GBUF%RINGSLIP(NEL*GBUF%G_RINGSLIP)                   ,STAT=Stat)
        GBUF%RINGSLIP = ZERO
      ALLOCATE (GBUF%ADD_NODE(NEL*GBUF%G_ADD_NODE)                   ,STAT=Stat)
        GBUF%ADD_NODE = 0
      ALLOCATE (GBUF%UPDATE(NEL*GBUF%G_UPDATE)                       ,STAT=Stat)
        GBUF%UPDATE = 0
      ALLOCATE (GBUF%FRAM_FACTOR(NEL*GBUF%G_FRAM_FACTOR)             ,STAT=Stat)
        GBUF%FRAM_FACTOR = ZERO  
      ALLOCATE (GBUF%INTVAR(NEL*GBUF%G_INTVAR)                       ,STAT=Stat)
        GBUF%INTVAR = ZERO          
c---
C ---H3D/TMAX in BUFLEN         
c---
      ALLOCATE (GBUF%TM_YIELD(NEL*GBUF%G_TM_YIELD)           ,STAT=Stat)
                GBUF%TM_YIELD = ZERO
      ALLOCATE (GBUF%TM_SEQ(NEL*GBUF%G_TM_SEQ)               ,STAT=Stat)
                GBUF%TM_SEQ = ZERO
      ALLOCATE (GBUF%TM_EINT(NEL*GBUF%G_TM_EINT)             ,STAT=Stat)
                GBUF%TM_EINT = ZERO
      ALLOCATE (GBUF%TM_DMG(NEL*GBUF%G_TM_DMG)               ,STAT=Stat)
                GBUF%TM_DMG = ZERO
      ALLOCATE (GBUF%TM_SIG1(NEL*GBUF%G_TM_SIG)               ,STAT=Stat)
                GBUF%TM_SIG1 = ZERO  
      ALLOCATE (GBUF%TM_STRA1(NEL*GBUF%G_TM_STRA)             ,STAT=Stat)
                GBUF%TM_STRA1 = ZERO
      ALLOCATE (GBUF%TM_SIG3(NEL*GBUF%G_TM_SIG)               ,STAT=Stat)
                GBUF%TM_SIG3 = ZERO  
      ALLOCATE (GBUF%TM_STRA3(NEL*GBUF%G_TM_STRA)             ,STAT=Stat)
                GBUF%TM_STRA3 = ZERO
                
      IF (STAT /= 0) THEN                         
            CALL ANCMSG(MSGID=727,MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,C1='ELEMENT BUFFER')              
      ENDIF                                       
c
c-----    Viscosity model buffer - per integration point                                                                  
c
c        DO IR = 1,NPTR                                       
c          DO IS = 1,NPTS                                      
c            DO IT = 1,NPTT   
c              VISCBUF => ELBUF_TAB(NG)%VISC(IR,IS,IT)
c              VISCBUF%LawId = IMAT
c              VISCBUF%MVISC = IPM(220,IMAT)    
c              VISCBUF%NVAR  = IPM(225,IMAT)    
c              print*,'visc_nvar=',VISCBUF%NVAR
c              ALLOCATE(VISCBUF%VAR(NEL*NVAR)  ,STAT=Stat)
c              VISCBUF%VAR(1:NEL*NVAR) = ZERO
c            ENDDO                                                           
c          ENDDO                                                            
c        ENDDO 
c-------------------------------
c         EOS model variables    (pas de UVAR pour l'instant)                        
c-------------------------------
c        IF (IEOS /= 0) THEN   
c          ALLOCATE (ELBUF_TAB(NG)%EOS(NPTR,NPTS,NPTT), STAT=Stat)
c          IF (STAT /= 0) THEN
c            CALL ANSTCKC(6,'SOLBUF')  
c            CALL ANCStat(727,ANSTOP)                      
c          ENDIF
C
c          DO IR = 1,NPTR                                     
c           DO IS = 1,NPTS                                    
c            DO IT = 1,NPTT
c                ALLOCATE(ELBUF_TAB(NG)%EOS(IR,IS,IT)%VAR(NEL,NVAR),           
c     .                                                     STAT=Stat)                                                            
c            ENDDO                                                         
c           ENDDO                                                          
c          ENDDO                                                           
c        ENDIF                                            
c-------------------------------
c         Viscosity model variables                   
c-------------------------------
C-----------------------------------------------
      RETURN
      END SUBROUTINE ALLOCBUF_AUTO
