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
Chd|  I6ASS3                        source/interfaces/inter3d/i6ass3.F
Chd|-- called by -----------
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I6ASS3(E       ,MSR     ,NSV     ,ES      ,
     2                  EM      ,NPC     ,TF      ,ANSMX   ,
     3                  FMX     ,FMY     ,FMZ     ,XMAS    ,
     4                  IFUNC   ,V       ,NOINT   ,NSN     ,
     5                  NMN     ,FSAV    ,DT2T    ,NELTST  ,
     6                  ITYPTST ,FFAC    ,FSKYI   ,ISKY    ,
     7                  FCONT   ,FACX    ,FAC2    ,STIFF   ,
     8                  HFLAG   ,IFUN2   ,ICOR    ,PENI    ,
     9                  ANSMX0  ,FF0     ,H3D_DATA)
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
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST, IFUNC,IFUN2, NOINT, NSN, NMN,HFLAG,ICOR
      INTEGER MSR(*), NSV(*), NPC(*), ISKY(*)
C     REAL
      my_real 
     .   DT2T,ANSMX,ANSMX0,FF0,FMX,FMY,FMZ,XMAS,FFAC,FACX,FAC2,STIFF,
     .   PENI
      my_real
     .   E(*),ES(*),EM(*),TF(*),V(*),FSAV(*),FSKYI(LSKYI,NFSKYI),
     .   FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPOINT, IL, IG, IG3, IG2, IG1, IL3, IL2, IL1, NISKYL
C     REAL
      my_real
     .   VSMAX, VMMAX, VMAX, FT,FU,FF, XK, DTMI, FAC, FACDT, DX, FINTER
      EXTERNAL FINTER
C=======================================================================
      IF (TT == ZERO) THEN
        ANSMX0 = ZERO
        FF0    = ZERO
        IF (ICOR == 1) THEN
          ANSMX0 = -ANSMX
          IF (HFLAG == 1) FF0 = FINTER(IFUN2,ABS(PENI)*FACX,NPC,TF,XK)
        ENDIF
      ENDIF
c
      IF (ANSMX > ZERO)THEN
        VSMAX =ZERO
        VMMAX =ZERO
        ANSMX0=ZERO
C
        DO IL=1,NSN
          IG=NSV(IL)
          IG3=3*IG
          IG2=IG3-1
          IG1=IG2-1
          VSMAX = MAX(VSMAX,V(IG1)**2+V(IG2)**2+V(IG3)**2)
        ENDDO
C
        DO IL=1,NMN
          IG=MSR(IL)
          IG3=3*IG
          IG2=IG3-1
          IG1=IG2-1
          VMMAX = MAX(VMMAX,V(IG1)**2+V(IG2)**2+V(IG3)**2)
        ENDDO
C
        VMAX  = SQRT(VSMAX)+SQRT(VMMAX)+ EM15
        FT = FINTER(IFUNC,ZERO,NPC,TF,XK)
        XK = MAX(EM15,XK)
        DTMI = MAX(EM01*SQRT(XMAS/XK),ANSMX/VMAX)
C------------------------------------------------
      ELSEIF (ANSMX == ZERO) THEN
        FT = FINTER(IFUNC,ANSMX*FACX,NPC,TF,XK)  
        XK = MAX(EM15,XK*FFAC)                   
        FT = FT*FFAC                             
        DTMI = EM01*SQRT(XMAS/XK)
C------------------------------------------------
      ELSE 
C       ANSMX < 0                                                    
        ANSMX = -ANSMX                                               
        FT = FINTER(IFUNC,ANSMX*FACX,NPC,TF,XK)
        XK = MAX(EM15,XK*FFAC)                                       
        FT = FT*FFAC
        FU = ZERO
        IF (HFLAG == 1) THEN
          FU = FINTER(IFUN2,ANSMX*FACX,NPC,TF,XK)                    
          FU = FU*FAC2
        ENDIF
C                                                                    
        IF (HFLAG > 0) THEN                                     
          DX = ANSMX-ANSMX0
          IF (DX >= ZERO) THEN                                        
c           loading                                                  
            FT  = MIN(FT, FF0 + STIFF*DX) 
          ELSE                                                       
c           unloading                                                
            FT  = MAX(FU, FF0 + STIFF*DX)                                     
          ENDIF                                                      
          XK  = FT - FF0 / MAX(EM15,DX)                    
          ANSMX0 = ANSMX   
          FF0 = FT 
        ENDIF
        FAC   = FT / MAX(EM15,SQRT(FMX**2+FMY**2+FMZ**2))
        FACDT = FAC*DT1
C------------------------------------------------
        FSAV(1)=FSAV(1)+FMX*FACDT        
        FSAV(2)=FSAV(2)+FMY*FACDT        
        FSAV(3)=FSAV(3)+FMZ*FACDT        
        FSAV(4)=FSAV(4)-FMX*FACDT        
        FSAV(5)=FSAV(5)-FMY*FACDT        
        FSAV(6)=FSAV(6)-FMZ*FACDT        
C
        IF (IPARIT == 0) THEN            
          DO 190 IL=1,NSN                
          IG=NSV(IL)                     
          IG3=3*IG                       
          IG2=IG3-1                      
          IG1=IG2-1                      
          IL3=3*IL                       
          IL2=IL3-1                      
          IL1=IL2-1                      
          E(IG1)=ES(IL1)*FAC             
          E(IG2)=ES(IL2)*FAC             
          E(IG3)=ES(IL3)*FAC             
 190      CONTINUE                       
C
          DO 200 IL=1,NMN                
          IG=MSR(IL)                     
          IG3=3*IG                       
          IG2=IG3-1                      
          IG1=IG2-1                      
          IL3=3*IL                       
          IL2=IL3-1                      
          IL1=IL2-1                      
          E(IG1)=EM(IL1)*FAC             
          E(IG2)=EM(IL2)*FAC             
          E(IG3)=EM(IL3)*FAC             
          FSAV(4)=FSAV(4)-EM(IL1)*FACDT  
          FSAV(5)=FSAV(5)-EM(IL2)*FACDT  
          FSAV(6)=FSAV(6)-EM(IL3)*FACDT  
 200      CONTINUE                       
                                         
        ELSE                             
C         IPARIT /= 0                      
#include "lockon.inc"                      
          NISKYL = NISKY                   
          NISKY = NISKY + NSN + NMN        
#include "lockoff.inc"                     
          IF (KDTINT == 0) THEN            
            DO 220 IL=1,NSN                
            IL3=3*IL                       
            IL2=IL3-1                      
            IL1=IL2-1                      
            NISKYL = NISKYL + 1            
            FSKYI(NISKYL,1)=ES(IL1)*FAC    
            FSKYI(NISKYL,2)=ES(IL2)*FAC    
            FSKYI(NISKYL,3)=ES(IL3)*FAC    
            FSKYI(NISKYL,4)=ZERO           
            ISKY(NISKYL) = NSV(IL)         
 220        CONTINUE                       
C
            DO 240 IL=1,NMN                
            IL3=3*IL                       
            IL2=IL3-1                      
            IL1=IL2-1                      
            NISKYL = NISKYL + 1            
            FSKYI(NISKYL,1)=EM(IL1)*FAC    
            FSKYI(NISKYL,2)=EM(IL2)*FAC    
            FSKYI(NISKYL,3)=EM(IL3)*FAC    
            FSKYI(NISKYL,4)=ZERO           
            ISKY(NISKYL) = MSR(IL)         
            FSAV(4)=FSAV(4)-EM(IL1)*FACDT  
            FSAV(5)=FSAV(5)-EM(IL2)*FACDT  
            FSAV(6)=FSAV(6)-EM(IL3)*FACDT  
 240        CONTINUE                       
          ELSE                             
            DO IL=1,NSN                    
            IL3=3*IL                       
            IL2=IL3-1                      
            IL1=IL2-1                      
            NISKYL = NISKYL + 1            
            FSKYI(NISKYL,1)=ES(IL1)*FAC    
            FSKYI(NISKYL,2)=ES(IL2)*FAC    
            FSKYI(NISKYL,3)=ES(IL3)*FAC    
            FSKYI(NISKYL,4)=ZERO           
            FSKYI(NISKYL,5)=ZERO           
            ISKY(NISKYL) = NSV(IL)         
            ENDDO                          
C
            DO IL=1,NMN                    
            IL3=3*IL                       
            IL2=IL3-1                      
            IL1=IL2-1                      
            NISKYL = NISKYL + 1            
            FSKYI(NISKYL,1)=EM(IL1)*FAC    
            FSKYI(NISKYL,2)=EM(IL2)*FAC    
            FSKYI(NISKYL,3)=EM(IL3)*FAC    
            FSKYI(NISKYL,4)=ZERO           
            FSKYI(NISKYL,5)=ZERO           
            ISKY(NISKYL) = MSR(IL)         
            FSAV(4)=FSAV(4)-EM(IL1)*FACDT  
            FSAV(5)=FSAV(5)-EM(IL2)*FACDT  
            FSAV(6)=FSAV(6)-EM(IL3)*FACDT  
            ENDDO                          
          ENDIF
        ENDIF
C-------  parith      
        IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0.AND.                      
     .    ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.                     
     .   (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN                   
#include "lockon.inc"                                         
           DO IL=1,NSN                                        
            IL3=3*IL                                          
            IL2=IL3-1                                         
            IL1=IL2-1                                         
            FCONT(1,NSV(IL)) =FCONT(1,NSV(IL)) + ES(IL1)*FAC  
            FCONT(2,NSV(IL)) =FCONT(2,NSV(IL)) + ES(IL2)*FAC  
            FCONT(3,NSV(IL)) =FCONT(3,NSV(IL)) + ES(IL3)*FAC 
           ENDDO                                              
C
           DO IL=1,NMN                                        
            IL3=3*IL                                          
            IL2=IL3-1                                         
            IL1=IL2-1                                         
            FCONT(1,MSR(IL)) =FCONT(1,MSR(IL)) + EM(IL1)*FAC  
            FCONT(2,MSR(IL)) =FCONT(2,MSR(IL)) + EM(IL2)*FAC  
            FCONT(3,MSR(IL)) =FCONT(3,MSR(IL)) + EM(IL3)*FAC  
           ENDDO                                              
#include "lockoff.inc"                                        
        ENDIF                                                 
C
        XK   = MAX(XK,FT / MAX(EM15,ANSMX))
        DTMI = EM01*SQRT(XMAS/MAX(XK,EM20))
      ENDIF
C-----------
      IF(DTMI<DT2T)THEN
        DT2T    = DTMI   
        NELTST  = NOINT  
        ITYPTST = 10     
      ENDIF
C-----------
      RETURN
      END
