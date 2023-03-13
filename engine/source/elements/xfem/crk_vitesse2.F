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
Chd|  CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        UPXVIT_C1                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_C2                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_T1                     source/elements/xfem/crk_vitesse2.F
Chd|        UPXVIT_T2                     source/elements/xfem/crk_vitesse2.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_VITESSE2(IPARG    ,NGROUC   ,IGROUC   ,ELCUTC   ,CRKEDGE  ,
     .                        NODEDGE  ,IXC      ,IXTG     ,XEDGE4N  ,XEDGE3N  ,
     .                        IADC_CRK ,IEL_CRK  ,INOD_CRK ,ITAB     )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUC
      INTEGER IPARG(NPARG,*),IGROUC(*),IADC_CRK(*),IEL_CRK(*),INOD_CRK(*),
     .   ELCUTC(2,*),NODEDGE(2,*),IXC(NIXC,*),IXTG(NIXTG,*),ITAB(*),
     .   XEDGE4N(4,*),XEDGE3N(3,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,ITY,NG,NEL,NFT,ITG1,ITG2,IXFEM,NXLAY,GOFF,XOFF
C=======================================================================
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
        NG = IGROUC(IG)
        IXFEM = IPARG(54,NG) 
        GOFF  = IPARG(8,NG)     ! GROUP OFF
        XOFF  = IPARG(70,NG)    ! XFEEM GROUP ACTIVITY FLAG   
        IF (IXFEM == 0 .or. GOFF == 1 .or. XOFF == 0) CYCLE
        ITY  = IPARG(5,NG)
        NEL  = IPARG(2,NG)
        NFT  = IPARG(3,NG)
        NXLAY= IPARG(59,NG)
c       copy velocities inside cracked element if ITRI/=0
        IF (ITY == 3) THEN
          CALL UPXVIT_C1(NEL     ,NFT     ,NXLAY  ,ELCUTC,
     .                   IEL_CRK ,IADC_CRK)
        ELSEIF (ITY == 7) THEN
          ITG1 = 1 + NUMELC
          ITG2 = 1 + ECRKXFEC*4
          CALL UPXVIT_T1(NEL    ,NFT   ,NXLAY  ,ELCUTC(1,ITG1) ,
     .                   IEL_CRK(ITG1) ,IADC_CRK(ITG2))
        ENDIF
      END DO
!$OMP END DO
c
c-----------
c
C Boucle parallele dynamique SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
        NG = IGROUC(IG)
        IXFEM = IPARG(54,NG) 
        GOFF  = IPARG(8,NG)     ! GROUP OFF
        XOFF  = IPARG(70,NG)    ! XFEEM GROUP ACTIVITY FLAG   
        IF (IXFEM == 0 .or. GOFF == 1 .or. XOFF == 0) CYCLE
        ITY  = IPARG(5,NG)
        NEL  = IPARG(2,NG)
        NFT  = IPARG(3,NG)
        NXLAY= IPARG(59,NG)
c       copy velocities between cracked elements
        IF (ITY == 3) THEN
          CALL UPXVIT_C2(NEL      ,NFT      ,NXLAY    ,IXC      ,XEDGE4N  ,
     .                   CRKEDGE  ,NODEDGE  ,IEL_CRK  ,IADC_CRK ,INOD_CRK ,
     .                   ELCUTC   ,ITAB     )
        ELSEIF (ITY == 7) THEN
          ITG1 = 1 + NUMELC
          ITG2 = 1 + ECRKXFEC*4
          CALL UPXVIT_T2(NEL      ,NFT     ,NXLAY     ,IXTG      ,XEDGE3N,
     .                   CRKEDGE  ,NODEDGE ,IEL_CRK(ITG1),IADC_CRK(ITG2),INOD_CRK ,
     .                   ELCUTC(1,ITG1) ,ITAB   )
        ENDIF
      END DO
!$OMP END DO
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  UPXVIT_C1                     source/elements/xfem/crk_vitesse2.F
Chd|-- called by -----------
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPXVIT_C1(NEL      ,NFT      ,NXLAY   ,ELCUTC ,
     .                     IEL_CRK  ,IADC_CRK )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,NXLAY
      INTEGER ELCUTC(2,*),IADC_CRK(4,*),IEL_CRK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,ILAY,IXEL,ILEV,IL,ICUT,ELEM,ELCRK,IAD,ITRI,EN,EN0,EN1
C=======================================================================
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO I=1,NEL
          ELEM = I+NFT                                              
          ELCRK = IEL_CRK(ELEM)
          ICUT  = ELCUTC(1,ELEM)                                       
          IF (ELCRK > 0 .and. ICUT > 0) THEN
            ITRI = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)                 
c                  print*,'vit2 :ELCRK,ITRI=',  ELCRK,ITRI                                
c----
            IF (ITRI < 0) THEN    ! copy IXEL=3 => IXEL=2
              IXEL = 2
              ILEV = II + IXEL
              DO K=1,4
                IAD = IADC_CRK(K,ELCRK)
                EN0 = CRKLVSET(ILEV)%ENR0(2,IAD)  ! enr initial du debut de cycle
                EN  = CRKLVSET(ILEV)%ENR0(1,IAD)  ! enr mise a jour dans le cycle
                IF (EN0 < 0 .and. EN > 0) THEN
                  IL  = ILEV+1                                         
                  EN1 = CRKLVSET(IL)%ENR0(2,IAD)                        
                  IF (EN1 > 0) THEN
                    CRKAVX(ILEV)%X(1,IAD)  = CRKAVX(IL)%X(1,IAD)        
                    CRKAVX(ILEV)%X(2,IAD)  = CRKAVX(IL)%X(2,IAD)        
                    CRKAVX(ILEV)%X(3,IAD)  = CRKAVX(IL)%X(3,IAD)        
                    CRKAVX(ILEV)%V(1,IAD)  = CRKAVX(IL)%V(1,IAD)        
                    CRKAVX(ILEV)%V(2,IAD)  = CRKAVX(IL)%V(2,IAD)        
                    CRKAVX(ILEV)%V(3,IAD)  = CRKAVX(IL)%V(3,IAD)        
                    CRKAVX(ILEV)%VR(1,IAD) = CRKAVX(IL)%VR(1,IAD)       
                    CRKAVX(ILEV)%VR(2,IAD) = CRKAVX(IL)%VR(2,IAD)       
                    CRKAVX(ILEV)%VR(3,IAD) = CRKAVX(IL)%VR(3,IAD)       
                  ENDIF                                                
                ENDIF                                                
              ENDDO
            ELSEIF (ITRI > 0) THEN        ! copy IXEL=3 => IXEL=1          
              IXEL = 1
              ILEV = II + IXEL
              DO K=1,4
                IAD = IADC_CRK(K,ELCRK)
                EN0 = CRKLVSET(ILEV)%ENR0(2,IAD)  ! enr initial du debut de cycle
                EN  = CRKLVSET(ILEV)%ENR0(1,IAD)  ! enr mise a jour dans le cycle
                IF (EN0 < 0 .and. EN > 0) THEN
                  IL  = ILEV+2                                            
                  EN1 = CRKLVSET(IL)%ENR0(2,IAD)                        
                  IF (EN1 > 0) THEN                                    
                    CRKAVX(ILEV)%X(1,IAD)  = CRKAVX(IL)%X(1,IAD)        
                    CRKAVX(ILEV)%X(2,IAD)  = CRKAVX(IL)%X(2,IAD)        
                    CRKAVX(ILEV)%X(3,IAD)  = CRKAVX(IL)%X(3,IAD)        
                    CRKAVX(ILEV)%V(1,IAD)  = CRKAVX(IL)%V(1,IAD)        
                    CRKAVX(ILEV)%V(2,IAD)  = CRKAVX(IL)%V(2,IAD)        
                    CRKAVX(ILEV)%V(3,IAD)  = CRKAVX(IL)%V(3,IAD)        
                    CRKAVX(ILEV)%VR(1,IAD) = CRKAVX(IL)%VR(1,IAD)       
                    CRKAVX(ILEV)%VR(2,IAD) = CRKAVX(IL)%VR(2,IAD)       
                    CRKAVX(ILEV)%VR(3,IAD) = CRKAVX(IL)%VR(3,IAD)       
                  ENDIF                                                
                ENDIF                                                
              ENDDO                                            
            END IF
c----
          END IF
        ENDDO      
      ENDDO        
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  UPXVIT_T1                     source/elements/xfem/crk_vitesse2.F
Chd|-- called by -----------
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPXVIT_T1(NEL    ,NFT     ,NXLAY  ,ELCUTG    ,
     .                     IEL_XTG,IADC_XTG)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,NXLAY
      INTEGER ELCUTG(2,*),IADC_XTG(3,*),IEL_XTG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,IG,NG,ILAY,IXEL,ILEV,IL,ICUT,ELEM,ELCRK,ELCRKTG,
     .   IAD,ITRI,EN,EN0,EN1
C=======================================================================
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO I=1,NEL
          ELEM    = I+NFT                                              
          ELCRKTG = IEL_XTG(ELEM)                                              
          ICUT    = ELCUTG(1,ELEM)                                       
          IF (ELCRKTG > 0 .and. ICUT > 0) THEN
            ELCRK = ELCRKTG + ECRKXFEC
            ITRI  = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)                 
c----
            IF (ITRI < 0) THEN    
              IXEL = 2
              ILEV = II + IXEL
              DO K=1,3
                IAD = IADC_XTG(K,ELCRKTG)
                EN0 = CRKLVSET(ILEV)%ENR0(2,IAD)  ! enr initial du debut de cycle
                EN  = CRKLVSET(ILEV)%ENR0(1,IAD)  ! enr mise a jour dans le cycle
                IF (EN0 < 0 .and. EN > 0) THEN
                  IL  = ILEV+1                                         
                  EN1 = CRKLVSET(IL)%ENR0(2,IAD)                        
                  IF (EN1 > 0) THEN                                    
                    CRKAVX(ILEV)%X(1,IAD)  = CRKAVX(IL)%X(1,IAD)        
                    CRKAVX(ILEV)%X(2,IAD)  = CRKAVX(IL)%X(2,IAD)        
                    CRKAVX(ILEV)%X(3,IAD)  = CRKAVX(IL)%X(3,IAD)        
                    CRKAVX(ILEV)%V(1,IAD)  = CRKAVX(IL)%V(1,IAD)        
                    CRKAVX(ILEV)%V(2,IAD)  = CRKAVX(IL)%V(2,IAD)        
                    CRKAVX(ILEV)%V(3,IAD)  = CRKAVX(IL)%V(3,IAD)        
                    CRKAVX(ILEV)%VR(1,IAD) = CRKAVX(IL)%VR(1,IAD)       
                    CRKAVX(ILEV)%VR(2,IAD) = CRKAVX(IL)%VR(2,IAD)       
                    CRKAVX(ILEV)%VR(3,IAD) = CRKAVX(IL)%VR(3,IAD)       
                  ENDIF                                                
                ENDIF                                                
              ENDDO
            ELSEIF (ITRI > 0) THEN              
              IXEL = 1
              ILEV = II + IXEL
              DO K=1,3
                IAD = IADC_XTG(K,ELCRKTG)
                EN0 = CRKLVSET(ILEV)%ENR0(2,IAD)  ! enr initial du debut de cycle
                EN  = CRKLVSET(ILEV)%ENR0(1,IAD)  ! enr mise a jour dans le cycle
                IF (EN0 < 0 .and. EN > 0) THEN
                  IL  = ILEV+2                                            
                  EN1 = CRKLVSET(IL)%ENR0(2,IAD)                        
                  IF (EN1 > 0) THEN                                    
                    CRKAVX(ILEV)%X(1,IAD)  = CRKAVX(IL)%X(1,IAD)        
                    CRKAVX(ILEV)%X(2,IAD)  = CRKAVX(IL)%X(2,IAD)        
                    CRKAVX(ILEV)%X(3,IAD)  = CRKAVX(IL)%X(3,IAD)        
                    CRKAVX(ILEV)%V(1,IAD)  = CRKAVX(IL)%V(1,IAD)        
                    CRKAVX(ILEV)%V(2,IAD)  = CRKAVX(IL)%V(2,IAD)        
                    CRKAVX(ILEV)%V(3,IAD)  = CRKAVX(IL)%V(3,IAD)        
                    CRKAVX(ILEV)%VR(1,IAD) = CRKAVX(IL)%VR(1,IAD)       
                    CRKAVX(ILEV)%VR(2,IAD) = CRKAVX(IL)%VR(2,IAD)       
                    CRKAVX(ILEV)%VR(3,IAD) = CRKAVX(IL)%VR(3,IAD)       
                  ENDIF                                                
                ENDIF                                                
              ENDDO                                            
            END IF
c----
          END IF
        ENDDO      
      ENDDO        
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  UPXVIT_C2                     source/elements/xfem/crk_vitesse2.F
Chd|-- called by -----------
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPXVIT_C2(NEL      ,NFT      ,NXLAY    ,IXC      ,XEDGE4N  ,
     .                     CRKEDGE  ,NODEDGE  ,IEL_CRK  ,IADC_CRK ,INOD_CRK ,
     .                     ELCUTC   ,ITAB     )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,NXLAY
      INTEGER IXC(NIXC,*),INOD_CRK(*),IADC_CRK(4,*),IEL_CRK(*),ELCUTC(2,*),
     .   NODEDGE(2,*),XEDGE4N(4,*),ITAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,KK,NSX,NN,IEL,ILAY,IXEL,ILEV,IL_SEND,COUNT,NOD1,NOD2,
     .   ICUT,ELCRK,IADS,IADR,EN,EN0,EN1,EDGE,BOUNDEDGE
C=======================================================================
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO I=1,NEL
          IEL   = I+NFT                                              
          ELCRK = IEL_CRK(IEL)
          ICUT  = ELCUTC(1,IEL)                                       
          IF (ELCRK > 0 .and. ICUT > 0) THEN
c----
            DO IXEL=1,2   ! receiver is IXEL=1 or IXEL=2
              ILEV = II + IXEL
              DO K=1,4
                KK  = IADC_CRK(K,ELCRK)
                EN0 = CRKLVSET(ILEV)%ENR0(2,KK)  ! enr initial de debut du cycle 
                EN  = CRKLVSET(ILEV)%ENR0(1,KK)  ! enr mise a jour dans le cycle 
c
                IF (EN0 <= 0 .and. EN > 0) THEN
                  NN  = IXC(K+1,IEL)        ! n  node sys std
                  NSX = INOD_CRK(NN)        ! n  node sys xfem
                  IADS    = XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,EN) ! IAD  sender
                  IL_SEND = XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,EN) ! ILEV sender 
                  IADR    = XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,EN) ! IAD  receiver
                  COUNT   = XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) 
c----
                  IF (IADS > 0 .and. IL_SEND > 0 .and. COUNT > 0.and.
     .                IADR == KK .and. IADS /= KK) THEN
!!!                    EN1 = CRKLVSET(IL_SEND)%ENR0(2,IADS)
                    EN1 = CRKLVSET(IL_SEND)%ENR0(1,IADS)
                    IF (EN1 == EN) THEN                
                      NOD1 = 0
                      NOD2 = 0
                      EDGE = XEDGE4N(K,ELCRK) ! global egdge number         
                      BOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                      IF (BOUNDEDGE == 2) THEN  ! Node N is boundary
                        NOD1 = NODEDGE(1,EDGE)
                        NOD2 = NODEDGE(2,EDGE)
                      ENDIF
                      IF (NN /= NOD1 .and. NN /= NOD2) THEN
                        CRKAVX(ILEV)%X(1,KK)  = CRKAVX(IL_SEND)%X(1,IADS)            
                        CRKAVX(ILEV)%X(2,KK)  = CRKAVX(IL_SEND)%X(2,IADS)            
                        CRKAVX(ILEV)%X(3,KK)  = CRKAVX(IL_SEND)%X(3,IADS)            
                        CRKAVX(ILEV)%V(1,KK)  = CRKAVX(IL_SEND)%V(1,IADS)            
                        CRKAVX(ILEV)%V(2,KK)  = CRKAVX(IL_SEND)%V(2,IADS)            
                        CRKAVX(ILEV)%V(3,KK)  = CRKAVX(IL_SEND)%V(3,IADS)            
                        CRKAVX(ILEV)%VR(1,KK) = CRKAVX(IL_SEND)%VR(1,IADS)           
                        CRKAVX(ILEV)%VR(2,KK) = CRKAVX(IL_SEND)%VR(2,IADS)           
                        CRKAVX(ILEV)%VR(3,KK) = CRKAVX(IL_SEND)%VR(3,IADS)        
                        COUNT = COUNT - 1                                    
                        XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) = COUNT    
c
                        IF (XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) == 0) THEN    
                          XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,EN) = 0             
                          XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,EN) = 0             
                        ENDIF                                                
                      ENDIF
                    ENDIF    
                  ENDIF 
c----
                ENDIF   
              ENDDO     
            ENDDO       
c----
          ENDIF         
        ENDDO           
      ENDDO             
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  UPXVIT_T2                     source/elements/xfem/crk_vitesse2.F
Chd|-- called by -----------
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPXVIT_T2(NEL      ,NFT      ,NXLAY    ,IXTG     ,XEDGE3N  ,
     .                     CRKEDGE  ,NODEDGE  ,IEL_XTG  ,IADC_XTG ,INOD_CRK ,
     .                     ELCUTG   ,ITAB     )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,NXLAY
      INTEGER IXTG(NIXTG,*),INOD_CRK(*),IADC_XTG(3,*),IEL_XTG(*),ELCUTG(2,*),
     .   NODEDGE(2,*),XEDGE3N(3,*),ITAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,KK,NSX,NN,IEL,ILAY,IXEL,ILEV,IL,COUNT,NOD1,NOD2,
     .   ICUT,ELCRK,ELCRKTG,IADS,IADR,EN,EN0,EN1,EDGE,BOUNDEDGE
C=======================================================================
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)    
        DO I=1,NEL
          IEL     = I+NFT                                              
          ELCRKTG = IEL_XTG(IEL)                                              
          ICUT  = ELCUTG(1,IEL)                                       
          IF (ELCRKTG > 0 .and. ICUT > 0) THEN
            ELCRK = ELCRKTG + ECRKXFEC
c----
            DO IXEL=1,2   ! receiver is IXEL=1 or IXEL=2
              ILEV = II + IXEL
              DO K=1,3
                KK  = IADC_XTG(K,ELCRKTG)
                EN0 = CRKLVSET(ILEV)%ENR0(2,KK)  ! enr initial de debut du cycle 
                EN  = CRKLVSET(ILEV)%ENR0(1,KK)  ! enr mise a jour dans le cycle 
                IF (EN0 <= 0 .and. EN > 0) THEN
                  NN  = IXTG(K+1,IEL)        ! n  node sys std
                  NSX = INOD_CRK(NN)        ! n  node sys xfem
                  IADS  = XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,EN) ! IAD  sender
                  IL    = XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,EN) ! ILEV sender 
                  IADR  = XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,EN) ! IAD  receiver
                  COUNT = XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) 
c----
                  IF (IADS > 0 .and. IL > 0 .and. COUNT > 0.and.
     .                IADR == KK .and. IADS /= KK) THEN
c                    EN1 = CRKLVSET(IL)%ENR0(2,IADS)
                    EN1 = CRKLVSET(IL)%ENR0(1,IADS)
                    IF (EN1 == EN) THEN                
                      NOD1 = 0
                      NOD2 = 0
                      EDGE = XEDGE3N(K,ELCRKTG) ! global egdge N           
                      BOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                      IF (BOUNDEDGE == 2) THEN  ! Node N is boundary
                        NOD1 = NODEDGE(1,EDGE)
                        NOD2 = NODEDGE(2,EDGE)
                      ENDIF
                      IF (NN /= NOD1 .and. NN /= NOD2) THEN
                        CRKAVX(ILEV)%X(1,KK)  = CRKAVX(IL)%X(1,IADS)            
                        CRKAVX(ILEV)%X(2,KK)  = CRKAVX(IL)%X(2,IADS)            
                        CRKAVX(ILEV)%X(3,KK)  = CRKAVX(IL)%X(3,IADS)            
                        CRKAVX(ILEV)%V(1,KK)  = CRKAVX(IL)%V(1,IADS)            
                        CRKAVX(ILEV)%V(2,KK)  = CRKAVX(IL)%V(2,IADS)            
                        CRKAVX(ILEV)%V(3,KK)  = CRKAVX(IL)%V(3,IADS)            
                        CRKAVX(ILEV)%VR(1,KK) = CRKAVX(IL)%VR(1,IADS)           
                        CRKAVX(ILEV)%VR(2,KK) = CRKAVX(IL)%VR(2,IADS)           
                        CRKAVX(ILEV)%VR(3,KK) = CRKAVX(IL)%VR(3,IADS)        
                        COUNT = COUNT - 1                                    
                        XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) = COUNT           
                        IF (XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,EN) == 0) THEN    
                          XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,EN) = 0             
                          XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,EN) = 0             
                        ENDIF                                                
                      ENDIF
                    ENDIF    
                  ENDIF 
c----
                ENDIF   
              ENDDO     
            ENDDO       
c----
          ENDIF         
        ENDDO           
      ENDDO             
c-----------
      RETURN
      END
