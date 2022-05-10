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
Chd|  DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        SHELL_LOCAL_FRAME             source/output/dynain/shell_rota.F
Chd|        SHELL_ROTA                    source/output/dynain/shell_rota.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE DYNAIN_C_STRSG(
     1                  ELBUF_TAB  ,IPARG     ,IPM        ,IGEO  ,IXC    ,
     2                  IXTG       ,WA        ,WAP0       ,IPARTC,IPARTTG,
     3                  IPART_DYNAIN,DYNAIN_INDXC,DYNAIN_INDXTG,SIZP0  ,
     4                  GEO        ,STACK     ,DRAPE_Q4   ,DRAPE_T3,X   ,
     5                  THKE       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE STACK_MOD
      USE DRAPE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTC(*), IPARTTG(*), IPART_DYNAIN(*),
     .        DYNAIN_INDXC(*), DYNAIN_INDXTG(*)
      my_real   
     .   GEO(NPROPG,*) , X(*) ,THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_Q4,DRAPE_T3
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,NPT,
     .   LLT,ITY,MLW,IH,IHBE, ID, IPRT0, IPRT,IR,IS,IT,
     .   NPG,IPG,MPT,IPT,NPTR,NPTS,NPTT,NLAY,L_PLA,ITHK,
     .   IGTYP,NPT_ALL,IL,KK(12),LARGE,IREP,IPID,IVISC,
     .   IPMAT,IXFEM,IXLAY,ISUBSTACK,IPTT,IS_WRITTEN,
     ,   LAYNPT_MAX,LAY_MAX,IERR,L_DIRA,L_DIRB,IORTH,
     .   JDIR,ILAY,J1,J2
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: PTWA, PTWA_P0
      INTEGER MATLY(MVSIZ*LAY_MAX),MAT(MVSIZ),PID(MVSIZ)
      CHARACTER*80 DELIMIT
      CHARACTER*100 LINE
      my_real
     .   SIG(6) , MOM(3),A1 ,A2
      my_real   
     .   PG,MPG,QPG(2,4),
     .   SK(2),ST(2),MK(2),MT(2),SHK(2),SHT(2),ZZ
      my_real
     .   THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX),POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX),
     .   THK_LY(MVSIZ,LAY_MAX*LAYNPT_MAX)
      my_real
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ), 
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ), 
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ), 
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   THK0(MVSIZ)
      my_real, ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB
      my_real, DIMENSION(:) ,POINTER     :: DIR_A, DIR_B
      TARGET :: DIRA,DIRB
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C-----------------------------------------------
      PARAMETER (PG = .577350269189626)
      PARAMETER (MPG=-.577350269189626)
      DATA QPG/MPG,MPG,PG,MPG,PG,PG,MPG,PG/
      DATA DELIMIT(1:48)
     ./'$--1---|---2---|---3---|---4---|---5---|---6---|'/
      DATA DELIMIT(49:80)
     ./'---7---|---8---|---9---|---10--|'/
C=======================================================================

C-----------------------
C    Allocation Tabs
C-----------------------
      ALLOCATE(PTWA(MAX(DYNAIN_NUMELC ,DYNAIN_NUMELTG)),STAT=IERR)
      ALLOCATE(PTWA_P0(0:MAX(1,DYNAIN_NUMELC_G,DYNAIN_NUMELTG_G)),STAT=IERR)
C*********************************************
C     4-NODE SHELLS
C*********************************************
      JJ = 0
C
      IE=0
      IF (DYNAIN_NUMELC.NE.0) THEN
       DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 3) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW  = IPARG(1,NG)
          NEL  = IPARG(2,NG)
          NFT  = IPARG(3,NG)
          MPT  = IPARG(6,NG)
          IHBE = IPARG(23,NG)
          ITHK = IPARG(28,NG)
          IGTYP= IPARG(38,NG)
          IXFEM   = IPARG(54,NG)
          ISUBSTACK=IPARG(71,NG)
          IXLAY = 0 ! standard element
          IPID = IXC(6,NFT+1)
          IREP = IGEO(6,IPID)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          IF (IHBE == 23) NPG=4
          LFT=1
          LLT=NEL
!
          DO I=1,12  ! lenght max (Hourglass for QEPH)
            KK(I) = NEL*(I-1)
          ENDDO
!
C
C pre counting of all NPTT (especialy for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
            NPT_ALL = 0
            DO IL=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF

          DO I=LFT,LLT
             MAT(I)=IXC(1,NFT+I)
             PID(I)=IXC(6,NFT+I)
          ENDDO

C-------------------------------------------------
C     RELATIVE POSITION OF INTEGRATION POINTS
C         POSLY between -0.5, 0.5 : need to multiply by 2 for LSDYNA
C------------------------------------------------
          IF (ITHK >0 ) THEN
             THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
          ELSE
             THK0(LFT:LLT) = THKE(LFT:LLT)
          END IF
          CALL LAYINI(
     .           ELBUF_TAB(NG),LFT      ,LLT        ,GEO        ,IGEO      ,
     .           MAT        ,PID        ,THKLY      ,MATLY      ,POSLY     ,
     .           IGTYP      ,IXFEM      ,IXLAY      ,NLAY       ,NPT       ,
     .           ISUBSTACK  ,STACK      ,DRAPE_Q4   ,NFT        ,THK0      , 
     .           NEL        ,THK_LY )

C-------------------------------------------------------
C     ELEMENT LOCAL FRAME : for rotation local -> Global
C-------------------------------------------------------
           CALL SHELL_LOCAL_FRAME(
     1                     LFT    , LLT   ,ITY      ,IHBE     ,IGTYP   ,
     2                     IXC    ,IXTG   ,NFT      ,X        ,GBUF%OFF,
     3                     RX     ,RY     ,RZ       ,SX       ,SY      ,  
     4                     SZ     ,E1X    ,E2X      ,E3X      ,E1Y     , 
     5                     E2Y    ,E3Y    ,E1Z      ,E2Z      ,E3Z     )

C-------------------------------------------------------
C     ORTHOTROPY DIRECTION : for rotation orthotropic -> local 
C-------------------------------------------------------

          L_DIRA = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRA
          L_DIRB = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRB
          ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
          ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
          DIRA=ZERO
          DIRB=ZERO
          IF (L_DIRA == 0) THEN
              CONTINUE
          ELSEIF (IREP == 0) THEN
             DO J=1,NLAY
                J1 = 1+(J-1)*L_DIRA*NEL
                J2 = J*L_DIRA*NEL
                DIRA(J1:J2) = ELBUF_TAB(NG)%BUFLY(J)%DIRA(1:NEL*L_DIRA)
            ENDDO
          ENDIF
          DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
          DIR_B => DIRB(1:NLAY*NEL*L_DIRB)

          CALL CORTDIR3(ELBUF_TAB(NG),DIR_A  ,DIR_B  ,LFT    ,LLT    ,
     .                      NLAY     ,IREP   ,RX     ,RY     ,RZ     ,
     .                      SX       ,SY     ,SZ     ,E1X    ,E1Y    ,
     .                      E1Z      ,E2X    ,E2Y    ,E2Z    ,NEL    )

          IORTH = 0
          IF (IGTYP /= 1) THEN                                                     
             IORTH = 1
          ENDIF
C-------------------------------------------------------
C     Viscosity stress add
C-------------------------------------------------------

          IVISC = 0
          IF (IGTYP == 11) THEN                                
              IPMAT = 100                                     
              DO N=1,MPT                                     
                 DO I=LFT,LLT
                    IF (IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1       
                 ENDDO                                       
              ENDDO                                          
          ELSEIF (IGTYP == 9 .OR. IGTYP == 10) THEN            
              DO N=1,MPT
                  DO I=LFT,LLT
                    IF (IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1         
                 ENDDO                                      
              ENDDO                                         
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP == 52)THEN                             
                  IPMAT   = 2 + NLAY
              DO N=1,NLAY                                      
                  DO I=LFT,LLT
                    IF(IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1         
                 END DO                                      
               ENDDO                                          
          ENDIF   ! IGTYP    

C-------------------------------------------------------
C-       Loop over 4 node shell elements
C-------------------------------------------------------
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTC(N)
            IF (IPART_DYNAIN(IPRT)==0) CYCLE
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%OFF(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            WA(JJ) = IXC(NIXC,N)
            JJ = JJ + 1
            IF (MPT == 0) THEN  ! global integration
               WA(JJ) = 3 ! Membrane - Lower - Upper
            ELSE 
               WA(JJ) = MPT  ! Integration points
            ENDIF
            JJ = JJ + 1
            WA(JJ) = NPG  ! Gauss points
            JJ = JJ + 1
            WA(JJ) = UN  ! LARGE

c---------

           IF (MPT == 0) THEN  ! global integration
               IF (MLW == 0 .or. MLW == 13) THEN
                  DO IPG=1,NPG 
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                    DO J=1,8        ! forces            	
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO
               ELSEIF (NPG == 1) THEN

! LOWER
                  A1 = UN
                  A2  = -SIX

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = -UN
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)         
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)              
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6) 
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF    
! Membrane
                  A1 = UN
                  A2  = ZERO

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = ZERO
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6)           
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF   
! Upper

                  A1 = UN
                  A2  = SIX

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = -UN
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6)           
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                       
            
              ELSE  ! NPG > 1
       
                  IF(IHBE ==23) THEN !IHBE = 23   (QEPH)

                    ST(1) = GBUF%HOURG(KK(1)+I) 
                    ST(2) =-GBUF%HOURG(KK(2)+I) 
                    MT(1) = GBUF%HOURG(KK(3)+I) 
                    MT(2) =-GBUF%HOURG(KK(4)+I) 
                    SK(1) =-GBUF%HOURG(KK(7)+I) 
                    SK(2) = GBUF%HOURG(KK(8)+I) 
                    MK(1) =-GBUF%HOURG(KK(9)+I) 
                    MK(2) = GBUF%HOURG(KK(10)+I) 
                    SHT(1)= GBUF%HOURG(KK(5)+I) 
                    SHT(2)=-GBUF%HOURG(KK(6)+I) 
                    SHK(1)=-GBUF%HOURG(KK(11)+I)
                    SHK(2)= GBUF%HOURG(KK(12)+I)

! LOWER
                   A1 = UN
                   A2  = -SIX

                   DO IPG=1,NPG

                      SIG(1) = GBUF%FOR(KK(1)+I)
     .	    	              + ST(1)*QPG(2,IPG)+SK(1)*QPG(1,IPG)
                      SIG(2) = GBUF%FOR(KK(2)+I)
     .	    	              + ST(2)*QPG(2,IPG)+SK(2)*QPG(1,IPG)
                      SIG(3) = GBUF%FOR(KK(3)+I)
                      SIG(4) = GBUF%FOR(KK(4)+I)
     .	    	             + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                      SIG(5) = GBUF%FOR(KK(5)+I)
     .	    	              + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)

                      MOM(1) = GBUF%MOM(KK(1)+I)
     .	    	            + MT(1)*QPG(2,IPG)+MK(1)*QPG(1,IPG)
                      MOM(2) = GBUF%MOM(KK(2)+I)
     .	    	            + MT(2)*QPG(2,IPG)+MK(2)*QPG(1,IPG)
                      MOM(3) = GBUF%MOM(KK(3)+I)
                      SIG(1) = A1*SIG(1) + A2*MOM(1) 
                      SIG(2) = A1*SIG(2) + A2*MOM(2)    
                      SIG(3) = A1*SIG(3) + A2*MOM(3)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                       JJ = JJ + 1
                       WA(JJ) = -UN
                       JJ = JJ + 1                                      
                       WA(JJ) = SIG(1)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(2)           
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(3)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(4)             
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(5)   
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(6)   
c
                       JJ = JJ + 1
                       IF (GBUF%G_PLA > 0) THEN    
            	          WA(JJ) = GBUF%PLA(I)
                       ELSE                         
                          WA(JJ) = ZERO              
                       ENDIF 

                   ENDDO

! MEMBRANE
                   A1 = UN
                   A2  = ZERO

                   DO IPG=1,NPG

                      SIG(1) = GBUF%FOR(KK(1)+I)
     .	    	              + ST(1)*QPG(2,IPG)+SK(1)*QPG(1,IPG)
                      SIG(2) = GBUF%FOR(KK(2)+I)
     .	    	              + ST(2)*QPG(2,IPG)+SK(1)*QPG(1,IPG)
                      SIG(3) = GBUF%FOR(KK(3)+I)
                      SIG(4) = GBUF%FOR(KK(4)+I)
     .	    	             + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                      SIG(5) = GBUF%FOR(KK(5)+I)
     .	    	              + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)

                      MOM(1) = GBUF%MOM(KK(1)+I)
     .	    	            + MT(1)*QPG(2,IPG)+MK(1)*QPG(1,IPG)
                      MOM(2) = GBUF%MOM(KK(2)+I)
     .	    	            + MT(2)*QPG(2,IPG)+MK(2)*QPG(1,IPG)
                      MOM(3) = GBUF%MOM(KK(3)+I)
                      SIG(1) = A1*SIG(1) + A2*MOM(1) 
                      SIG(2) = A1*SIG(2) + A2*MOM(2)    
                      SIG(3) = A1*SIG(3) + A2*MOM(3)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                       JJ = JJ + 1
                       WA(JJ) = ZERO
                       JJ = JJ + 1                                      
                       WA(JJ) = SIG(1)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(2)           
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(3)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(4)             
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(5)   
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(6)   
c
                       JJ = JJ + 1
                       IF (GBUF%G_PLA > 0) THEN    
            	          WA(JJ) = GBUF%PLA(I)
                       ELSE                         
                          WA(JJ) = ZERO              
                       ENDIF 

                   ENDDO  
! Upper
                   A1 = UN
                   A2  = SIX

                   DO IPG=1,NPG

                      SIG(1) = GBUF%FOR(KK(1)+I)
     .	    	              + ST(1)*QPG(2,IPG)+SK(1)*QPG(1,IPG)
                      SIG(2) = GBUF%FOR(KK(2)+I)
     .	    	              + ST(2)*QPG(2,IPG)+SK(2)*QPG(1,IPG)
                      SIG(3) = GBUF%FOR(KK(3)+I)
                      SIG(4) = GBUF%FOR(KK(4)+I)
     .	    	             + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)
                      SIG(5) = GBUF%FOR(KK(5)+I)
     .	    	              + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)

                      MOM(1) = GBUF%MOM(KK(1)+I)
     .	    	            + MT(1)*QPG(2,IPG)+MK(1)*QPG(1,IPG)
                      MOM(2) = GBUF%MOM(KK(2)+I)
     .	    	            + MT(2)*QPG(2,IPG)+MK(2)*QPG(1,IPG)
                      MOM(3) = GBUF%MOM(KK(3)+I)
                      SIG(1) = A1*SIG(1) + A2*MOM(1) 
                      SIG(2) = A1*SIG(2) + A2*MOM(2)    
                      SIG(3) = A1*SIG(3) + A2*MOM(3)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                       JJ = JJ + 1
                       WA(JJ) = UN
                       JJ = JJ + 1                                      
                       WA(JJ) = SIG(1)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(2)           
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(3)            
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(4)             
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(5)   
                       JJ = JJ + 1                                
                       WA(JJ) = SIG(6)   
c
                       JJ = JJ + 1
                       IF (GBUF%G_PLA > 0) THEN    
            	          WA(JJ) = GBUF%PLA(I)
                       ELSE                         
                          WA(JJ) = ZERO              
                       ENDIF 

                   ENDDO 

                 ELSE ! IHBE = 23
! LOWER
                    A1 = UN
                    A2  = -SIX

                    DO IR=1,NPTR                                                         
                       DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                          IPG = NPTR*(IS-1) + IR
                          K = (IPG-1)*NEL*5

                         SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                         SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                         SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                         SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                         SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                         SIG(6) = ZERO  

                         IORTH = 0
                         ILAY = 0
                         JDIR = 0

                         CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                         JJ = JJ + 1
                         WA(JJ) = -UN
                         JJ = JJ + 1                                      
                         WA(JJ) = SIG(1)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(2)           
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(3)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(4)             
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(5)   
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(6)   
c
                         JJ = JJ + 1
                         IF (GBUF%G_PLA > 0) THEN    
            	           WA(JJ) = GBUF%PLA(I)
                         ELSE                         
                           WA(JJ) = ZERO              
                         ENDIF 
                       ENDDO
                    ENDDO    

! MEMBRANE
                    A1 = UN
                    A2  = ZERO

                    DO IR=1,NPTR                                                         
                       DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                          IPG = NPTR*(IS-1) + IR
                          K = (IPG-1)*NEL*5

                         SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                         SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                         SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                         SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                         SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                         SIG(6) = ZERO  

                         IORTH = 0
                         ILAY = 0
                         JDIR = 0

                         CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                         JJ = JJ + 1
                         WA(JJ) = ZERO
                         JJ = JJ + 1                                      
                         WA(JJ) = SIG(1)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(2)           
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(3)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(4)             
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(5)   
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(6)   
c
                         JJ = JJ + 1
                         IF (GBUF%G_PLA > 0) THEN    
            	           WA(JJ) = GBUF%PLA(I)
                         ELSE                         
                           WA(JJ) = ZERO              
                         ENDIF 
                       ENDDO
                    ENDDO    
! Upper
                    A1 = UN
                    A2  = SIX

                    DO IR=1,NPTR                                                         
                       DO IS=1,NPTS 
                          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                          IPG = NPTR*(IS-1) + IR
                          K = (IPG-1)*NEL*5

                         SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                         SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                         SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                         SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                         SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                         SIG(6) = ZERO  

                         IORTH = 0
                         ILAY = 0
                         JDIR = 0

                         CALL SHELL_ROTA(
     1                        I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                        IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                        RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                        SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                        E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                        DIR_A    ,DIR_B    )

                         JJ = JJ + 1
                         WA(JJ) = UN
                         JJ = JJ + 1                                      
                         WA(JJ) = SIG(1)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(2)           
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(3)            
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(4)             
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(5)   
                         JJ = JJ + 1                                
                         WA(JJ) = SIG(6)   
c
                         JJ = JJ + 1
                         IF (GBUF%G_PLA > 0) THEN    
            	           WA(JJ) = GBUF%PLA(I)
                         ELSE                         
                           WA(JJ) = ZERO              
                         ENDIF 
                       ENDDO
                    ENDDO    

                 ENDIF ! IHBE = 23

              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
C-------------------------
C           (MPT /=0 ):
C-------------------------
           ELSEIF (MLW == 0 .or. MLW == 13) THEN

              DO K=1,MPT
                  DO IPG=1,NPG
                    DO J=1,8     ! Stress + pos + plastic strain
                      JJ = JJ + 1
                      WA(JJ) = ZERO
                    ENDDO                                                               
                  ENDDO                                                                 
               ENDDO

           ELSEIF(IHBE == 23) THEN

               ST(1) = GBUF%HOURG(KK(1)+I) 
               ST(2) =-GBUF%HOURG(KK(2)+I) 
               MT(1) = GBUF%HOURG(KK(3)+I) 
               MT(2) =-GBUF%HOURG(KK(4)+I) 
               SK(1) =-GBUF%HOURG(KK(7)+I) 
               SK(2) = GBUF%HOURG(KK(8)+I) 
               MK(1) =-GBUF%HOURG(KK(9)+I) 
               MK(2) = GBUF%HOURG(KK(10)+I) 
               SHT(1)= GBUF%HOURG(KK(5)+I) 
               SHT(2)=-GBUF%HOURG(KK(6)+I) 
               SHK(1)=-GBUF%HOURG(KK(11)+I)
               SHK(2)= GBUF%HOURG(KK(12)+I)

               IPTT = 0
               DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT

                  JDIR = 1 + (IL-1)*NEL*2

                  DO IT=1,NPTT
                    IPT = IPTT + IT        
                    LBUF => BUFLY%LBUF(1,1,IT)
                    L_PLA = BUFLY%L_PLA
                    ZZ = POSLY(I,IPT)*THK0(I)
                    DO IPG=1,NPG

                      SIG(1) = LBUF%SIG(KK(1)+I)
     .                       + (ST(1)+ZZ*MT(1))*QPG(2,IPG)              
     .                       + (SK(1)+ZZ*MK(1))*QPG(1,IPG)  			
                      SIG(2) = LBUF%SIG(KK(2)+I)
     .                       + (ST(2)+ZZ*MT(2))*QPG(2,IPG)              
     .                       + (SK(2)+ZZ*MK(2))*QPG(1,IPG)  			
                      SIG(3) = LBUF%SIG(KK(3)+I)			
                      SIG(4) = LBUF%SIG(KK(4)+I)
     .	    	             + SHT(2)*QPG(2,IPG)+SHK(2)*QPG(1,IPG)			
                      SIG(5) = LBUF%SIG(KK(5)+I)
     .	    	             + SHT(1)*QPG(2,IPG)+SHK(1)*QPG(1,IPG)
                      SIG(6) = ZERO 
 
                      CALL SHELL_ROTA(
     1                      I        ,IL      ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                      JJ = JJ + 1
                      WA(JJ) = DEUX * POSLY(I,IPT)

                      JJ = JJ + 1
                      WA(JJ) = SIG(1)
                      JJ = JJ + 1
                      WA(JJ) = SIG(2)
                      JJ = JJ + 1
                      WA(JJ) = SIG(3)
                      JJ = JJ + 1
                      WA(JJ) = SIG(4)
                      JJ = JJ + 1
                      WA(JJ) = SIG(5)
                      JJ = JJ + 1
                      WA(JJ) = SIG(6)
                      JJ = JJ + 1
                      IF (BUFLY%L_PLA > 0) THEN
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                     ENDIF

                    ENDDO  !  DO IPG=1,NPG      
                  ENDDO  !  DO IT=1,NPTT
                  IPTT= IPTT + NPTT

               ENDDO  !  DO IL=1,NLAY    


           ELSEIF (NLAY == 1) THEN    ! PID1

               BUFLY => ELBUF_TAB(NG)%BUFLY(1)
               NPTT = BUFLY%NPTT

               DO IT=1,NPTT
                  DO IS=1,NPTS 
                    DO IR=1,NPTR                                                         
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      IPG = NPTR*(IS-1) + IR
                      SIG(1) = LBUF%SIG(KK(1)+I)			
                      SIG(2) = LBUF%SIG(KK(2)+I)			
                      SIG(3) = LBUF%SIG(KK(3)+I)			
                      SIG(4) = LBUF%SIG(KK(4)+I)			
                      SIG(5) = LBUF%SIG(KK(5)+I)
                      SIG(6) = ZERO    
C
C                 viscous stress added 
C 
                     IF (IVISC > 0) THEN

                      SIG(1) = SIG(1) + LBUF%VISC(KK(1)+I)			
                      SIG(2) = SIG(2) + LBUF%VISC(KK(2)+I)			
                      SIG(3) = SIG(3) + LBUF%VISC(KK(3)+I)			
                      SIG(4) = SIG(4) + LBUF%VISC(KK(4)+I)			
                      SIG(5) = SIG(5) + LBUF%VISC(KK(5)+I)

                     ENDIF  !  IF (IVISC > 0)

                     ILAY = 1
                     JDIR = 1 + (ILAY-1)*LLT*2

                     CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )	

					

                     JJ = JJ + 1
                     WA(JJ) = DEUX * POSLY(I,IT)

                     JJ = JJ + 1
                     WA(JJ) = SIG(1)
                     JJ = JJ + 1
                     WA(JJ) = SIG(2)
                     JJ = JJ + 1
                     WA(JJ) = SIG(3)
                     JJ = JJ + 1
                     WA(JJ) = SIG(4)
                     JJ = JJ + 1
                     WA(JJ) = SIG(5)
                     JJ = JJ + 1
                     WA(JJ) = SIG(6)
                     JJ = JJ + 1
                     IF (BUFLY%L_PLA > 0) THEN
            	        WA(JJ) = LBUF%PLA(I)
                     ELSE
                        WA(JJ) = ZERO
                     ENDIF

                    ENDDO      
                  ENDDO	    
                ENDDO  !  DO IPT = 1,NPTT


           ELSE  !  NLAY > 1, PID10,PID11,PID16,PID17,PID51,PID52

                IPTT = 0
                DO IL = 1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT

                  JDIR = 1 + (IL-1)*LLT*2

                  DO IT=1,NPTT
                    IPT = IPTT + IT
                    DO IS=1,NPTS 
                      DO IR=1,NPTR                                                         
                        LBUF => BUFLY%LBUF(IR,IS,IT)

                        SIG(1) = LBUF%SIG(KK(1)+I)			
                        SIG(2) = LBUF%SIG(KK(2)+I)			
                        SIG(3) = LBUF%SIG(KK(3)+I)			
                        SIG(4) = LBUF%SIG(KK(4)+I)			
                        SIG(5) = LBUF%SIG(KK(5)+I)
                        SIG(6) = ZERO   
C
C                 viscous stress added 
C 
                        IF (IVISC > 0) THEN
                           SIG(1) = SIG(1) + LBUF%VISC(KK(1)+I)			
                           SIG(2) = SIG(2) + LBUF%VISC(KK(2)+I)			
                           SIG(3) = SIG(3) + LBUF%VISC(KK(3)+I)			
                           SIG(4) = SIG(4) + LBUF%VISC(KK(4)+I)			
                           SIG(5) = SIG(5) + LBUF%VISC(KK(5)+I)
                        ENDIF  !  IF (IVISC > 0) 

                        CALL SHELL_ROTA(
     1                      I        ,IL      ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )	
		
                        JJ = JJ + 1
                        WA(JJ) = DEUX * POSLY(I,IPT)

                        JJ = JJ + 1
                        WA(JJ) = SIG(1)
                        JJ = JJ + 1
                        WA(JJ) = SIG(2)
                        JJ = JJ + 1
                        WA(JJ) = SIG(3)
                        JJ = JJ + 1
                        WA(JJ) = SIG(4)
                        JJ = JJ + 1
                        WA(JJ) = SIG(5)
                        JJ = JJ + 1
                        WA(JJ) = SIG(6)
                        JJ = JJ + 1
                        IF (BUFLY%L_PLA > 0) THEN
            	          WA(JJ) = LBUF%PLA(I)
                        ELSE
                          WA(JJ) = ZERO
                        ENDIF
                      ENDDO
                    ENDDO	    
                  ENDDO	
                  IPTT = IPTT + NPTT
                ENDDO
              ENDIF    ! MPT, NLAY
C
            IE=IE+1
C           Pointer last position of shell IE in WA
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT

          IF (ALLOCATED(DIRB))     DEALLOCATE(DIRB)                                                          
          IF (ALLOCATED(DIRA))     DEALLOCATE(DIRA)   

c------- end loop over 4 node shell elements
        ENDIF ! ITY == 3
       ENDDO   ! NG = 1, NGROUP
      ENDIF  ! DYNAIN_NUMEL /= 0


c-----------------------------------------------------------------------
c     4N SHELLS - WRITE
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
Clm     recopying for code simplification
        PTWA_P0(0)=0
        DO N=1,DYNAIN_NUMELC
          PTWA_P0(N)=PTWA(N)
        ENDDO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
      ELSE
C      Global pointers WAP0
        CALL SPMD_STAT_PGATHER(PTWA,DYNAIN_NUMELC,PTWA_P0,DYNAIN_NUMELC_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
c-------------------------------------
      IS_WRITTEN = 0
      IF (ISPMD == 0.AND.LEN > 0) THEN
          WRITE(IUDYNAIN,'(A)') DELIMIT
          WRITE(IUDYNAIN,'(A)')'*INITIAL_STRESS_SHELL'
          WRITE(IUDYNAIN,'(A)')
     .    '$  SHELLID       NPG     NBINT                         LARGE  ' 
          WRITE(IUDYNAIN,'(A)')
     .     '$ IF(NPT == 0), REPEAT I=1,NPG :'
          WRITE(IUDYNAIN,'(A)')
     .    '$ IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'      
           WRITE(IUDYNAIN,'(A)')
     .     '$         T    SIGXX     SIGYY     SIGZZ     SIGXY     SIGYZ     SIGZX       EPSP '
            WRITE(IUDYNAIN,'(A)') DELIMIT
         IS_WRITTEN = 1
        DO N=1,DYNAIN_NUMELC_G
C         Retrieving shell ID in increasing order 
          K=DYNAIN_INDXC(N)
C         Adress in  WAP0
          J=PTWA_P0(K-1)

          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
c
            ID  = NINT(WAP0(J + 2)) 
            NPT = NINT(WAP0(J + 3)) 
            NPG = NINT(WAP0(J + 4)) 
            LARGE = NINT(WAP0(J + 5)) 

            J = J + 5

            WRITE(IUDYNAIN,'(3I10,20X,I10)')ID,NPG,NPT,LARGE
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(JJ + K),K=1,5)
                WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(JJ + K),K=6,8)
                J = J + 8
              ENDDO
            ELSE
              DO IPT=1,NPT 
                DO IPG=1,NPG

                   WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(J + K),K=1,5)
                   WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(J + K),K=6,8)
                   J = J + 8
                ENDDO
              ENDDO

            ENDIF  !  IF (NPT == 0)
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,DYNAIN_NUMELC_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)

C***********************************************
C     3-NODE SHELLS
C***********************************************
      JJ = 0
      IE=0
C
      IF(DYNAIN_NUMELTG.NE.0) THEN
       DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 7) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW  = IPARG(1,NG)
          NEL  = IPARG(2,NG)
          NFT  = IPARG(3,NG)
          MPT  = IPARG(6,NG)
          IHBE = IPARG(23,NG)
          ITHK = IPARG(28,NG)
          IGTYP= IPARG(38,NG)
          IPID = IXTG(5,NFT+1)
          IREP = IGEO(6,IPID)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          LFT=1
          LLT=NEL
!
          DO I=1,6
            KK(I) = NEL*(I-1)
          ENDDO
!
C
C pre counting of all NPTT (especialy for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO K=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(K)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF

          DO I=LFT,LLT
             MAT(I)=IXTG(1,NFT+I)
             PID(I)=IXTG(5,NFT+I)
          ENDDO
C-------------------------------------------------
C     RELATIVE POSITION OF INTEGRATION POINTS
C         POSLY between -0.5, 0.5 : need to multiply by 2 for LSDYNA
C------------------------------------------------
          IF (ITHK >0 ) THEN
             THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
          ELSE
             THK0(LFT:LLT) = THKE(LFT:LLT)
          END IF

          CALL LAYINI(
     .           ELBUF_TAB(NG),LFT      ,LLT        ,GEO        ,IGEO      ,
     .           MAT        ,PID        ,THKLY      ,MATLY      ,POSLY     ,
     .           IGTYP      ,IXFEM      ,IXLAY      ,NLAY       ,NPT       ,
     .           ISUBSTACK  ,STACK      ,DRAPE_Q4   ,NFT        ,THK0      , 
     .           NEL        ,THK_LY )

C-------------------------------------------------------
C     ELEMENT LOCAL FRAME : for rotation local -> Global
C-------------------------------------------------------
           CALL SHELL_LOCAL_FRAME(
     1                     LFT    , LLT   ,ITY      ,IHBE     ,IGTYP   ,
     2                     IXC    ,IXTG   ,NFT      ,X        ,GBUF%OFF,
     3                     RX     ,RY     ,RZ       ,SX       ,SY      ,  
     4                     SZ     ,E1X    ,E2X      ,E3X      ,E1Y     , 
     5                     E2Y    ,E3Y    ,E1Z      ,E2Z      ,E3Z     )

C-------------------------------------------------------
C     ORTHOTROPY DIRECTION : for rotation orthotropic -> local 
C-------------------------------------------------------

          L_DIRA = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRA
          L_DIRB = ELBUF_TAB(NG)%BUFLY(1)%LY_DIRB
          ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
          ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
          DIRA=ZERO
          DIRB=ZERO
          IF (L_DIRA == 0) THEN
              CONTINUE
          ELSEIF (IREP == 0) THEN
             DO J=1,NLAY
                J1 = 1+(J-1)*L_DIRA*NEL
                J2 = J*L_DIRA*NEL
                DIRA(J1:J2) = ELBUF_TAB(NG)%BUFLY(J)%DIRA(1:NEL*L_DIRA)
            ENDDO
          ENDIF
          DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
          DIR_B => DIRB(1:NLAY*NEL*L_DIRB)

          CALL CORTDIR3(ELBUF_TAB(NG),DIR_A  ,DIR_B  ,LFT    ,LLT    ,
     .                      NLAY     ,IREP   ,RX     ,RY     ,RZ     ,
     .                      SX       ,SY     ,SZ     ,E1X    ,E1Y    ,
     .                      E1Z      ,E2X    ,E2Y    ,E2Z    ,NEL    )

          IORTH = 1
C-------------------------------------------------------
C     Viscosity stress add
C-------------------------------------------------------

          IVISC = 0
          IF (IGTYP == 11) THEN                                
              IPMAT = 100                                     
              DO N=1,MPT                                     
                 DO I=LFT,LLT
                    IF (IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1       
                 ENDDO                                       
              ENDDO                                          
          ELSEIF (IGTYP == 9 .OR. IGTYP == 10) THEN            
              DO N=1,MPT
                  DO I=LFT,LLT
                    IF (IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1         
                 ENDDO                                      
              ENDDO                                         
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP == 52)THEN                             
                  IPMAT   = 2 + NLAY
              DO N=1,NLAY                                      
                  DO I=LFT,LLT
                    IF(IPM(222,MATLY((N-1)*LLT + I)) > 0 ) IVISC = 1         
                 END DO                                      
               ENDDO                                          
          ENDIF   ! IGTYP   

C-------------------------------------------------------
C-       Loop over 3 node shell elements
C-------------------------------------------------------

          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTTG(N)
            IF (IPART_DYNAIN(IPRT) == 0) CYCLE
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%OFF(I)
            ELSE
              WA(JJ) = ZERO
            ENDIF
            JJ = JJ + 1
            WA(JJ) = IXTG(NIXTG,N)
            JJ = JJ + 1
            IF (MPT == 0) THEN  ! global integration
               WA(JJ) = 3 ! Membrane - Lower - Upper
            ELSE 
               WA(JJ) = MPT  ! Integration points
            ENDIF
            JJ = JJ + 1
            WA(JJ) = NPG
            JJ = JJ + 1
            WA(JJ) = UN  ! LARGE
c----          
            IF (MPT == 0) THEN  ! global integration
              IF (MLW == 0 .or. MLW == 13) THEN
                DO IPG=1,NPG 
                  DO J=1,9                                     
                    JJ = JJ + 1                                
                    WA(JJ) = ZERO                              
                  ENDDO                                        
                ENDDO                                          
              ELSEIF (NPG == 1) THEN

! LOWER
                 A1 = UN
                 A2  = -SIX

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = -UN
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)         
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)              
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6) 
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF    
! Membrane
                  A1 = UN
                  A2  = ZERO

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = ZERO
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6)           
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF   
! Upper

                  A1 = UN
                  A2  = SIX

                  SIG(1) = A1*GBUF%FOR(KK(1)+I) + A2* GBUF%MOM(KK(1)+I)
                  SIG(2) = A1*GBUF%FOR(KK(2)+I) + A2* GBUF%MOM(KK(2)+I)
                  SIG(3) = A1*GBUF%FOR(KK(3)+I) + A2* GBUF%MOM(KK(3)+I)
                  SIG(4) = GBUF%FOR(KK(4)+I) 
                  SIG(5) = GBUF%FOR(KK(5)+I)  
                  SIG(6) = ZERO    
        
                  IORTH = 0
                  ILAY = 0
                  JDIR = 0

                  CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )

                  JJ = JJ + 1
                  WA(JJ) = UN
                  JJ = JJ + 1                                      
                  WA(JJ) = SIG(1)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(2)          
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(3)            
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(4)             
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(5)    
                  JJ = JJ + 1                                
                  WA(JJ) = SIG(6)           
c
                  JJ = JJ + 1                                      
                  IF (GBUF%G_PLA > 0) THEN    
            	    WA(JJ) = GBUF%PLA(I)       
                  ELSE                         
                    WA(JJ) = ZERO              
                  ENDIF                 
              ELSE                                              
! LOWER
                  A1 = UN
                  A2  = -SIX

                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5

                      SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                      SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                      SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                      SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                      SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )		

                      JJ = JJ + 1
                      WA(JJ) = -UN
                      JJ = JJ + 1                                      
                      WA(JJ) = SIG(1)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(2)           
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(3)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(4)             
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(5)   
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(6)   
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF 
                    ENDDO
                  ENDDO    

! MEMBRANE
                  A1 = UN
                  A2  = ZERO

                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5
                      SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                      SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                      SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                      SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                      SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )		

                      JJ = JJ + 1
                      WA(JJ) = ZERO
                      JJ = JJ + 1                                      
                      WA(JJ) = SIG(1)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(2)           
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(3)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(4)             
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(5)   
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(6)   
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF    
                    ENDDO
                  ENDDO  

! Upper
                  A1 = UN
                  A2  = SIX
                  DO IR=1,NPTR                                                         
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
                      IPG = NPTR*(IS-1) + IR
                      K = (IPG-1)*NEL*5

                      SIG(1) = A1*GBUF%FORPG(K + KK(1) + I) + A2*GBUF%MOMPG(K + KK(1) + I)   
                      SIG(2) = A1*GBUF%FORPG(K + KK(2) + I) + A2*GBUF%MOMPG(K + KK(2) + I)     
                      SIG(3) = A1*GBUF%FORPG(K + KK(3) + I)  + A2*GBUF%MOMPG(K + KK(3) + I)    
                      SIG(4) = GBUF%FORPG(K + KK(4) + I) 
                      SIG(5) = GBUF%FORPG(K + KK(5) + I)   
                      SIG(6) = ZERO  

                      IORTH = 0
                      ILAY = 0
                      JDIR = 0

                      CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )		

                      JJ = JJ + 1
                      WA(JJ) = UN
                      JJ = JJ + 1                                      
                      WA(JJ) = SIG(1)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(2)           
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(3)            
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(4)             
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(5)   
                      JJ = JJ + 1                                
                      WA(JJ) = SIG(6)   
c
                      JJ = JJ + 1
                      IF (GBUF%G_PLA > 0) THEN    
            	        WA(JJ) = LBUF%PLA(I)
                      ELSE                         
                        WA(JJ) = ZERO              
                      ENDIF                              
                    ENDDO                                                               
                  ENDDO     
                                 
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ELSE ! MPT > 0
              IF (MLW == 0 .or. MLW == 13) THEN
                DO IPG=1,NPG
                  DO J=1,8
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                  ENDDO
                ENDDO
              ELSEIF (NLAY == 1) THEN    ! PID1

               BUFLY => ELBUF_TAB(NG)%BUFLY(1)
               NPTT = BUFLY%NPTT

               DO IT=1,NPTT
                  DO IS=1,NPTS 
                    DO IR=1,NPTR                                                         
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      IPG = NPTR*(IS-1) + IR
                      SIG(1) = LBUF%SIG(KK(1)+I)			
                      SIG(2) = LBUF%SIG(KK(2)+I)			
                      SIG(3) = LBUF%SIG(KK(3)+I)			
                      SIG(4) = LBUF%SIG(KK(4)+I)			
                      SIG(5) = LBUF%SIG(KK(5)+I)
                      SIG(6) = ZERO    
C
C                 viscous stress added for animation
C 
                     IF (IVISC > 0) THEN

                      SIG(1) = SIG(1) + LBUF%VISC(KK(1)+I)			
                      SIG(2) = SIG(2) + LBUF%VISC(KK(2)+I)			
                      SIG(3) = SIG(3) + LBUF%VISC(KK(3)+I)			
                      SIG(4) = SIG(4) + LBUF%VISC(KK(4)+I)			
                      SIG(5) = SIG(5) + LBUF%VISC(KK(5)+I)

                     ENDIF  !  IF (IVISC > 0)

                     ILAY = 1
                     JDIR = 1 + (ILAY-1)*LLT*2

                     CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )			

                      JJ = JJ + 1
                      WA(JJ) = DEUX * POSLY(I,IT)
                      JJ = JJ + 1
                      WA(JJ) = SIG(1)
                      JJ = JJ + 1
                      WA(JJ) = SIG(2)
                      JJ = JJ + 1
                      WA(JJ) = SIG(3)
                      JJ = JJ + 1
                      WA(JJ) = SIG(4)
                      JJ = JJ + 1
                      WA(JJ) = SIG(5)
                      JJ = JJ + 1
                      WA(JJ) = SIG(6)
            	      JJ = JJ + 1
                      IF (BUFLY%L_PLA > 0) THEN
                        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                      ENDIF
                    ENDDO      
                  ENDDO	    
                ENDDO  !  DO IPT = 1,NPTT

              ELSE
                IPTT = 0
                DO IL=1,NLAY
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  DO IT=1,NPTT
                    DO IPG=1,NPG
                      LBUF => BUFLY%LBUF(IPG,1,IT)
                      L_PLA = BUFLY%L_PLA

                      SIG(1) = LBUF%SIG(KK(1)+I)			
                      SIG(2) = LBUF%SIG(KK(2)+I)			
                      SIG(3) = LBUF%SIG(KK(3)+I)			
                      SIG(4) = LBUF%SIG(KK(4)+I)			
                      SIG(5) = LBUF%SIG(KK(5)+I)
                      SIG(6) = ZERO    
C
C                 viscous stress added for animation
C 
                     IF (IVISC > 0) THEN

                      SIG(1) = SIG(1) + LBUF%VISC(KK(1)+I)			
                      SIG(2) = SIG(2) + LBUF%VISC(KK(2)+I)			
                      SIG(3) = SIG(3) + LBUF%VISC(KK(3)+I)			
                      SIG(4) = SIG(4) + LBUF%VISC(KK(4)+I)			
                      SIG(5) = SIG(5) + LBUF%VISC(KK(5)+I)

                     ENDIF  !  IF (IVISC > 0)

                     ILAY = 0
                     JDIR = 1 + (IL-1)*LLT*2

                     CALL SHELL_ROTA(
     1                      I        ,ILAY    ,NEL     ,IORTH  ,ITY          ,
     2                      IGTYP    ,MLW     ,JDIR    ,SIG    ,ELBUF_TAB(NG),
     3                      RX       ,RY      ,RZ      ,SX     ,SY           ,  
     4                      SZ       ,E1X     ,E2X     ,E3X    ,E1Y          , 
     5                      E2Y      ,E3Y     ,E1Z     ,E2Z    ,E3Z          ,
     6                      DIR_A    ,DIR_B    )				
                      JJ = JJ + 1
                      WA(JJ) = DEUX * POSLY(I,IPTT)
                      JJ = JJ + 1
                      WA(JJ) = SIG(1)
                      JJ = JJ + 1
                      WA(JJ) = SIG(2)
                      JJ = JJ + 1
                      WA(JJ) = SIG(3)
                      JJ = JJ + 1
                      WA(JJ) = SIG(4)
                      JJ = JJ + 1
                      WA(JJ) = SIG(5)  
                      JJ = JJ + 1
                      WA(JJ) = SIG(6)
                      JJ = JJ + 1
                      IF (L_PLA > 0) THEN                                   
                        WA(JJ) = LBUF%PLA(I)
                      ELSE
                        WA(JJ) = ZERO
                      ENDIF
                    ENDDO !  DO IPG=1,NPG
                  ENDDO !  DO IT=1,NPTT   
                  IPTT = IPTT + NPTT
                ENDDO  !  DO IL=1,NLAY
              ENDIF  !  IF (MLW == 0 .or. MLW == 13)
            ENDIF  !  IF (MPT == 0)
C
            IE=IE+1
C        pointer for last position for element IE
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT

          IF (ALLOCATED(DIRB))     DEALLOCATE(DIRB)                                                          
          IF (ALLOCATED(DIRA))     DEALLOCATE(DIRA)   

        ENDIF  !  IF (ITY == 7)
       ENDDO  !  DO NG=1,NGROUP
      ENDIF
C
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
Clm     recopying for code simplification
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
        PTWA_P0(0)=0
        DO N=1,DYNAIN_NUMELTG
          PTWA_P0(N)=PTWA(N)
        ENDDO
      ELSE
C      Global pointers WAP0
        CALL SPMD_STAT_PGATHER(PTWA,DYNAIN_NUMELTG,PTWA_P0,DYNAIN_NUMELTG_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF

      IF (ISPMD == 0.AND.LEN > 0) THEN
        IF(IS_WRITTEN == 0 ) THEN
          WRITE(IUDYNAIN,'(A)') DELIMIT
          WRITE(IUDYNAIN,'(A)')'*INITIAL_STRESS_SHELL'
          WRITE(IUDYNAIN,'(A)')
     .    '$  SHELLID       NPG     NBINT                         LARGE  ' 
          WRITE(IUDYNAIN,'(A)')
     .     '$ IF(NPT == 0), REPEAT I=1,NPG :'
          WRITE(IUDYNAIN,'(A)')
     .    '$ IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'      
          WRITE(IUDYNAIN,'(A)')
     .     '$         T    SIGXX     SIGYY     SIGZZ     SIGXY     SIGYZ     SIGZX       EPSP '
          WRITE(IUDYNAIN,'(A)') DELIMIT

          IS_WRITTEN = 1

         ENDIF

        DO N=1,DYNAIN_NUMELTG_G
C         Retrieving shell ID in increasing order 
          K=DYNAIN_INDXTG(N)
C         Adress in  WAP0
          J=PTWA_P0(K-1)
C
          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
            ID  = NINT(WAP0(J + 2)) 
            NPT = NINT(WAP0(J + 3)) 
            NPG = NINT(WAP0(J + 4)) 
            LARGE = NINT(WAP0(J + 5)) 
            J = J + 5

            WRITE(IUDYNAIN,'(3I10,20X,I10)')ID,NPG,NPT,LARGE
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(JJ + K),K=1,5)
                WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(JJ + K),K=6,8)
                J = J + 8
              ENDDO
            ELSE
              DO IPT=1,NPT 
                DO IPG=1,NPG

                   WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(J + K),K=1,5)
                   WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(J + K),K=6,8)
                   J = J + 8
                ENDDO
              ENDDO

            ENDIF  !  IF (NPT == 0)
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,DYNAIN_NUMELTG_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C
      DEALLOCATE(PTWA,PTWA_P0)
C
      RETURN
      END
