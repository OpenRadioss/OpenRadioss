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
Chd|  DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        GET_Q4L                       source/output/sta/stat_c_strafg.F
Chd|        GET_T3L                       source/output/sta/stat_c_strafg.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        SHELL2G                       source/output/sta/stat_c_strafg.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE DYNAIN_C_STRAG(
     1                  ELBUF_TAB  ,IPARG     ,IPM        ,IGEO  ,IXC    ,
     2                  IXTG       ,WA        ,WAP0       ,IPARTC,IPARTTG,
     3                  IPART_DYNAIN,DYNAIN_INDXC,DYNAIN_INDXTG,SIZP0    ,
     4                  GEO        ,STACK     ,DRAPE_SH4N   ,DRAPE_SH3N,X    ,
     5                  THKE       ,DRAPEG)
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
     .   GEO(NPROPG,*) , X(*) , THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE),DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,NPT,
     .   LLT,ITY,MLW,IH,IHBE, ID, IPRT0, IPRT,IR,IS,IT,
     .   NPG,IPG,MPT,IPT,NPTR,NPTS,NPTT,NLAY,L_PLA,ITHK,
     .   IGTYP,NPT_ALL,IL,KK(8),LARGE,IREP,IPID,IVISC,
     .   IPMAT,IXFEM,IXLAY,ISUBSTACK,IPTT,IS_WRITTEN,
     ,   LAYNPT_MAX,LAY_MAX,IERR,
     .   JDIR,ILAY,J1,J2,IREL,G_STRA,IPT_ALL
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: PTWA, PTWA_P0
      INTEGER MATLY(MVSIZ*LAY_MAX),MAT(MVSIZ),PID(MVSIZ)
      CHARACTER*80 DELIMIT
      CHARACTER*100 LINE
      my_real
     .   SIG(6)
      my_real
     .   THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX),POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX),
     .   THK_LY(MVSIZ,LAY_MAX*LAYNPT_MAX)
      my_real, 
     .   DIMENSION(:),POINTER  :: STRAIN    
      my_real 
     .   QT(9,MVSIZ),STRAING(6),ZH,THKP ,THK0(MVSIZ)   
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
C-----------------------------------------------

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
      IF (DYNAIN_NUMELC/=0) THEN
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
          LFT=1
          LLT=NEL

          G_STRA = GBUF%G_STRA
        
!
         DO J=1,8  ! length max of GBUF%G_STRA = 8
           KK(J) = NEL*(J-1)
         ENDDO
!
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
            NPT_ALL = 0
            DO IL=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF
          IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0

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
          NUMEL_DRAPE = NUMELC_DRAPE
          SEDRAPE = SCDRAPE 
          CALL LAYINI(
     .           ELBUF_TAB(NG),LFT      ,LLT        ,GEO        ,IGEO      ,
     .           MAT        ,PID        ,THKLY      ,MATLY      ,POSLY     ,
     .           IGTYP      ,IXFEM      ,IXLAY      ,NLAY       ,NPT       ,
     .           ISUBSTACK  ,STACK      ,DRAPE_SH4N   ,NFT        ,THK0      , 
     .           NEL        ,THK_LY     ,DRAPEG%INDX_SH4N)

C-------------------------------------------------------
C     ELEMENT LOCAL FRAME : for rotation local -> Global
C-------------------------------------------------------
          IF (IHBE>10.OR.IGTYP==16.OR.ISHFRAM ==0) THEN
            IREL=0
          ELSEIF (ISHFRAM ==1) THEN
            IREL=2
          ELSE
            IREL=1
          END IF
          CALL GET_Q4L(LFT ,LLT ,IXC(1,NFT+1),X ,GBUF%OFF,IREL ,QT )

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
            WA(JJ) = ONE  ! LARGE

c---------

            IF (MLW == 0 .or. MLW == 13) THEN
              DO IPG=1,NPG 
                 JJ = JJ + 1
                 WA(JJ) = ZERO
                 DO J=1,7        ! STRAIN + POS           	
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                ENDDO                                                               
              ENDDO
            ELSEIF (MPT==0 .AND. G_STRA /= 0) THEN

              IF (NPG > 1) THEN
                 STRAIN => GBUF%STRPG
              ELSE
                 STRAIN => GBUF%STRA
              ENDIF

! LOWER
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 
                 ZH = -HALF*THKP

                 STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                 STRAING(3)=HALF*STRAING(3)
                 STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = -ONE
              ENDDO

! MEMBRANE
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 

                 STRAING(1:2)=STRAIN(KK(1:2)+I+K)
                 STRAING(3:5)=HALF*STRAIN(KK(3:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = ZERO
              ENDDO

! Upper
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 
                 ZH = HALF*THKP

                 STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                 STRAING(3)=HALF*STRAING(3)
                 STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = ONE
              ENDDO


            ELSEIF (G_STRA /= 0) THEN

              IF (NPG > 1) THEN
                 STRAIN => GBUF%STRPG
              ELSE
                 STRAIN => GBUF%STRA
              ENDIF

              IF (ITHK >0 ) THEN
                 THKP = GBUF%THK(I)
              ELSE
                 THKP = THKE(I + NFT)
              END IF

              IPT_ALL = 0
              DO ILAY =1,NLAY
                 NPTT   = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                 DO IT=1,NPTT
                    IPT = IPT_ALL + IT        
C--   
                    DO IPG=1,NPG
                       K = (IPG-1)*NEL*G_STRA 
                       ZH = POSLY(I,IPT)*THKP
                       STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                       STRAING(3)=HALF*STRAING(3)
                       STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                       CALL SHELL2G(STRAING,QT(1,I))             
C               
                       DO J=1,6
                         JJ    = JJ + 1
                         WA(JJ) = STRAING(J)
                       END DO      
                       JJ    = JJ + 1
                       WA(JJ) = POSLY(I,IPT)*TWO
                    END DO        
                 END DO !IT=1,NPTT
                 IPT_ALL = IPT_ALL + NPTT
              END DO !ILAY =1,NLAY
         
            ENDIF
C
            IE=IE+1
C           Pointer last position of shell IE in WA
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT

c------- end loop over 4 node shell elements
        ENDIF ! ITY == 3
       ENDDO   ! NG = 1, NGROUP
      ENDIF  ! DYNAIN_NUMEL /= 0

c-----------------------------------------------------------------------
c     4N SHELLS - WRITE
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
C       recopying for code simplification
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
          WRITE(IUDYNAIN,'(A)')'*INITIAL_STRAIN_SHELL'
          WRITE(IUDYNAIN,'(A)')
     .    '$  SHELLID       NPG     NBINT     LARGE  ' 
          WRITE(IUDYNAIN,'(A)')
     .     '$ IF(NPT == 0), REPEAT I=1,NPG :'
          WRITE(IUDYNAIN,'(A)')
     .    '$ IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'      
           WRITE(IUDYNAIN,'(A)')
     .     '$    EPSXX     EPSYY     EPSZZ     EPSXY     EPSYZ     EPSZX         T'
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

            WRITE(IUDYNAIN,'(3I10,I10)')ID,NPG,NPT,LARGE
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(JJ + K),K=1,3)
                WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(JJ + K),K=6,7)
                J = J + 7
              ENDDO
            ELSE
              DO IPT=1,NPT 
                DO IPG=1,NPG

                   WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(J + K),K=1,5)
                   WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(J + K),K=6,7)
                   J = J + 7
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
      IF(DYNAIN_NUMELTG/=0) THEN
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

          G_STRA = GBUF%G_STRA
!
          DO J=1,8  ! length max of GBUF%G_STRA = 8
            KK(J) = NEL*(J-1)
          ENDDO
!
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO K=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(K)%NPTT
            ENDDO
            MPT  = MAX(1,NPT_ALL)
          ENDIF
          IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) MPT=0

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
          NUMEL_DRAPE = NUMELTG_DRAPE
          SEDRAPE = STDRAPE
          CALL LAYINI(
     .           ELBUF_TAB(NG),LFT      ,LLT        ,GEO        ,IGEO      ,
     .           MAT        ,PID        ,THKLY      ,MATLY      ,POSLY     ,
     .           IGTYP      ,IXFEM      ,IXLAY      ,NLAY       ,NPT       ,
     .           ISUBSTACK  ,STACK      ,DRAPE_SH3N   ,NFT        ,THK0      , 
     .           NEL        ,THK_LY     ,DRAPEG%INDX_SH3N)

C-------------------------------------------------------
C     ELEMENT LOCAL FRAME : for rotation local -> Global
C-------------------------------------------------------
          IF (IHBE>=30) THEN
            IREL=0
          ELSE
            IREL=2
          END IF
          CALL GET_T3L(LFT    ,LLT    ,IXTG(1,NFT+1),X    ,GBUF%OFF,
     .                IREL   ,QT     )
  
C
c------- loop over 3 node shell elements
C
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
            WA(JJ) = ONE  ! LARGE

            IF (ITHK >0 ) THEN
               THKP = GBUF%THK(I)
            ELSE
               THKP = THKE(I + NFT)
            END IF

c---------
            IF (MLW == 0 .or. MLW == 13) THEN
              DO IPG=1,NPG 
                 JJ = JJ + 1
                 WA(JJ) = ZERO
                 DO J=1,7        ! STRAIN + POS           	
                    JJ = JJ + 1
                    WA(JJ) = ZERO
                ENDDO                                                               
              ENDDO
            ELSEIF (MPT==0 .AND. G_STRA /= 0) THEN

              IF (NPG > 1) THEN
                 STRAIN => GBUF%STRPG
              ELSE
                 STRAIN => GBUF%STRA
              ENDIF


! LOWER
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 
                 ZH = -HALF*THKP

                 STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                 STRAING(3)=HALF*STRAING(3)
                 STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = -ONE
              ENDDO

! MEMBRANE
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 

                 STRAING(1:2)=STRAIN(KK(1:2)+I+K)
                 STRAING(3:5)=HALF*STRAIN(KK(3:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = ZERO
              ENDDO

! Upper
              DO IPG=1,NPG
                 K = (IPG-1)*NEL*G_STRA 
                 ZH = HALF*THKP

                 STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                 STRAING(3)=HALF*STRAING(3)
                 STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                 CALL SHELL2G(STRAING,QT(1,I))             
 
                 DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                 END DO  
                 JJ = JJ + 1
                 WA(JJ) = ONE
              ENDDO


            ELSEIF (G_STRA /= 0) THEN

              IF (NPG > 1) THEN
                 STRAIN => GBUF%STRPG
              ELSE
                 STRAIN => GBUF%STRA
              ENDIF

              IPT_ALL = 0
              DO ILAY =1,NLAY
                 NPTT   = ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
                 DO IT=1,NPTT
                    IPT = IPT_ALL + IT        
C--   
                    IF (ITHK >0 ) THEN
                       THKP = GBUF%THK(I)
                    ELSE
                       THKP = THKE(I + NFT)
                    END IF

                    DO IPG=1,NPG
                       K = (IPG-1)*NEL*G_STRA 
                       ZH = POSLY(I,IPT)*THKP
                       STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                       STRAING(3)=HALF*STRAING(3)
                       STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)

                       CALL SHELL2G(STRAING,QT(1,I))             
C               
                       DO J=1,6
                         JJ    = JJ + 1
                         WA(JJ) = STRAING(J)
                       END DO      
                       JJ    = JJ + 1
                       WA(JJ) = POSLY(I,IPT)*TWO
                    END DO        
                 END DO !IT=1,NPTT
                 IPT_ALL = IPT_ALL + NPTT
              END DO !ILAY =1,NLAY
         
            ENDIF
C
            IE=IE+1
C        pointer for last position for element IE
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT  

        ENDIF  !  IF (ITY == 7)
       ENDDO  !  DO NG=1,NGROUP
      ENDIF
C
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
C       recopying for code simplification
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
          WRITE(IUDYNAIN,'(A)')'*INITIAL_STRAIN_SHELL'
          WRITE(IUDYNAIN,'(A)')
     .    '$  SHELLID       NPG     NBINT     LARGE  ' 
          WRITE(IUDYNAIN,'(A)')
     .     '$ IF(NPT == 0), REPEAT I=1,NPG :'
          WRITE(IUDYNAIN,'(A)')
     .    '$ IF(NPT /= 0) REPEAT K=1,NPT : REPEAT I=1,NPG :'      
          WRITE(IUDYNAIN,'(A)')
     .     '$         T    EPSXX     EPSYY     EPSZZ     EPSXY     EPSYZ     EPSZX'
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
                WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(JJ + K),K=6,7)
                J = J + 7
              ENDDO
            ELSE
              DO IPT=1,NPT 
                DO IPG=1,NPG
                   WRITE(IUDYNAIN,'(1P5G16.9)')(WAP0(J + K),K=1,5)
                   WRITE(IUDYNAIN,'(1P3G16.9)')(WAP0(J + K),K=6,7)
                   J = J + 7
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
