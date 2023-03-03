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
Chd|  NLOC_DMG_INIT                 source/materials/fail/nloc_dmg_init.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NLOCAL_INIT_STA               source/materials/fail/nlocal_init_sta.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE NLOC_DMG_INIT(ELBUF_TAB,NLOC_DMG ,IPARG    ,IXC      ,
     .                         IXS      ,IXTG     ,AREA     ,DTELEM   ,
     .                         NUMEL    ,IPM      ,X        ,XREFS    ,
     .                         XREFC    ,XREFTG   ,BUFMAT   ,PM       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "my_allocate.inc"
#include      "scr15_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL
      INTEGER IPARG(NPARG,NGROUP),IXS(NIXS,*),IXC(NIXC,*),IXTG(NIXTG,*)
      INTEGER IPM(NPROPMI,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_) , TARGET                    :: NLOC_DMG 
      my_real, 
     .         DIMENSION(NUMELC+NUMELTG),INTENT(IN)  :: AREA 
      my_real,
     .         DIMENSION(NUMEL), INTENT(INOUT)       :: DTELEM
      my_real
     .   X(3,*),XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*),BUFMAT(*),
     .   PM(NPROPM,*)
      TARGET BUFMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL :: ENG_FILE
      INTEGER I,J,K,NG,NEL,NFT,ITY,NPTT,ILOC,INOD,NNOD,NDEPAR,IMAT,
     .        L_NLOC,POS,NDD,FORMU,N,NUMELS_NL,IGTYP,NUMELC_NL,NDDMAX,
     .        NUMELTG_NL,NPTR,NPTS,IR,IS,ISOLNOD,IO_ERR1,LEN_TMP_NAME,
     .        WORK(70000),IDEB,IDEBS,IDEBC,IDEBT,IADBUF,MATSIZE
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGNOD
      INTEGER, DIMENSION(:),   ALLOCATABLE :: INDX,IDXI,NMAT,NDDL,
     .                                        POSI,ITRI,INDEX,TAGTET
      INTEGER, DIMENSION(:,:), POINTER     :: IADS
      my_real
     .   DENS, DTMIN, LEN, SSPNL,NTH1, NTH2,
     .   Z01(11,11), WF1(11,11), ZN1(12,11),DAMP,WS,LE_MIN,
     .   DTSCA_AMS,DTSCA_CST_AMS,LE_MAX,SSP,
     .   DTMINI_AMS,DTMINI_CST_AMS,DTMINI
      my_real, DIMENSION(:,:), ALLOCATABLE ::  
     .   VOLN_S, VOLN_C, VOLN_TG, WARN_LENGHT
      my_real, DIMENSION(:)  , ALLOCATABLE ::  
     .   VOLN, VOLU
      my_real ,DIMENSION(:)  , POINTER     :: 
     .   VOL, THCK, UPARAM
      TYPE(BUF_NLOC_), POINTER             :: BUFNL
      my_real, DIMENSION(:,:), POINTER     :: 
     .   MASSTH
      ! Damping ratio ZETA 
      ! ZETA = 0     : Undamped
      ! 0 < ZETA < 1 : Underdamped
      ! ZETA = 1     : Critically damped
      ! ZETA > 1     : Overdamped
      my_real, PARAMETER                   :: ZETA = 0.2D0 
      ! Safety coefficient for non-local stability vs mechanical stability
      ! (here we have a good compromise to find, DENS must be as low as 
      !  possible but sufficiently high to avoid the decrease of the timestep)
      my_real, PARAMETER                   :: CSTA = 40.0D0
      ! Position of integration points in the thickness
      DATA  Z01/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7 0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8 0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9 -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9 0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A -.5      ,-.3888889,-.2777778,-.1666667,-.0555555,
     A 0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B 0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
      ! Weight of integration in the thickness 
      DATA  WF1/
     1 1.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 0.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 0.25     ,0.5      ,0.25     ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 0.1666667,0.3333333,0.3333333,0.1666667,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 0.125    ,0.25     ,0.25     ,0.25     ,0.125    ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 0.1      ,0.2      ,0.2      ,0.2      ,0.2      ,
     6 0.1      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 0.0833333,0.1666667,0.1666667,0.1666667,0.1666667,
     7 0.1666667,0.0833333,0.       ,0.       ,0.       ,0.       ,
     8 0.0714286,0.1428571,0.1428571,0.1428571,0.1428571,
     8 0.1428571,0.1428571,0.0714286,0.       ,0.       ,0.       ,
     9 0.0625   ,0.125    ,0.125    ,0.125    ,0.125    ,
     9 0.125    ,0.125    ,0.125    ,0.0625   ,0.       ,0.       ,
     A 0.0555556,0.1111111,0.1111111,0.1111111,0.1111111,
     A 0.1111111,0.1111111,0.1111111,0.1111111,0.0555556,0.       ,
     B 0.05     ,0.1      ,0.1      ,0.1      ,0.1      ,
     B 0.1      ,0.1      ,0.1      ,0.1      ,0.1      ,0.05     /
      ! Position of nodes in the shell thickness
      DATA  ZN1/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,-.25     ,0.25     ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.3333333,0.       ,0.3333333,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.375    ,-0.125   ,0.125    ,0.375    ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.4      ,-.2      ,0.0      ,0.2      ,0.4      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.4166667,-.25     ,-.0833333,0.0833333,0.25     ,
     7 0.4166667,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.4285715,-.2857143,-.1428572,0.0      ,0.1428572,
     8 0.2857143,0.4285715,0.5      ,0.       ,0.       ,0.       ,     
     9 -.5      ,-.4375   ,-.3125   ,-.1875   ,-.0625   ,0.0625   ,
     9 0.1875   ,0.3125   ,0.4375   ,0.5      ,0.       ,0.       ,
     A -.5      ,-.4444444,-.3333333,-.2222222,-.1111111,0.       ,
     A 0.1111111,0.2222222,0.3333333,0.4444444,0.5      ,0.       ,
     B -.5      ,-.45     ,-.35     ,-.25     ,-.15     ,-.05     ,
     B 0.05     ,0.15     ,0.25     ,0.35     ,0.45     ,0.5      /
C=======================================================================
      ! If the non-local method is not used
      IF (NLOC_DMG%IMOD == 0) THEN
        NLOC_DMG%NNOD       = 0
        NLOC_DMG%L_NLOC     = 0
        NLOC_DMG%NUMELS_NL  = 0
        NLOC_DMG%NUMELC_NL  = 0
        NLOC_DMG%NUMELTG_NL = 0
        NLOC_DMG%NDDMAX     = 0
        IF (.NOT.ALLOCATED(NLOC_DMG%DENS))     ALLOCATE(NLOC_DMG%DENS(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%DAMP))     ALLOCATE(NLOC_DMG%DAMP(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%LEN))      ALLOCATE(NLOC_DMG%LEN(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%LE_MAX))   ALLOCATE(NLOC_DMG%LE_MAX(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%SSPNL))    ALLOCATE(NLOC_DMG%SSPNL(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%INDX))     ALLOCATE(NLOC_DMG%INDX(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%POSI))     ALLOCATE(NLOC_DMG%POSI(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IDXI))     ALLOCATE(NLOC_DMG%IDXI(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%ADDCNE))   ALLOCATE(NLOC_DMG%ADDCNE(0))        
        IF (.NOT.ALLOCATED(NLOC_DMG%CNE))      ALLOCATE(NLOC_DMG%CNE(0)) 
        IF (.NOT.ALLOCATED(NLOC_DMG%IADS))     ALLOCATE(NLOC_DMG%IADS(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IADC))     ALLOCATE(NLOC_DMG%IADC(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IADTG))    ALLOCATE(NLOC_DMG%IADTG(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%MASS))     ALLOCATE(NLOC_DMG%MASS(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%MASS0))    ALLOCATE(NLOC_DMG%MASS0(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%FNL))      ALLOCATE(NLOC_DMG%FNL(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%VNL))      ALLOCATE(NLOC_DMG%VNL(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%VNL_OLD))  ALLOCATE(NLOC_DMG%VNL_OLD(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%DNL))      ALLOCATE(NLOC_DMG%DNL(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%UNL))      ALLOCATE(NLOC_DMG%UNL(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%STIFNL))   ALLOCATE(NLOC_DMG%STIFNL(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%FSKY))     ALLOCATE(NLOC_DMG%FSKY(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%STSKY))    ALLOCATE(NLOC_DMG%STSKY(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IAD_ELEM)) ALLOCATE(NLOC_DMG%IAD_ELEM(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IAD_SIZE)) ALLOCATE(NLOC_DMG%IAD_SIZE(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%FR_ELEM))  ALLOCATE(NLOC_DMG%FR_ELEM(0))
c        
      ! If non-local method is used
      ELSE
c
        ! Writing header
        WRITE(ISTDO,'(A)') ' .. NON-LOCAL STRUCTURE INITIALIZATION'
c
        ! Allocation of tables
        ALLOCATE( TAGNOD(NUMNOD,3) )
        ALLOCATE( INDX(NUMNOD) )  
        ALLOCATE( IDXI(NUMNOD) )     
        ALLOCATE( NDDL(NUMNOD) )  
        ALLOCATE( NMAT(NUMNOD) ) 
        ALLOCATE( POSI(NUMNOD+1) )
        ALLOCATE( VOLN_S(8,NUMELS) )
        ALLOCATE( VOLN_C(4,NUMELC) )
        ALLOCATE( VOLN_TG(3,NUMELTG) )
        ALLOCATE( VOLN(NUMNOD) )
        ALLOCATE( VOLU(NUMELS) )
        ALLOCATE( TAGTET(NUMELS) )
        ALLOCATE( INDEX(NUMELS+NUMELC+NUMELTG) )
        ALLOCATE( ITRI(NUMELS+NUMELC+NUMELTG) )
C
        IF (NSUBDOM > 0) THEN
C--       multidomains - origianl nummat is used
          MATSIZE = NUMMAT0
        ELSE
          MATSIZE = NUMMAT
        ENDIF
        MY_ALLOCATE2D(WARN_LENGHT,MATSIZE,3)
c   
        ! Initialization of variables   
        VOLN_S(1:8,1:NUMELS)   = ZERO
        VOLN_C(1:4,1:NUMELC)   = ZERO
        VOLN_TG(1:3,1:NUMELTG) = ZERO
        VOLN(1:NUMNOD)         = ZERO
        VOLU(1:NUMELS)         = ZERO
        INDEX(1:NUMELS+NUMELC+NUMELTG) = 0
        ITRI(1:NUMELS+NUMELC+NUMELTG)  = 0
        TAGNOD(1:NUMNOD,1:3) = 0
        TAGTET(1:NUMELS)     = 0
        NUMELS_NL  = 0
        NUMELC_NL  = 0
        NUMELTG_NL = 0
        NDDMAX     = 0
        WARN_LENGHT(1:MATSIZE,1:3) = ZERO
c     
        ! Computation of the volume of elements 
        DO NG=1,NGROUP
          ! Flag for non-local regularization
          ILOC  = IPARG(78,NG)
          ! Property
          IGTYP = IPARG(38,NG)
          ! Number of elements in the group
          NEL   = IPARG(2,NG)
          IF (ILOC > 0) THEN
            ! First element of the group
            NFT = IPARG(3,NG)
            ! Type of elements
            ITY = IPARG(5,NG)
            ! Formulation of the elements (under-integrated,fully integrated,...)
            FORMU = IPARG(23,NG)
            ! For brick elements
            IF (ITY == 1) THEN
              ! To detect tetra elements
              ISOLNOD = IPARG(28,NG)
              ! Volume of the element
              VOL => ELBUF_TAB(NG)%GBUF%VOL(1:NEL)
              ! Index and sorting table
              DO K = 1,NEL
                INDEX(NUMELS_NL+K) = K + NFT
                ITRI(K+NFT) = IXS(11,K+NFT)
                VOLU(K+NFT) = VOL(K)
              ENDDO 
              ! Counting non-local solid elements
              NUMELS_NL = NUMELS_NL + NEL
              ! Material Law ID
              IMAT = IXS(1,1+NFT)
              ! Tetra element
              IF (ISOLNOD == 4) THEN 
                ! Loop over elements
                DO I=1,NEL
                  ! Tag tetra element
                  TAGTET(I+NFT) = 1
                  ! Loop over tetra nodes
                  DO J=1,4
                    ! Number of the nodes
                    IF (J == 1) K = 2
                    IF (J == 2) K = 4
                    IF (J == 3) K = 7
                    IF (J == 4) K = 6
                    INOD = IXS(K,I+NFT)
                    ! Tag the node as non-local
                    TAGNOD(INOD,1) = 1
                    ! Tag the number of additional d.o.fs
                    TAGNOD(INOD,2) = 1
                    ! If already written and different => error
                    IF ((TAGNOD(INOD,3) /= 0).AND.(TAGNOD(INOD,3) /= IMAT)) THEN
                      CALL ANCMSG(MSGID=1656,MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,I1=INOD,I2=IMAT,I3=TAGNOD(INOD,3))                  
                    ENDIF
                    ! Tag the material law id of the node
                    TAGNOD(INOD,3) = IMAT
                    ! Computation of the volume associated to the node
                    VOLN_S(J,I+NFT) = FOURTH * VOL(I)
                  ENDDO      
                ENDDO
              ! Brick element
              ELSEIF (ISOLNOD == 8) THEN
                ! Supported formulations
                IF (FORMU/=16) THEN
                  ! Loop over elements
                  DO I = 1,NEL
                    ! Loop over brick nodes
                    DO J = 1,8
                      ! Number of the nodes
                      K = J + 1
                      INOD = IXS(K,I+NFT)
                      ! Tag the node as non-local
                      TAGNOD(INOD,1) = 1
                      ! Tag the number of additional d.o.fs
                      TAGNOD(INOD,2) = 1
                      ! If already written and different => error
                      IF ((TAGNOD(INOD,3) /= 0).AND.(TAGNOD(INOD,3) /= IMAT)) THEN
                        CALL ANCMSG(MSGID=1656,MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,I1=INOD,I2=IMAT,I3=TAGNOD(INOD,3))                  
                      ENDIF
                      ! Tag the material law id of the node
                      TAGNOD(INOD,3) = IMAT
                      ! Computation of the volume associated to the node
                      VOLN_S(J,I+NFT) = ONE_OVER_8 * VOL(I)
                    ENDDO      
                  ENDDO
                ELSE
                  ! Formulations not supported
                  CALL ANCMSG(MSGID=1659,MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,I1=FORMU)                 
                ENDIF
              ENDIF  
            ! For 4-nodes shell elements
            ELSEIF (ITY == 3) THEN
              ! Non-supported property
              IF ((IGTYP /= 1).AND.(IGTYP /= 9)) THEN 
                CALL ANCMSG(MSGID=1662,MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,I1=IGTYP)               
              ENDIF
              ! Beginning value in index table
              IDEB = NUMELS
              ! Index and sorting table
              DO K = 1,NEL
                INDEX(IDEB+NUMELC_NL+K) = K + NFT
                ITRI(IDEB+K+NFT)        = IXC(7,K+NFT)
              ENDDO              
              ! Counting non-local shell elements
              NUMELC_NL = NUMELC_NL + NEL
              ! Number of integration points in the thickness
              NPTT = IPARG(6,NG)
              ! Material law ID
              IMAT = IXC(1,1+NFT)
              ! Thickness of the shell element
              THCK => ELBUF_TAB(NG)%GBUF%THK(1:NEL) 
              ! Loop over elements
              DO I = 1,NEL
                ! Loop over shell element nodes
                DO J = 1,4
                  ! Node number
                  K = J + 1
                  INOD = IXC(K,I+NFT)
                  ! Tag the node as non-local
                  TAGNOD(INOD,1) = 1
                  ! If already written and different => error
                  IF ((TAGNOD(INOD,2) /= 0).AND.(TAGNOD(INOD,2) /= NPTT)) THEN
                    CALL ANCMSG(MSGID=1657,MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,I1=INOD,I2=NPTT,I3=TAGNOD(INOD,2))                  
                  ENDIF
                  ! Tag the number of additional d.o.fs (= NPTT)
                  TAGNOD(INOD,2) = NPTT
                  ! If already written and different => error
                  IF ((TAGNOD(INOD,3) /= 0).AND.(TAGNOD(INOD,3) /= IMAT)) THEN
                    CALL ANCMSG(MSGID=1656,MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,I1=INOD,I2=IMAT,I3=TAGNOD(INOD,3))                  
                  ENDIF
                  ! Tag the material law ID
                  TAGNOD(INOD,3)  = IMAT
                  ! Computation of the volume associated to the node
                  VOLN_C(J,I+NFT) = FOURTH * AREA(NFT+I) * THCK(I)
                ENDDO      
              ENDDO  
            ! For 3-nodes shell elements
            ELSEIF (ITY == 7) THEN
              ! Non-supported property
              IF ((IGTYP /= 1).AND.(IGTYP /= 9)) THEN 
                CALL ANCMSG(MSGID=1662,MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,I1=IGTYP)               
              ENDIF
              ! Beginning value in index table
              IDEB = NUMELS+NUMELC
              ! Index and sorting table
              DO K = 1,NEL
                INDEX(IDEB+NUMELTG_NL+K) = K + NFT
                ITRI(IDEB+K+NFT)         = IXTG(6,K+NFT)
              ENDDO 
              ! Counting non-local triangle shell elements
              NUMELTG_NL = NUMELTG_NL + NEL
              ! Number of integration point in the shell thickness
              NPTT = IPARG(6,NG)
              ! Material law ID
              IMAT = IXTG(1,1+NFT)
              ! Thickness of the shell
              THCK => ELBUF_TAB(NG)%GBUF%THK(1:NEL) 
              ! Loop over elements
              DO I = 1,NEL
                ! Loop over the nodes of the shell 
                DO J = 1,3
                  ! Number of the node
                  K = J + 1
                  INOD = IXTG(K,I+NFT)
                  ! Tag the node as non-local
                  TAGNOD(INOD,1) = 1
                  ! If already written and different => error
                  IF ((TAGNOD(INOD,2) /= ZERO).AND.(TAGNOD(INOD,2) /= NPTT)) THEN
                    CALL ANCMSG(MSGID=1657,MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,I1=INOD,I2=NPTT,I3=TAGNOD(INOD,2))                  
                  ENDIF
                  ! Tag the number of additional d.o.fs (= NPTT)
                  TAGNOD(INOD,2) = NPTT
                  ! If already written and different => error
                  IF ((TAGNOD(INOD,3) /= ZERO).AND.(TAGNOD(INOD,3) /= IMAT)) THEN
                    CALL ANCMSG(MSGID=1656,MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,I1=INOD,I2=IMAT,I3=TAGNOD(INOD,3))                  
                  ENDIF
                  ! Tag the material law ID
                  TAGNOD(INOD,3) = IMAT
                  ! Computation of the volume associated to the node
                  VOLN_TG(J,I+NFT) = THIRD * AREA(NUMELC+NFT+I) * THCK(I)
                ENDDO      
              ENDDO
            ! For all other types of elements
            ELSE
              CALL ANCMSG(MSGID=1658,MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,I1=ITY)   
            ENDIF
          ENDIF
        ENDDO
C
        ! Checking if a DT option is set in the engine file
        DTMINI_AMS     = ZERO
        DTMINI_CST_AMS = ZERO
        FILNAM = ROOTNAM(1:ROOTLEN)//'_0001.rad'
        LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
        TMP_NAME = INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
        INQUIRE(FILE = TMP_NAME,EXIST = ENG_FILE)
        IF (ENG_FILE) THEN 
          ! Opening the engine file
          OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
          ! Reading keywords
 10       READ(71,'(A)',END=20) KEYA
          !/DT/AMS
          IF(KEYA(1:7)=='/DT/AMS') THEN
 30         READ(71,'(A)') KEYA
            IF ((KEYA(1:1)=='#').OR.(KEYA(1:1)=='$')) THEN
              GOTO 30
            ELSE
              BACKSPACE(71)
            ENDIF
            READ(71,*) DTSCA_AMS,DTMINI_AMS
            IF (DTSCA_AMS == ZERO) DTSCA_AMS = ZEP9    
          ENDIF
          !/DT/CST_AMS
          IF(KEYA(1:11)=='/DT/CST_AMS') THEN
 40         READ(71,'(A)') KEYA
            IF ((KEYA(1:1)=='#').OR.(KEYA(1:1)=='$')) THEN
              GOTO 40
            ELSE
              BACKSPACE(71)
            ENDIF
            READ(71,*) DTSCA_CST_AMS,DTMINI_CST_AMS
            IF (DTSCA_CST_AMS == ZERO) DTSCA_CST_AMS = ZEP9    
          ENDIF
          ! Back to read the keywords
          GOTO 10
 20       CONTINUE
          ! Closing the file
          CLOSE(71)
        ELSE
          ! No engine file has been found
          CALL ANCMSG(MSGID=1730,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                C1=ROOTNAM(1:ROOTLEN)//'_0001.rad')   
        ENDIF
        ! Maximum of the minimal timesteps
        DTMINI = MAX(DTMINI_AMS,DTMINI_CST_AMS)
C
        ! Sorting tables
        !   -> Sorting solid elements
        IF ((NUMELS>0).AND.(NUMELS_NL>0))   CALL QUICKSORT_I2(ITRI, INDEX, 1, NUMELS_NL)
        !   -> Sorting shell elements
        IF ((NUMELC>0).AND.(NUMELC_NL>0))   CALL QUICKSORT_I2(ITRI, INDEX, NUMELS+1, NUMELS+NUMELC_NL)
        !   -> Sorting triangle elements
        IF ((NUMELTG>0).AND.(NUMELTG_NL>0)) CALL QUICKSORT_I2(ITRI, INDEX, NUMELS+NUMELC+1, NUMELS+NUMELC+NUMELTG_NL)
        ! Offsets in the DTELEM table
        IDEBS = MINVAL(IXS(11,1:NUMELS))
        IDEBC = MINVAL(IXC(7,1:NUMELC))
        IDEBT = MINVAL(IXTG(6,1:NUMELTG))
C
        ! Assembling the volume per node always in the same order
        ! and automatic computation of the non-local density, and the non-local damping
        DO J = 1, NUMELS_NL+NUMELC_NL+NUMELTG_NL
          ! Solid elements
          IF (J<=NUMELS_NL) THEN 
            ! Number of the element
            I = INDEX(J)
            ! Material number
            IMAT   = IXS(1,I)
            ! Soundspeed
            SSP    = SQRT(((THIRD*PM(20,IMAT)/(ONE - PM(21,IMAT)*TWO)) + FOUR_OVER_3*PM(22,IMAT))/PM(1,IMAT))
            ! Element characteristic length
            LE_MIN = (VOLU(I))**THIRD
            IF (TAGTET(I)>0) THEN 
              ! Loop over element nodes
              DO K = 1,4
                IF (K == 1) N = IXS(2,I)
                IF (K == 2) N = IXS(4,I)
                IF (K == 3) N = IXS(7,I)
                IF (K == 4) N = IXS(6,I)
                VOLN(N) = VOLN(N) + VOLN_S(K,I)
              ENDDO 
            ELSE
              ! Loop over element nodes
              DO K = 1,8
                N = IXS(K+1,I)
                VOLN(N) = VOLN(N) + VOLN_S(K,I)
              ENDDO 
            ENDIF
          ! Shell elements
          ELSEIF (J<=NUMELS_NL+NUMELC_NL) THEN
            ! Number of the element 
            I = INDEX(NUMELS+J-NUMELS_NL)
            ! Loop over nodes of the element
            DO K = 1,4
              N = IXC(K+1,I)
              VOLN(N) = VOLN(N) + VOLN_C(K,I)
            ENDDO 
            ! Material number
            IMAT   = IXC(1,I)
            ! Soundspeed
            SSP    = SQRT(PM(24,IMAT)/PM(1,IMAT))
            ! Element characteristic length
            LE_MIN = SQRT(AREA(I))       
          ! Triangle elements
          ELSEIF (J<=NUMELS_NL+NUMELC_NL+NUMELTG_NL) THEN
            ! Number of the element
            I = INDEX(NUMELS+NUMELC+J-NUMELS_NL-NUMELC_NL)
            ! Loop over nodes of the element
            DO K = 1,3
              N = IXTG(K+1,I)
              VOLN(N) = VOLN(N) + VOLN_TG(K,I)
            ENDDO
            ! Material number
            IMAT   = IXTG(1,I)
            ! Soundspeed
            SSP    = SQRT(PM(24,IMAT)/PM(1,IMAT))
            ! Element characteristic length
            LE_MIN = SQRT((FOUR/SQRT(THREE))*AREA(I))            
          ENDIF
          ! Recovering the non-local internal length
          LEN    = NLOC_DMG%LEN(IMAT)
          ! Computing the theoretical maximal length
          LE_MAX = NLOC_DMG%LE_MAX(IMAT)
          IF (LE_MAX == ZERO) THEN 
            NLOC_DMG%LE_MAX(IMAT) = LE_MIN
            LE_MAX = LE_MIN
          ENDIF 
          ! Computation of the minimal timestep
          DTMIN = MAX(LE_MAX/SSP,DTMINI)
          ! Computation of the non-local density
          DENS = CSTA*(((LEN/MAX(LE_MAX,EM20))**2 + (ONE/TWELVE))*(DTMIN**2))
          IF (LE_MIN > LE_MAX) THEN 
            WARN_LENGHT(IMAT,1) = ONE
            WARN_LENGHT(IMAT,2) = LE_MAX
            WARN_LENGHT(IMAT,3) = LE_MIN
          ENDIF
          ! Computation of non-local damping
          IF ((DENS < NLOC_DMG%DENS(IMAT)).OR.(NLOC_DMG%DENS(IMAT) == ZERO)) THEN 
            ! Computation of the damping parameter (homogeneous to a time value)
            DAMP  = TWO*ZETA*SQRT((FOUR*(LEN/MAX(LE_MAX,EM20))**2 + THIRD)*DENS)
            ! Saving non-local parameters (storing the maximal value)
            NLOC_DMG%DENS(IMAT) = MAX(DENS,ZERO)
            NLOC_DMG%DAMP(IMAT) = MAX(DAMP,ZERO)
          ENDIF
          ! Computation of the initial non-local sound-speed
          SSPNL = (SQRT(TWELVE*(LEN**2) + (LE_MIN)**2))/(TWO*SQRT(THREE*DENS))
          IF ((SSPNL < NLOC_DMG%SSPNL(IMAT)).OR.(NLOC_DMG%SSPNL(IMAT) == ZERO)) THEN
            NLOC_DMG%SSPNL(IMAT) = MAX(SSPNL,ZERO)
          ENDIF
        ENDDO
c
        ! Checking non-local length consistency with mesh size
        DO I = 1, MATSIZE
          IF (WARN_LENGHT(I,1) > ZERO) THEN
            CALL ANCMSG(MSGID=1812,MSGTYPE=MSGWARNING,
     .         ANMODE=ANINFO_BLIND_1,I1=IPM(1,I),R1=NLOC_DMG%LEN(I),
     .                R2=WARN_LENGHT(I,2),R3=WARN_LENGHT(I,3))
          ENDIF
        ENDDO
c
        ! Printing out non-local parameters
        WRITE(IOUT,1800)
        DO I = 1, MATSIZE
          IF (NLOC_DMG%DENS(I) > ZERO) THEN 
            WRITE(IOUT,1900) IPM(1,I),NLOC_DMG%LEN(I),NLOC_DMG%LE_MAX(I),NLOC_DMG%DENS(I),NLOC_DMG%DAMP(I)
          ENDIF
        ENDDO
c        
        ! Data on non-local nodes
        NNOD   = 0 ! Total number of non-local nodes
        L_NLOC = 0 ! Length of the non-local vectors
        INDX(1:NUMNOD)   = 0 ! Index of non-local nodes
        NDDL(1:NUMNOD)   = 0 ! Number of additional d.o.fs for non-local nodes
        POSI(1:NUMNOD+1) = 0 ! Position of the first degree of freedom for non-local nodes
        NMAT(1:NUMNOD)   = 0 ! Material of the non-local nodes
        IDXI(1:NUMNOD)   = 0 ! Inversed of the index table
        DO I=1,NUMNOD
          IF (TAGNOD(I,1) == 1) THEN
            NNOD       = NNOD + 1
            INDX(NNOD) = I
            NDDL(NNOD) = TAGNOD(I,2)
            NMAT(NNOD) = TAGNOD(I,3)
            POSI(NNOD) = L_NLOC + 1
            IDXI(I)    = NNOD
            L_NLOC     = L_NLOC + TAGNOD(I,2)
          ENDIF
        ENDDO
        POSI(NNOD + 1) = L_NLOC + 1 ! Last value of the position
c       
        ! Maximal number of additional d.o.fs
        NDDMAX = MAXVAL(NDDL(1:NNOD))
c
        ! Saving non-local parameters
        NLOC_DMG%NNOD       = NNOD
        NLOC_DMG%L_NLOC     = L_NLOC
        NLOC_DMG%NUMELS_NL  = NUMELS_NL
        NLOC_DMG%NUMELC_NL  = NUMELC_NL
        NLOC_DMG%NUMELTG_NL = NUMELTG_NL
        NLOC_DMG%NDDMAX     = NDDMAX
c
        ! Allocation of non-local tables
        MY_ALLOCATE(NLOC_DMG%INDX,NNOD)
        MY_ALLOCATE(NLOC_DMG%POSI,NNOD+1)
        MY_ALLOCATE(NLOC_DMG%IDXI,NUMNOD)
        MY_ALLOCATE(NLOC_DMG%MASS,L_NLOC)   
        MY_ALLOCATE(NLOC_DMG%MASS0,L_NLOC)   
        MY_ALLOCATE(NLOC_DMG%VNL,L_NLOC)
        MY_ALLOCATE(NLOC_DMG%VNL_OLD,L_NLOC) 
        MY_ALLOCATE(NLOC_DMG%DNL,L_NLOC)
        MY_ALLOCATE(NLOC_DMG%UNL,L_NLOC)
        IF (.NOT.ALLOCATED(NLOC_DMG%STIFNL))   ALLOCATE(NLOC_DMG%STIFNL(L_NLOC,1))
        IF (.NOT.ALLOCATED(NLOC_DMG%FNL))      ALLOCATE(NLOC_DMG%FNL(L_NLOC,1))
        IF (.NOT.ALLOCATED(NLOC_DMG%FSKY))     ALLOCATE(NLOC_DMG%FSKY(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%STSKY))    ALLOCATE(NLOC_DMG%STSKY(0,0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IAD_SIZE)) ALLOCATE(NLOC_DMG%IAD_SIZE(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%IAD_ELEM)) ALLOCATE(NLOC_DMG%IAD_ELEM(0))
        IF (.NOT.ALLOCATED(NLOC_DMG%FR_ELEM))  ALLOCATE(NLOC_DMG%FR_ELEM(0))
c
        ! Initializing non-local tables
        NLOC_DMG%INDX(1:NNOD)       = INDX(1:NNOD)
        NLOC_DMG%POSI(1:NNOD+1)     = POSI(1:NNOD+1)
        NLOC_DMG%IDXI(1:NUMNOD)     = IDXI(1:NUMNOD)
        NLOC_DMG%FNL(1:L_NLOC,1)    = ZERO
        NLOC_DMG%VNL(1:L_NLOC)      = ZERO
        NLOC_DMG%VNL_OLD(1:L_NLOC)  = ZERO
        NLOC_DMG%DNL(1:L_NLOC)      = ZERO
        NLOC_DMG%UNL(1:L_NLOC)      = ZERO
        NLOC_DMG%STIFNL(1:L_NLOC,1) = ZERO
c 
        ! Computing non-local masses
        DO I=1,NNOD
          NDD  = NDDL(I)
          POS  = POSI(I)
          DENS = NLOC_DMG%DENS(NMAT(I))
          DO J = POS,POS+NDD-1
            ! For brick elements
            IF (ITY == 1) THEN
              NLOC_DMG%MASS(J)  = VOLN(INDX(I))*DENS
              NLOC_DMG%MASS0(J) = VOLN(INDX(I))*DENS
            ! For shell and triangle elements
            ELSEIF ((ITY == 3).OR.(ITY == 7)) THEN 
              NLOC_DMG%MASS(J)  = WF1(J-POS+1,NDD)*VOLN(INDX(I))*DENS
              NLOC_DMG%MASS0(J) = WF1(J-POS+1,NDD)*VOLN(INDX(I))*DENS
            ENDIF
          ENDDO
        ENDDO
c        
        ! Computing non-local masses in the thickness for shell elements only
        DO NG=1,NGROUP
          ! Non-local flag
          ILOC  = IPARG(78,NG)
          ! Type of elements
          ITY   = IPARG(5,NG)
          ! If the elements are non-local and are shells or triangles
          IF ((ILOC > 0).AND.((ITY == 3).OR.(ITY == 7))) THEN
            ! First element position
            NFT   = IPARG(3,NG)
            ! Number of the material
            IF (ITY == 3) THEN 
              IMAT  = IXC(1,1+NFT)
              NDEPAR = 0
            ELSEIF (ITY == 7) THEN
              IMAT  = IXTG(1,1+NFT)
              NDEPAR = NUMELC
            ENDIF
            ! Non-local density
            DENS = NLOC_DMG%DENS(IMAT)
            ! Number of the elements inside the group
            NEL  = IPARG(2,NG)
            ! Number of integration points in the R direction
            NPTR = ELBUF_TAB(NG)%NPTR
            ! Number of integration points in the S direction
            NPTS = ELBUF_TAB(NG)%NPTS
            ! Weight of integration in the plane of the shell
            WS   = ONE/(NPTS*NPTR)
            ! Number of integration points in the shell thickness
            NPTT = IPARG(6,NG) 
            ! Thickness of the shells
            THCK => ELBUF_TAB(NG)%GBUF%THK(1:NEL)
            ! Non-local in the thickness only if NPTT>1
            IF (NPTT>1) THEN
              ! Loop over integration points in the shell surface
              DO IR = 1, NPTR
                DO IS = 1, NPTS
                  BUFNL => ELBUF_TAB(NG)%NLOC(IR,IS)       
                  MASSTH => BUFNL%MASSTH   
                  ! Loop over integration points in the shell thickness
                  DO K = 1, NPTT
                    IF ((NPTT==2).AND.(K==2)) THEN
                      NTH1 = (Z01(K,NPTT)   - ZN1(K,NPTT))/
     .                   (ZN1(K-1,NPTT) - ZN1(K,NPTT))  
                      NTH2 = (Z01(K,NPTT) - ZN1(K-1,NPTT))/
     .                       (ZN1(K,NPTT) - ZN1(K-1,NPTT))  
                    ELSE
                      NTH1 = (Z01(K,NPTT)   - ZN1(K+1,NPTT))/
     .                       (ZN1(K,NPTT)   - ZN1(K+1,NPTT))  
                      NTH2 = (Z01(K,NPTT)   - ZN1(K,NPTT))/
     .                       (ZN1(K+1,NPTT) - ZN1(K,NPTT))  
                    ENDIF
                    ! Loop over elements
                    DO I=1,NEL                                   
                      IF ((NPTT==2).AND.(K==2)) THEN
                        MASSTH(I,K-1) = MASSTH(I,K-1) + 
     .                       (NTH1**2 + NTH1*NTH2)*DENS*AREA(NDEPAR+NFT+I)*THCK(I)*WS*WF1(K,NPTT)
                        MASSTH(I,K)   = MASSTH(I,K)   + 
     .                       (NTH2**2 + NTH1*NTH2)*DENS*AREA(NDEPAR+NFT+I)*THCK(I)*WS*WF1(K,NPTT)
                      ELSE
                        MASSTH(I,K)   = MASSTH(I,K)   + 
     .                       (NTH1**2 + NTH1*NTH2)*DENS*AREA(NDEPAR+NFT+I)*THCK(I)*WS*WF1(K,NPTT)
                        MASSTH(I,K+1) = MASSTH(I,K+1) + 
     .                       (NTH2**2 + NTH1*NTH2)*DENS*AREA(NDEPAR+NFT+I)*THCK(I)*WS*WF1(K,NPTT)
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
c
        ! Initialization of non-local fields and variables
        IF (ISIGI /= 0) THEN 
          WRITE(ISTDO,'(A)') ' .. NON-LOCAL FIELDS INITIALIZATION'
          CALL NLOCAL_INIT_STA(ELBUF_TAB,NLOC_DMG ,IPARG    ,IXC      ,
     .                         IXS      ,IXTG     ,AREA     ,X        ,
     .                         XREFS    ,XREFC    ,XREFTG   ,IPM      ,
     .                         BUFMAT   ) 
        ENDIF
c
       ENDIF
c
       ! Tables deallocation
       IF (ALLOCATED(TAGNOD))  DEALLOCATE(TAGNOD)
       IF (ALLOCATED(INDX))    DEALLOCATE(INDX)
       IF (ALLOCATED(IDXI))    DEALLOCATE(IDXI)
       IF (ALLOCATED(NDDL))    DEALLOCATE(NDDL)
       IF (ALLOCATED(NMAT))    DEALLOCATE(NMAT)
       IF (ALLOCATED(POSI))    DEALLOCATE(POSI)
       IF (ALLOCATED(INDEX))   DEALLOCATE(INDEX)
       IF (ALLOCATED(ITRI))    DEALLOCATE(ITRI)
       IF (ALLOCATED(TAGTET))  DEALLOCATE(TAGTET)
       IF (ALLOCATED(VOLN_S))  DEALLOCATE(VOLN_S)
       IF (ALLOCATED(VOLN_C))  DEALLOCATE(VOLN_C)
       IF (ALLOCATED(VOLN_TG)) DEALLOCATE(VOLN_TG)
       IF (ALLOCATED(VOLN))    DEALLOCATE(VOLN)
       IF (ALLOCATED(VOLU))    DEALLOCATE(VOLU)
       IF (ALLOCATED(WARN_LENGHT))  DEALLOCATE(WARN_LENGHT)
c-----------
 1800 FORMAT(
     . 5X,' NON-LOCAL PARAMETERS '/
     . 5X,'----------------------'/
     . 5X,' MATERIAL ID',5X, '      LENGTH',5X, 'CONV. LE_MAX',5X,'     DENSITY',5X,'     DAMPING'/
     . 5X,'            ',5X, '            ',5X, '            ',5X,'  (AUTO-SET)',5X,'  (AUTO-SET)'/)
 1900 FORMAT(
     . 5X,I12,5X,ES12.4,5X,ES12.4,5X,ES12.4,5X,ES12.4/)
      RETURN
      END
