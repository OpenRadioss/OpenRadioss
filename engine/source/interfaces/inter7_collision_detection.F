Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INTER7_COLLISION_DETECTION_MODsource/interfaces/inter7_collision_detection.F
Chd|-- called by -----------
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|====================================================================
       MODULE INTER7_COLLISION_DETECTION_MOD
       CONTAINS
Chd|====================================================================
Chd|  INTER7_COLLISION_DETECTION    source/interfaces/inter7_collision_detection.F
Chd|-- called by -----------
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTER7_CANDIDATE_PAIRS        source/interfaces/intsort/inter7_candidate_pairs.F
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        INTER7_CANDIDATE_PAIRS_MOD    source/interfaces/intsort/inter7_candidate_pairs.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE INTER7_COLLISION_DETECTION(
     1   X        ,IRECT   ,NSV     ,INACTI   ,CAND_P  ,
     2   NMN      ,NRTM    ,NSN     ,CAND_E   ,CAND_N  ,
     3   GAP      ,NOINT   ,II_STOK ,NCONTACT ,BMINMA  ,
     4   TZINF    ,CAND_A,CURV_MAX,
     5   NB_N_B   ,ESHIFT  ,ILD     ,IFQ      ,IFPEN   ,
     8   STFN     ,STF     ,IGAP     ,GAP_S   ,
     A   NSNR     ,NCONT   ,RENUM   ,NSNROLD  ,GAP_M   ,
     B   GAPMIN   ,GAPMAX  ,NUM_IMP,GAP_S_L ,
     C   GAP_M_L  ,ITASK   ,BGAPSMX  ,I_MEM   ,  
     D   KREMNOD  ,REMNOD  ,FLAGREMNODE, DRAD ,
     E   ITIED    ,CAND_F  ,DGAPLOAD,
     F   TOTAL_NB_NRTM, s_cand_a,
     F   S_KREMNOD, S_REMNOD)

C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE INTER7_CANDIDATE_PAIRS_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN,NSN, NOINT,INACTI,IFQ,  NSNR, NSNROLD
      INTEGER, INTENT(in) :: NRTM !< number of segments per threads
      INTEGER, INTENT(in) :: TOTAL_NB_NRTM !< total number of segments
      integer, intent(in) :: s_cand_a
      INTEGER IRECT(4,*),NSV(*),CAND_A(s_cand_a), RENUM(*),NUM_IMP, ITASK
      INTEGER CAND_E(*),CAND_N(*),IFPEN(*),KREMNOD(*),REMNOD(*)
      INTEGER NCONTACT,ESHIFT,ILD,NB_N_B, IGAP, NCONT,I_MEM,
     *        II_STOK, FLAGREMNODE, ITIED
      INTEGER, intent(in) :: S_KREMNOD, S_REMNOD

C     REAL
      my_real
     .   GAP,TZINF,
     .   GAPMIN, GAPMAX, BMINMA(12),CURV_MAX(NRTM),BGAPSMX
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      my_real
     .   X(3,*), CAND_P(*), STFN(*),
     .   STF(*), GAP_S(*), GAP_M(*), 
     .   GAP_S_L(*), GAP_M_L(*), CAND_F(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,
     .        N, S_PREV_REMOTE_NUMBER,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6,2), MARGE, AAA
      INTEGER NBX,NBY,NBZ ! number of cells in each direction
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C-----INITIALISATION 
C----- Get bounds of the domains
C
      XYZM(1,1) = BMINMA(4)
      XYZM(2,1) = BMINMA(5)
      XYZM(3,1) = BMINMA(6)
      XYZM(4,1) = BMINMA(1)
      XYZM(5,1) = BMINMA(2)
      XYZM(6,1) = BMINMA(3)
      XYZM(1,2) = BMINMA(10)
      XYZM(2,2) = BMINMA(11)
      XYZM(3,2) = BMINMA(12)
      XYZM(4,2) = BMINMA(7)
      XYZM(5,2) = BMINMA(8)
      XYZM(6,2) = BMINMA(9)
      I_MEM = 0
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.
     .   IFQ>0.OR.NUM_IMP>0.OR.ITIED/=0) THEN
        S_PREV_REMOTE_NUMBER = NSNR
      ELSE
        S_PREV_REMOTE_NUMBER = 0
      END IF
      MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD) ! margin 
C Work on the reduced box
      IF( NMN /= 0 ) THEN
        AAA = SQRT(NMN /
     .           ((BMINMA(7)-BMINMA(10))*(BMINMA(8)-BMINMA(11))
     .           +(BMINMA(8)-BMINMA(11))*(BMINMA(9)-BMINMA(12))
     .           +(BMINMA(9)-BMINMA(12))*(BMINMA(7)-BMINMA(10))))
      ELSE
        AAA = 0
      ENDIF

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(7)-BMINMA(10)))
      NBY = NINT(AAA*(BMINMA(8)-BMINMA(11)))
      NBZ = NINT(AAA*(BMINMA(9)-BMINMA(12)))
C
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)
     
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF
      
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      ENDIF

      ! redundancy in smp
!$OMP SINGLE
      DO I=1,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
!$OMP END SINGLE NOWAIT
      IF(ITASK == 0) THEN 
         ALLOCATE(NEXT_NOD(NSN+NSNR))
         ALLOCATE(PREV_REMOTE_NUMBER(S_PREV_REMOTE_NUMBER))
         ! find the old id of remote candidate nodes (inactive, ifq, itied)
       if(nspmd>1.and.(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)) then
         CALL SPMD_OLDNUMCD(RENUM,PREV_REMOTE_NUMBER,S_PREV_REMOTE_NUMBER,NSNROLD)
       end if
      endif
      call MY_BARRIER

       CALL INTER7_CANDIDATE_PAIRS(
     1   NSN     ,PREV_REMOTE_NUMBER ,NSNR     ,S_PREV_REMOTE_NUMBER  ,I_MEM   ,
     2   IRECT   ,X        ,STF      ,STFN     ,XYZM    ,
     3   NSV     ,II_STOK  ,CAND_N   ,ESHIFT   ,CAND_E  ,
     4   NCONTACT,TZINF    ,GAP_S_L  ,GAP_M_L ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ      ,
     6   INACTI  ,IFQ      ,CAND_A,CAND_P   ,IFPEN   ,
     7   NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,
     8   GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,
     9   ITASK    ,BGAPSMX  ,S_KREMNOD, KREMNOD  ,S_REMNOD, REMNOD  ,
     A   FLAGREMNODE,DRAD   ,ITIED    ,CAND_F  ,
     B   DGAPLOAD, s_cand_a, 
     C   TOTAL_NB_NRTM,  NUMNOD, XREM, SIZE(XREM,1), 
     D   IREM, size(irem,1), NEXT_NOD)

        IF(ITASK==0)  THEN
            IF(ALLOCATED(NEXT_NOD)) DEALLOCATE(NEXT_NOD)
            IF(ALLOCATED(PREV_REMOTE_NUMBER)) DEALLOCATE(PREV_REMOTE_NUMBER)

        ENDIF
C     I_MEM = 2 ==> Not enough memory
      IF (I_MEM ==2) RETURN
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM==2) THEN
        IF(DEBUG(1)>=1) THEN
          IWARN = IWARN+1
#include "lockon.inc"
          WRITE(ISTDO,*)' **WARNING INTERFACE/MEMORY'
          WRITE(IOUT,*)' **WARNING INTERFACE NB:',NOINT
          WRITE(IOUT,*)'       TOO MANY POSSIBLE IMPACTS'
          WRITE(IOUT,*)'       SIZE OF INFLUENCE ZONE IS'
          WRITE(IOUT,*)'       MULTIPLIED BY 0.75'
#include "lockoff.inc"
        ENDIF
        RETURN
        TZINF = THREE_OVER_4*TZINF
C taille de boite non diminuee
C        MINBOX= THREE_OVER_4*MINBOX
C        MAXBOX= THREE_OVER_4*MAXBOX
        IF( TZINF<=MAX(GAP+DGAPLOAD,DRAD)  ) THEN
          CALL ANCMSG(MSGID=98,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I7BUCE)')
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ELSEIF(I_MEM==3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B > NCONT) THEN
          CALL ANCMSG(MSGID=100,ANMODE=ANINFO,
     .            I1=NOINT)
          CALL ARRET(2)
        ENDIF
        ILD = 1
      ENDIF
C
      RETURN
      END
      END MODULE
