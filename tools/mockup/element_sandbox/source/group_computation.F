Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!	------------------------------------
	SUBROUTINE GROUP_COMPUTATION(NBR_GROUP,IGROUP,PM,EINT,
     .             OFF,SIGY,DT1C,VOL0,GS,DPLA,PLA,FOR,SIGO,SIGN,DEP,NGL,THREAD_ID)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"

	
!	------------------------------------
        INTEGER, INTENT(IN) :: NBR_GROUP,THREAD_ID
        INTEGER, DIMENSION(*), INTENT(IN) :: NGL
        INTEGER, DIMENSION(11,NBR_GROUP), INTENT(IN) :: IGROUP
        my_real, INTENT(IN) :: PM(PMSIZE,*),EINT(*),
     .             OFF(*),SIGY(*),DT1C(*),VOL0(*),GS(*),
     .             DPLA(*),PLA(*)
        my_real, DIMENSION(*), INTENT(IN) :: SIGO,SIGN,DEP,FOR

!	------------------------------------
        INTEGER :: I,NG,ADFGR,NEL
        INTEGER :: JFT,JLT,IPLA,JTHE
        INTEGER :: IPT,NPTTOT,IMAT,ISRATE,MLW
!	------------------------------------

        !       computation for each group
!$OMP DO SCHEDULE(DYNAMIC,1)
        DO NG=1,NBR_GROUP

                NEL = IGROUP(1,ng)      !       number of element per group
                MLW = IGROUP(2,ng)      !       material law
                JFT = IGROUP(3,ng)      !       first element of the group
                JLT = IGROUP(4,ng)      !       last element of the group
                IMAT = ng               !       group number
                ADFGR = IGROUP(5,ng)    !       address of the first element of the group
                IPLA = IGROUP(6,ng)     !       plasticity
                JTHE = IGROUP(8,ng)     !       ???
                IPT = IGROUP(9,ng)      !       current integration point
                NPTTOT = IGROUP(10,ng)  !       total number of integration point
                ISRATE = IGROUP(11,ng)  !       ???


                CALL SHELL_COMPUTATION(JFT ,JLT ,IPLA  ,NEL,
     .                                 JTHE,NPTTOT,IMAT,ISRATE,
     .                                 PM  ,EINT(2*ADFGR+1),OFF(ADFGR+1)   ,SIGY(ADFGR+1),DT1C(ADFGR+1),
     .                                 VOL0(ADFGR+1),GS(ADFGR+1)  ,DPLA(ADFGR+1)  ,PLA(ADFGR+1),FOR(5*ADFGR+1),
     .                                 SIGO(5*ADFGR+1),SIGN(5*ADFGR+1),DEP(5*ADFGR+1),MLW,NGL(ADFGR+1))

        ENDDO
!$OMP END DO
	
        RETURN

!	------------------------------------
	END SUBROUTINE GROUP_COMPUTATION
!	------------------------------------
