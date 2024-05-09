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
! 
!  ______     ______     _____     __     ______     ______     ______    
! /\  == \   /\  __ \   /\  __-.  /\ \   /\  __ \   /\  ___\   /\  ___\
! \ \  __<   \ \  __ \  \ \ \/\ \ \ \ \  \ \ \/\ \  \ \___  \  \ \___  \
!  \ \_\ \_\  \ \_\ \_\  \ \____-  \ \_\  \ \_____\  \/\_____\  \/\_____\
!   \/_/ /_/   \/_/\/_/   \/____/   \/_/   \/_____/   \/_____/   \/_____/
!
!                                                      ____S_T_A_R_T_E_R
!_______________________________________________________________________

       PROGRAM STARTER

       USE QA_OUT_MOD

       CALL QAOPEN('starter')
       CALL QAPRINT('Start',0,0.0_8)

       CALL SET_STACK_VALUES()  ! Initialize Stacksize & KMP_STACKSIZE

       CALL STARTER0()

       END
!_______________________________________________________________________
