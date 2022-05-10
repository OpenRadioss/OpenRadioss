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
#if defined(_OPENMP)

#if CPP_mach == CPP_pwr4
        CALL OMP_UNSET_LOCK_(LLOCK(1,1))
#elif 1
        CALL OMP_UNSET_LOCK(LLOCK(1,1))
#endif

#elif CPP_mach == CPP_sgi3 || CPP_mach == CPP_sgi4 || CPP_mach == CPP_sgi6
        CALL MP_UNSETLOCK
#elif CPP_mach == CPP_ppc
        IERRLO = f_pthread_mutex_unlock(LLOCK(1,1))
#elif CPP_mach == CPP_c90 || CPP_mach == CPP_ymp || CPP_mach == CPP_t90 || CPP_mach == CPP_t90_i3e
        CALL LOCKOFF(LLOCK(1,1))
#elif CPP_mach == CPP_sx4 || CPP_mach == CPP_sx4_i3e
        CALL PLUNLOCK(LLOCK(1,1))
#elif CPP_mach == CPP_deca_mp
        CALL mppxcs(0)
#elif CPP_mach == CPP_hp11
        IERRLO = UNLOCK_GATE(LLOCK(1,1))
#endif
