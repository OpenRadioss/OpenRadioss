//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software 
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss 
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the 
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.    
#define _FCALL
#include <list>
#include <vector>
#include <algorithm>
#include <cmath>
#include <deque>
#include <fstream>
#include <iostream>
#include <limits>
#include <cfloat>

#define NDEBUG
#include <assert.h>

// to be consistent with Fortran
// But template would be better design
#ifndef MYREAL4
#define MY_REAL double
#else
#define MY_REAL float
#endif

struct Velocity
{
	MY_REAL vx = 0,vy = 0,vz = 0;
};

bool compareVel(Velocity a, Velocity b)
{
	if(a.vx != b.vx) return (a.vx > b.vx);
	if(a.vy != b.vy) return (a.vy > b.vy);
	return (a.vz > b.vz);
}

bool areVelEqual(Velocity &a, Velocity &b)
{
	return (a.vx == b.vx && a.vy == b.vy && a.vz == b.vz);
}

// =============================================================================
// find the global velocity of a set of nodes
// The global velocity is computed at the most frequent velocity among the nodes
// In :
//  v -  vector of nodal velocity
// out:: 
//  v1,v2,v3 - directional velocities
//  freq - frequency of the velocity
// =============================================================================
void globalVelocity(std::vector<Velocity> & v, MY_REAL &v1, MY_REAL &v2, MY_REAL &v3, int & freq)
{
	std::sort(v.begin(),v.end(),compareVel);
	Velocity most_frequent_velocity;
	Velocity current_velocity;
	size_t max_frequency = 0;
	size_t current_frequency = 0;

	most_frequent_velocity.vx = FLT_MAX;
	most_frequent_velocity.vy = FLT_MAX;
	most_frequent_velocity.vz = FLT_MAX;
	current_velocity.vx = FLT_MAX;
	current_velocity.vy = FLT_MAX;
	current_velocity.vz = FLT_MAX;

	for(auto it = v.begin() ; it != v.end(); it++)
	{
		if(areVelEqual(current_velocity,*it))
		{
			current_frequency ++;
		}else
		{ // new velocity
			if(max_frequency < current_frequency)
			{ // save old velocity if it is the most frequent so far
				max_frequency = current_frequency;
				most_frequent_velocity.vx = current_velocity.vx;
				most_frequent_velocity.vy = current_velocity.vy;
				most_frequent_velocity.vz = current_velocity.vz;
			}
			current_frequency = 1;
			current_velocity.vx = (*it).vx;
			current_velocity.vy = (*it).vy;
			current_velocity.vz = (*it).vz;
		}
	}

	if(max_frequency < current_frequency)
	{ // save old velocity if it is the most frequent so far
		max_frequency = current_frequency;
		most_frequent_velocity.vx = current_velocity.vx;
		most_frequent_velocity.vy = current_velocity.vy;
		most_frequent_velocity.vz = current_velocity.vz;
	}
	v1 = most_frequent_velocity.vx;
	v2 = most_frequent_velocity.vy;
	v3 = most_frequent_velocity.vz;
	freq = max_frequency;
}

extern "C" {
void c_compute_velocity_(MY_REAL *v, int * numnod, int *idx, int *size_idx, MY_REAL * v1, int * freq)
{
	std::vector<Velocity> vel;
	vel.resize(*size_idx);
	for(size_t i = 0; i < (size_t) *size_idx ; i++)
	{
                const int index_in_v = idx[i]-1;
		assert(index_in_v >= 0);
		assert(index_in_v < *numnod);
		vel[i].vx = v[3*index_in_v];
		vel[i].vy = v[3*index_in_v+1];
		vel[i].vz = v[3*index_in_v+2];

	}
        int f = 0;
	MY_REAL vx,vy,vz;
        globalVelocity(vel,vx,vy,vz,f);
	v1[0] = vx;
	v1[1] = vy;
	v1[2] = vz;
        *freq = f;
}
void _FCALL C_COMPUTE_VELOCITY(MY_REAL *v, int * numnod, int *idx, int *size_idx, MY_REAL * v1, int * freq)
{
	c_compute_velocity_(v, numnod, idx, size_idx, v1, freq);
}
}

