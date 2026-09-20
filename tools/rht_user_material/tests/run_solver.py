# SPDX-License-Identifier: AGPL-3.0-or-later
"""Generate and run homogeneous one-brick OpenRadioss USER01 checks.

All node translations are prescribed. This isolates the constitutive response
from inertia/contact/hourglass calibration. Output directories are external.
"""
import argparse
import csv
import json
import math
import os
from pathlib import Path
import subprocess


def reals(values):
    return ''.join(f'{v:20.12E}' for v in values)


def ints(values):
    return ''.join(f'{v:10d}' for v in values)


def make_deck(folder, name, end_strain, endtime=1e-3):
    folder.mkdir(parents=True,exist_ok=True)
    c=[2314,1.67e10,1e6,2,0,0,3.527e10,1.6,0.61,3.5e7,0.18,0.1,
       0.6805,0.0105,0,3e-5,3e-6,3e25,3e25,0.032,0.036,0.001,
       0.53,0.70,0.5,0.04,1,0.01,1.6,0.61,0,3.527e10,3.958e10,9.04e9,
       2.33e7,6e9,3,1.1884]
    if name=='energy':
        c[4]=c[5]=1.22
    if name=='gruneisen':
        c[30]=1.1
    lines=['#RADIOSS STARTER','/BEGIN',name,ints([2022,0]),
           f'{"kg":>20}{"m":>20}{"s":>20}',f'{"kg":>20}{"m":>20}{"s":>20}',
           '/TITLE','RHT homogeneous one-brick verification','/MAT/USER01/1',
           'RHT published three-surface model',reals([c[0]])]
    for i in range(0,38,5):
        lines.append(reals(c[i:i+5]))
    lines+=['/NODE']
    xyz=[(0,0,0),(1,0,0),(1,1,0),(0,1,0),(0,0,1),(1,0,1),(1,1,1),(0,1,1)]
    for i,x in enumerate(xyz,1):
        lines.append(ints([i])+reals([v*0.01 for v in x]))
    lines+=['/PART/1','RHT brick',ints([1,1,0]),'/BRICK/1',ints([1,1,2,3,4,5,6,7,8]),
            '/PROP/SOLID/1','One integration point brick',
            ints([1,0,0,0,0,0,0,0])+reals([0]),reals([0,0,0]),reals([0])+ints([1])]
    # Prescribe zero and nonzero faces for every coordinate direction.
    for axis in range(3):
        for face in [0,1]:
            gid=2*axis+face+1
            nodes=[i for i,x in enumerate(xyz,1) if x[axis]==face]
            disp=face*end_strain[axis]*0.01
            points=[reals([endtime,disp])]
            if name=='unload':
                points=[reals([endtime/2,disp]),reals([endtime,0])]
            lines+=[f'/GRNOD/NODE/{gid}',f'Face {gid}',ints(nodes),f'/FUNCT/{gid}',
                    'Homogeneous displacement',reals([0,0]),*points,
                    f'/IMPDISP/{gid}','Prescribed face translation',
                    ints([gid])+f'{"XYZ"[axis]:>10}'+ints([0,0,gid,0]),reals([1,1,0,0])]
    lines+=['/TH/BRIC/1','Brick history','DEF',ints([1]),
            '/TH/BRIC/2','RHT history variables',''.join(f'{"USR"+str(i):>10}' for i in range(1,11)),
            ints([1]),'/TH/BRIC/3','RHT diagnostics',
            ''.join(f'{"USR"+str(i):>10}' for i in range(11,17)),ints([1]),'/END']
    starter=folder/f'{name}_0000.rad'
    starter.write_text('\n'.join(lines)+'\n',encoding='ascii')
    engine=folder/f'{name}_0001.rad'
    engine.write_text('\n'.join(['#RADIOSS ENGINE','/VERS/2022',f'/RUN/{name}/1',f'{endtime:.12E}',
                                 '/PRINT/-100','/DT','0.5 0.0','/TH/TITLE','/TFILE',f'{endtime/200:.12E}',
                                 '/ANIM/DT',f'0.0 {endtime/10:.12E}',
                                 '/ANIM/BRICK/TENS/STRESS','/ANIM/ELEM/PRES',
                                 '/ANIM/ELEM/USER1','/ANIM/ELEM/USER2','/ANIM/ELEM/USER3',
                                 '/ANIM/ELEM/USER4'])+'\n',encoding='ascii')
    return starter,engine


def cold_pressure(rho):
    """Independent pressure-space p-alpha root for monotonic cold compression."""
    lo,hi=0,1e12
    for _ in range(100):
        p=(lo+hi)/2
        alpha=1+0.1884*max(0,min(1,(6e9-p)/(6e9-2.33e7)))**3
        eta=alpha*rho/(1.1884*2314)-1
        residual=alpha*p-(3.527e10*eta+3.958e10*eta**2+9.04e9*eta**3)
        if residual>0:
            hi=p
        else:
            lo=p
    return (lo+hi)/2


def verify_csv(path, name, strain, endtime):
    with path.open(newline='') as f:
        reader=csv.reader(f)
        # The release converter can append a control byte to fixed-width
        # titles. Discard only those non-printable bytes, not column names.
        headers=[''.join(ch for ch in h if ch.isprintable()) for h in next(reader)]
        raw=[[float(x) for x in row] for row in reader]
    columns={h.split()[-1]:i for i,h in enumerate(headers) if h.startswith(('Brick history','RHT '))}
    def col(row,key):
        return row[columns[key]]
    rows=[r for r in raw if r[0]>0]
    assert len(rows)>=5,'Too few history samples'
    assert rows[-1][0]>=endtime*0.8,'Run did not reach the requested end time'
    assert all(math.isfinite(x) for r in rows for x in r),'Non-finite history output'
    pressure_errors=[]
    oldalpha,olddamage=1.1884,0
    for row in rows:
        t=row[0]
        fraction=t/endtime
        if name=='unload':
            fraction=2*fraction if fraction<=0.5 else 2*(1-fraction)
        rho=2314/math.prod(1+v*fraction for v in strain)
        assert abs(col(row,'DENS')-rho)<rho*2e-5,'Density/volume mapping mismatch'
        assert abs(col(row,'USR7')-rho)<rho*2e-5,'USER01 density mapping mismatch'
        alpha,damage=col(row,'USR1'),col(row,'USR3')
        assert 1-2e-6<=alpha<=oldalpha+2e-6,'Irreversible compaction violated'
        assert olddamage-2e-6<=damage<=1+2e-6,'Irreversible damage violated'
        assert abs(col(row,'PLAS')-col(row,'USR2'))<2e-6,'Standard plastic strain output mismatch'
        p=-(col(row,'SX')+col(row,'SY')+col(row,'SZ'))/3
        assert abs(p-col(row,'USR8'))<20+2e-6*abs(p),'Pressure output mismatch'
        q=math.sqrt(1.5*(sum((col(row,k)+p)**2 for k in ('SX','SY','SZ'))+
                          2*sum(col(row,k)**2 for k in ('SXY','SYZ','SXZ'))))
        assert abs(q-col(row,'USR10'))<30+1e-5*q,'Stress invariant output mismatch'
        if name=='hydro':
            expected=cold_pressure(rho)
            error=abs(p-expected)/max(3.5e7,abs(expected))
            pressure_errors.append(error)
            assert error<5e-4,'Hydrostatic pressure disagrees with the independent p-alpha root'
            assert q<100,'Hydrostatic loading produced a deviator'
        if name=='elastic':
            expected=(3.527e10/1.1884+4*1.67e10/3)*strain[0]*t/endtime
            assert abs(col(row,'SX')-expected)<0.0001*max(1,abs(expected)), 'Elastic modulus/shear convention mismatch'
            assert damage==0 and col(row,'USR2')==0,'Elastic test became plastic'
        if name in ('energy','gruneisen'):
            rhoelastic=rho*math.exp(col(row,'USR4'))
            eta=alpha*rhoelastic/(1.1884*2314)-1
            poly=3.527e10*eta+3.958e10*eta**2+9.04e9*eta**3
            e=col(row,'USR15')
            if name=='energy':
                expected=poly/alpha+(1.22+1.22*eta)*rho*e
            else:
                expected=poly*(1-1.1*eta/2)/alpha+1.1*rho*e
            assert abs(p-expected)<3.5e7*0.002+abs(p)*0.0001,'Energy-dependent EOS/energy units mismatch'
        oldalpha,olddamage=alpha,damage
    if name in ('tension','hydro_tension'):
        assert olddamage>0 and col(rows[-1],'USR4')>0,'Tensile plastic volume/damage not active'
    if name=='compression':
        assert olddamage>0 and oldalpha<1.1884,'Compressive compaction/damage not active'
    if name=='fast':
        assert max(col(r,'USR11') for r in rows)>1.1,'Plastic strain-rate branch not active'
    if name in ('energy','gruneisen'):
        assert col(rows[-1],'USR15')>0,'Energy coupling was not exercised'
    if name=='spall':
        assert olddamage==1,'Material did not reach complete tensile damage'
        assert max(abs(col(rows[-1],k)) for k in ('SX','SY','SZ'))<1,'Fully damaged tension retained stress'
        assert col(rows[-1],'OFF')==1,'Complete tensile damage incorrectly eroded the element'
    if name=='unload':
        peak=min(col(r,'USR1') for r in rows)
        assert peak<1.188 and abs(oldalpha-peak)<2e-6,'Unloading reopened compacted pores'
    return {'samples':len(rows),'last_time':rows[-1][0],
            'last_pressure':col(rows[-1],'USR8'),'last_alpha':oldalpha,
            'last_damage':olddamage,'max_rate_factor':max(col(r,'USR11') for r in rows),
            'max_hydro_pressure_relative_error':max(pressure_errors,default=0)}


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--radioss-root',type=Path,required=True)
    ap.add_argument('--library',type=Path,required=True)
    ap.add_argument('--output',type=Path,required=True)
    ap.add_argument('--case',default='all')
    args=ap.parse_args()
    root=args.radioss_root.resolve()
    out=args.output.resolve()
    lib=args.library.resolve()
    out.mkdir(parents=True,exist_ok=True)
    win=os.name=='nt'
    arch='win64' if win else 'linux64'
    suffix='.exe' if win else ''
    starter=root/'exec'/('starter_'+arch+suffix if win else 'starter_linux64_gf')
    engine=root/'exec'/('engine_'+arch+suffix if win else 'engine_linux64_gf')
    env=os.environ.copy()
    env['RAD_CFG_PATH']=str(root/'hm_cfg_files')
    env['RAD_H3D_PATH']=str(root/'extlib/h3d/lib'/arch)
    env['OMP_NUM_THREADS']='1'
    env['KMP_STACKSIZE']='100m'
    paths=[root/'extlib/hm_reader'/arch,root/'extlib/intelOneAPI_runtime'/arch]
    if win:
        env['PATH']=os.pathsep.join(map(str,paths))+os.pathsep+env.get('PATH','')
    else:
        env['LD_LIBRARY_PATH']=os.pathsep.join(map(str,paths))+os.pathsep+env.get('LD_LIBRARY_PATH','')
    cases={'elastic':([1e-7,0,0],1e-4),
           'hydro':([-0.08]*3,1e-3),
           'compression':([-0.04,0,0],1e-3),
           'tension':([0.002,0,0],1e-3),
           'hydro_tension':([0.002]*3,1e-3),
           'fast':([-0.01,0,0],1e-5),
           'energy':([-0.04,0,0],1e-3),
           'gruneisen':([-0.04,0,0],1e-3),
           'unload':([-0.04]*3,2e-3),
           'spall':([0.02]*3,1e-3)}
    results=[]
    for name,(strain,endtime) in cases.items():
        if args.case not in ('all',name):
            continue
        folder=out/name
        st,en=make_deck(folder,name,strain,endtime)
        entry={'case':name}
        for role,exe,deck in [('starter',starter,st),('engine',engine,en)]:
            cmd=[str(exe),'-i',deck.name,'-dylib',str(lib),'-np','1']
            if role=='engine':
                cmd=cmd[:-2]+['-nt','1']
            proc=subprocess.run(cmd,cwd=folder,env=env,stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,timeout=120)
            log=proc.stdout.decode('utf-8',errors='replace')
            (folder/f'{role}.log').write_text(log,encoding='utf-8')
            entry[role+'_returncode']=proc.returncode
            print(name,role,proc.returncode,flush=True)
            if proc.returncode:
                print(log[-4000:])
                break
        results.append(entry)
        if entry.get('engine_returncode',1)==0:
            try:
                assert 'NORMAL TERMINATION' in (folder/'engine.log').read_text(), 'Engine did not terminate normally'
                converter=root/'exec'/('th_to_csv_'+arch+suffix)
                subprocess.run([str(converter),name+'T01'],cwd=folder,env=env,
                               stdout=subprocess.PIPE,stderr=subprocess.STDOUT,check=True)
                entry['checks']=verify_csv(folder/(name+'T01.csv'),name,strain,endtime)
                entry['verified']=True
            except (AssertionError,ValueError,KeyError,IndexError,OSError,subprocess.CalledProcessError) as exc:
                entry['verified']=False
                entry['verification_error']=str(exc)
                print(name,'verification failed:',exc,flush=True)
    (out/'run_summary.json').write_text(json.dumps(results,indent=2),encoding='utf-8')
    if not results or any(not r.get('verified',False) for r in results):
        raise SystemExit(1)


if __name__=='__main__':
    main()
