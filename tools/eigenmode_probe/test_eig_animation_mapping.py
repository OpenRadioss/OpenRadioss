"""Regression checks for the modal GENANI argument mapping.

The eigen animation wrapper deliberately mirrors GENANI's positional legacy
interface. These checks protect the resident Engine arrays from being
replaced, shifted, or accidentally mapped back to scalar scratch storage.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RESOL = ROOT / "engine/source/engine/resol.F"
EIG_ANIM = ROOT / "engine/source/implicit/eig_animation_output.F"
GENANI = ROOT / "engine/source/output/anim/generate/genani.F"


def _fixed_form_source(path: Path) -> str:
    source = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line and line[0] in "Cc*!":
            continue
        if line.startswith("#"):
            continue
        source.append(line[6:] if len(line) > 6 else "")
    return "\n".join(source)


def _arguments(path: Path, marker: str, occurrence: int = 0) -> list[str]:
    source = _fixed_form_source(path)
    upper = source.upper()
    position = -1
    for _ in range(occurrence + 1):
        position = upper.index(marker.upper(), position + 1)
    opening = source.index("(", position + len(marker))
    depth = 0
    start = opening + 1
    arguments = []
    for index in range(opening, len(source)):
        character = source[index]
        if character == "(":
            depth += 1
        elif character == ")":
            depth -= 1
            if depth == 0:
                arguments.append(source[start:index])
                break
        elif character == "," and depth == 1:
            arguments.append(source[start:index])
            start = index + 1
    return ["".join(argument.split()).upper() for argument in arguments]


def test_every_former_dummy_uses_its_resident_resol_array() -> None:
    eig_formals = _arguments(EIG_ANIM, "SUBROUTINE EIG_ANIMATION_OUTPUT")
    resol_actuals = _arguments(RESOL, "CALL EIG_ANIMATION_OUTPUT")
    genani_formals = _arguments(GENANI, "SUBROUTINE GENANI")
    eig_genani_actuals = _arguments(EIG_ANIM, "CALL GENANI")

    assert len(eig_formals) == len(resol_actuals)
    assert len(genani_formals) == len(eig_genani_actuals)

    resol_by_eig_formal = dict(zip(eig_formals, resol_actuals, strict=True))
    eig_actual_by_genani_formal = dict(
        zip(genani_formals, eig_genani_actuals, strict=True)
    )

    expected_resol_actual = {
        "IFLOW": "IFLOW",
        "RFLOW": "RFLOW",
        "DIAG_SMS": "DIAG_SMS",
        "FNCONT2": "OUTPUT%DATA%VECT_CONT2",
        "DXANCG": "DXANCG",
        "NOD_PXFEM": "INOD_PXFEM",
        "IEL_PXFEM": "IEL_PXFEM",
        "ZI_PLY": "ZI_PLY",
        "VGAZ": "VFLOW",
        "FCONTG": "FCONTG",
        "FNCONTG": "FNCONTG",
        "FTCONTG": "FTCONTG",
        "FANREAC": "FREAC",
        "INOD_CRK": "INOD_CRK",
        "IEL_CRK": "IEL_CRK",
        "ELCUTC": "ELCUTC",
        "IADC_CRK": "IADC_CRK",
        "PDAMA2": "OUTPUT%DATA%SCAL_DAMA2",
        "RES_SMS": "RES_SMS",
        "NODGLOBXFE": "NODGLOBXFE",
        "NODEDGE": "NODEDGE",
        "KXIG3D": "KXIG3D",
        "IXIG3D": "IXIG3D",
        "KNOT": "KNOT",
        "WIGE": "WIGE",
        "KNOTLOCPC": "KNOTLOCPC",
        "KNOTLOCEL": "KNOTLOCEL",
    }

    observed = {}
    for genani_formal, expected in expected_resol_actual.items():
        eig_formal = eig_actual_by_genani_formal[genani_formal]
        assert eig_formal.isidentifier(), (
            f"GENANI {genani_formal} must receive a named wrapper argument, "
            f"not {eig_formal}"
        )
        observed[genani_formal] = resol_by_eig_formal[eig_formal]
        assert observed[genani_formal] == expected

    assert len(observed) == 27


def test_genani_scalar_flag_is_not_an_array_placeholder() -> None:
    genani_formals = _arguments(GENANI, "SUBROUTINE GENANI")
    eig_genani_actuals = _arguments(EIG_ANIM, "CALL GENANI")
    mapping = dict(zip(genani_formals, eig_genani_actuals, strict=True))

    assert mapping["SIG3DSOLID"] == "SIG3DSOLID"
