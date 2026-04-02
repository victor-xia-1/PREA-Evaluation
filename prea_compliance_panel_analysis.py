import re
from pathlib import Path
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

EXCEL_PATH = Path('/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx')
OUT_DIR = Path('/Users/victorxia/Documents/Honors Thesis/data/clean')
OUT_DIR.mkdir(parents=True, exist_ok=True)

YEARS = list(range(2012, 2021))
COMPLIANCE_YEARS = list(range(2014, 2021))

STATES_50_DC = {
    'Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware',
    'District Of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa',
    'Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
    'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
    'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
    'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington',
    'West Virginia','Wisconsin','Wyoming'
}


def normalize_state(x: object) -> str | None:
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return None
    s = str(x).strip()
    if not s:
        return None
    s = re.sub(r'/[a-z].*$', '', s, flags=re.IGNORECASE)  # remove footnote suffixes like Georgia/b
    s = re.sub(r'\s+', ' ', s)
    s = s.replace('D.C.', 'District Of Columbia').replace('D C', 'District Of Columbia')
    s = s.title()
    return s


def to_num(v: object) -> float:
    if v is None or (isinstance(v, float) and np.isnan(v)):
        return np.nan
    s = str(v).strip()
    if s in {'', '/', 'nan', 'NaN'}:
        return np.nan
    s = re.sub(r'[^0-9.\-]', '', s)
    if s == '':
        return np.nan
    try:
        return float(s)
    except ValueError:
        return np.nan


def build_compliance_long() -> pd.DataFrame:
    df = pd.read_excel(EXCEL_PATH, sheet_name='PREA_Certification_vs_Assurance')
    cols = ['State'] + [y for y in COMPLIANCE_YEARS if y in df.columns]
    c = df[cols].copy()
    c['state'] = c['State'].map(normalize_state)
    c = c[c['state'].isin(STATES_50_DC)]

    for y in COMPLIANCE_YEARS:
        c[f'comp_{y}'] = (c[y].astype(str).str.strip() == '1').astype(int)

    out_rows = []
    for _, row in c.iterrows():
        cum = 0
        for y in YEARS:
            if y in COMPLIANCE_YEARS:
                cum += int(row[f'comp_{y}'])
            out_rows.append({'state': row['state'], 'year': y, 'cum_compliance_years': cum})
    out = pd.DataFrame(out_rows)
    return out


def extract_year_counts(year: int) -> pd.DataFrame:
    raw = pd.read_excel(EXCEL_PATH, sheet_name=str(year), header=None)

    header_idx = None
    for i in range(min(30, len(raw))):
        if raw.iloc[i].astype(str).str.contains('Jurisdiction', case=False, na=False).any():
            header_idx = i
            break
    if header_idx is None:
        raise ValueError(f'Could not find header row in sheet {year}')

    rows = []
    for i in range(header_idx + 1, len(raw)):
        st = normalize_state(raw.iloc[i, 1] if raw.shape[1] > 1 else None)
        if st is None:
            continue
        if st in {'Total', 'Federal', 'State'}:
            continue
        if st not in STATES_50_DC:
            continue

        # Nonconsensual sexual acts + abusive sexual contact
        noncons_alleged = to_num(raw.iloc[i, 4]) if raw.shape[1] > 4 else np.nan
        noncons_subst = to_num(raw.iloc[i, 5]) if raw.shape[1] > 5 else np.nan
        abusive_alleged = to_num(raw.iloc[i, 7]) if raw.shape[1] > 7 else np.nan
        abusive_subst = to_num(raw.iloc[i, 8]) if raw.shape[1] > 8 else np.nan

        alleged_count = np.nansum([noncons_alleged, abusive_alleged])
        substantiated_count = np.nansum([noncons_subst, abusive_subst])

        # if both components missing, keep missing
        if np.isnan(noncons_alleged) and np.isnan(abusive_alleged):
            alleged_count = np.nan
        if np.isnan(noncons_subst) and np.isnan(abusive_subst):
            substantiated_count = np.nan

        rows.append({
            'state': st,
            'year': year,
            'alleged_count': alleged_count,
            'substantiated_count': substantiated_count,
        })

    return pd.DataFrame(rows)


def run_models(panel: pd.DataFrame):
    models = {}
    for dep in ['alleged_count', 'substantiated_count']:
        d = panel.dropna(subset=[dep, 'cum_compliance_years']).copy()

        pooled = smf.ols(f'{dep} ~ cum_compliance_years', data=d).fit(
            cov_type='cluster', cov_kwds={'groups': d['state']}
        )
        fe = smf.ols(f'{dep} ~ cum_compliance_years + C(state) + C(year)', data=d).fit(
            cov_type='cluster', cov_kwds={'groups': d['state']}
        )

        models[dep] = {'pooled': pooled, 'fe': fe, 'n': len(d)}
    return models


def tidy_result(model, dep, spec):
    coef = model.params.get('cum_compliance_years', np.nan)
    se = model.bse.get('cum_compliance_years', np.nan)
    p = model.pvalues.get('cum_compliance_years', np.nan)
    return {
        'dependent': dep,
        'spec': spec,
        'coef_cum_compliance_years': coef,
        'std_err': se,
        'p_value': p,
        'n_obs': int(model.nobs),
        'r_squared': model.rsquared,
    }


def main():
    counts = pd.concat([extract_year_counts(y) for y in YEARS], ignore_index=True)
    compliance = build_compliance_long()
    panel = counts.merge(compliance, on=['state', 'year'], how='inner')

    # keep a balanced state-year grid for panel transparency
    full_grid = pd.MultiIndex.from_product([sorted(STATES_50_DC), YEARS], names=['state', 'year']).to_frame(index=False)
    panel_full = full_grid.merge(panel, on=['state', 'year'], how='left')

    panel_out = OUT_DIR / 'prea_panel_state_year_2012_2020.csv'
    panel_full.to_csv(panel_out, index=False)

    models = run_models(panel)

    rows = []
    for dep, res in models.items():
        rows.append(tidy_result(res['pooled'], dep, 'pooled_ols_cluster_state'))
        rows.append(tidy_result(res['fe'], dep, 'state_year_fe_cluster_state'))

    res_df = pd.DataFrame(rows)
    res_out = OUT_DIR / 'prea_compliance_panel_regression_results.csv'
    res_df.to_csv(res_out, index=False)

    print('Panel rows used (non-missing dep var):')
    for dep, res in models.items():
        print(f'  {dep}: {res["n"]}')

    print('\nKey coefficient: cum_compliance_years')
    for _, r in res_df.iterrows():
        print(
            f"{r['dependent']} | {r['spec']} | coef={r['coef_cum_compliance_years']:.4f} "
            f"se={r['std_err']:.4f} p={r['p_value']:.4g} R2={r['r_squared']:.4f} N={int(r['n_obs'])}"
        )

    print('\nSaved:')
    print(panel_out)
    print(res_out)


if __name__ == '__main__':
    main()
