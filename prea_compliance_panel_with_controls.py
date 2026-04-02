import re
from pathlib import Path
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

BASE = Path('/Users/victorxia/Documents/Honors Thesis')
EXCEL_PATH = BASE / 'PREA Data 102125.xlsx'
OUT_DIR = BASE / 'data/clean'
OUT_DIR.mkdir(parents=True, exist_ok=True)

STAFF_PATH = OUT_DIR / 'state_correctional_officer_employment_long.csv'
CRIME_PATH = OUT_DIR / 'fbi_ucr_violent_crime_rates_panel.csv'

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

# 2020 Census state populations in thousands (50 states + DC)
STATE_POP_2020_K = {
    'Alabama': 5024, 'Alaska': 733, 'Arizona': 7151, 'Arkansas': 3012, 'California': 39538,
    'Colorado': 5774, 'Connecticut': 3606, 'Delaware': 990, 'District Of Columbia': 689,
    'Florida': 21538, 'Georgia': 10712, 'Hawaii': 1452, 'Idaho': 1838, 'Illinois': 12821,
    'Indiana': 6786, 'Iowa': 3190, 'Kansas': 2938, 'Kentucky': 4506, 'Louisiana': 4658,
    'Maine': 1362, 'Maryland': 6177, 'Massachusetts': 7029, 'Michigan': 10037,
    'Minnesota': 5706, 'Mississippi': 2961, 'Missouri': 6155, 'Montana': 1084, 'Nebraska': 1962,
    'Nevada': 3101, 'New Hampshire': 1378, 'New Jersey': 9289, 'New Mexico': 2118,
    'New York': 20201, 'North Carolina': 10439, 'North Dakota': 779, 'Ohio': 11799,
    'Oklahoma': 3959, 'Oregon': 4237, 'Pennsylvania': 13003, 'Rhode Island': 1097,
    'South Carolina': 5118, 'South Dakota': 886, 'Tennessee': 6911, 'Texas': 29146, 'Utah': 3272,
    'Vermont': 643, 'Virginia': 8630, 'Washington': 7705, 'West Virginia': 1794,
    'Wisconsin': 5894, 'Wyoming': 577,
}


def normalize_state(x: object) -> str | None:
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return None
    s = str(x).strip()
    if not s:
        return None

    s = re.sub(r'/[a-z].*$', '', s, flags=re.IGNORECASE)
    s = re.sub(r'\d+[\d,\s]*$', '', s).strip()
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
    return pd.DataFrame(out_rows)


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
        if st is None or st in {'Total', 'Federal', 'State'} or st not in STATES_50_DC:
            continue

        noncons_alleged = to_num(raw.iloc[i, 4]) if raw.shape[1] > 4 else np.nan
        noncons_subst = to_num(raw.iloc[i, 5]) if raw.shape[1] > 5 else np.nan
        abusive_alleged = to_num(raw.iloc[i, 7]) if raw.shape[1] > 7 else np.nan
        abusive_subst = to_num(raw.iloc[i, 8]) if raw.shape[1] > 8 else np.nan
        prisoners = to_num(raw.iloc[i, 2]) if raw.shape[1] > 2 else np.nan

        alleged_count = np.nansum([noncons_alleged, abusive_alleged])
        substantiated_count = np.nansum([noncons_subst, abusive_subst])
        if np.isnan(noncons_alleged) and np.isnan(abusive_alleged):
            alleged_count = np.nan
        if np.isnan(noncons_subst) and np.isnan(abusive_subst):
            substantiated_count = np.nan

        rows.append(
            {
                'state': st,
                'year': year,
                'alleged_count': alleged_count,
                'substantiated_count': substantiated_count,
                'prisoners': prisoners,
            }
        )

    return pd.DataFrame(rows)


def load_staffing_controls() -> pd.DataFrame:
    staff = pd.read_csv(STAFF_PATH)
    staff['state'] = staff['state'].map(normalize_state)
    staff['year'] = pd.to_numeric(staff['year'], errors='coerce')
    staff['tot_emp'] = pd.to_numeric(staff['tot_emp'], errors='coerce')
    staff = staff[(staff['state'].isin(STATES_50_DC)) & (staff['year'].isin(YEARS))]
    staff = staff[['state', 'year', 'tot_emp']].dropna()
    staff['log_tot_emp'] = np.log(staff['tot_emp'])
    return staff


def load_crime_controls() -> pd.DataFrame:
    crime = pd.read_csv(CRIME_PATH)
    crime['state'] = crime['state'].map(normalize_state)
    crime['year'] = pd.to_numeric(crime['year'], errors='coerce')
    crime['violent_crime_rate_per_100k'] = pd.to_numeric(crime['violent_crime_rate_per_100k'], errors='coerce')
    crime = crime[(crime['state'].isin(STATES_50_DC)) & (crime['year'].isin(YEARS))]
    crime = crime[['state', 'year', 'violent_crime_rate_per_100k']].dropna()

    # If a state-year appears more than once because of source footnote artifacts, keep the median value.
    crime = (
        crime.groupby(['state', 'year'], as_index=False)['violent_crime_rate_per_100k']
        .median()
    )
    return crime


def load_population_controls() -> pd.DataFrame:
    pop = pd.DataFrame(
        {'state': list(STATE_POP_2020_K.keys()), 'population_millions': [v / 1000.0 for v in STATE_POP_2020_K.values()]}
    )
    return pop


def fit_models(panel: pd.DataFrame, dep: str):
    d = panel.dropna(subset=[dep, 'cum_compliance_years']).copy()

    # Baseline TWFE on all rows with outcome + compliance
    twfe_base = smf.ols(f'{dep} ~ cum_compliance_years + C(state) + C(year)', data=d).fit(
        cov_type='cluster', cov_kwds={'groups': d['state']}
    )

    # Controlled model with year FE: identifies population + violent crime controls.
    c = d.dropna(subset=['violent_crime_rate_per_100k', 'population_millions', 'staff_per_1000_inmates']).copy()
    year_fe_ctrl = smf.ols(
        (
            f'{dep} ~ cum_compliance_years + violent_crime_rate_per_100k + '
            f'population_millions + staff_per_1000_inmates + C(year)'
        ),
        data=c,
    ).fit(cov_type='cluster', cov_kwds={'groups': c['state']})

    # TWFE controlled model (population omitted because it is time-invariant and absorbed by state FE).
    twfe_vcrime_ctrl = smf.ols(
        f'{dep} ~ cum_compliance_years + violent_crime_rate_per_100k + staff_per_1000_inmates + C(state) + C(year)',
        data=c,
    ).fit(cov_type='cluster', cov_kwds={'groups': c['state']})

    return twfe_base, year_fe_ctrl, twfe_vcrime_ctrl, len(d), len(c)


def tidy(model, dep, spec, n_raw):
    return {
        'dependent': dep,
        'spec': spec,
        'coef_cum_compliance_years': model.params.get('cum_compliance_years', np.nan),
        'std_err_cum_compliance_years': model.bse.get('cum_compliance_years', np.nan),
        'p_value_cum_compliance_years': model.pvalues.get('cum_compliance_years', np.nan),
        'coef_violent_crime_rate_per_100k': model.params.get('violent_crime_rate_per_100k', np.nan),
        'coef_population_millions': model.params.get('population_millions', np.nan),
        'coef_staff_per_1000_inmates': model.params.get('staff_per_1000_inmates', np.nan),
        'n_obs_model': int(model.nobs),
        'n_obs_available_depvar': int(n_raw),
        'r_squared': model.rsquared,
    }


def main():
    outcomes = pd.concat([extract_year_counts(y) for y in YEARS], ignore_index=True)
    compliance = build_compliance_long()
    staff = load_staffing_controls()
    crime = load_crime_controls()
    pop = load_population_controls()

    panel = outcomes.merge(compliance, on=['state', 'year'], how='inner')
    panel = panel.merge(staff[['state', 'year', 'tot_emp']], on=['state', 'year'], how='left')
    panel = panel.merge(crime, on=['state', 'year'], how='left')
    panel = panel.merge(pop, on='state', how='left')

    panel['alleged_per_1000_inmates'] = np.where(
        panel['alleged_count'].notna() & panel['prisoners'].notna() & (panel['prisoners'] > 0),
        (panel['alleged_count'] / panel['prisoners']) * 1000.0,
        np.nan,
    )
    panel['substantiated_per_1000_inmates'] = np.where(
        panel['substantiated_count'].notna() & panel['prisoners'].notna() & (panel['prisoners'] > 0),
        (panel['substantiated_count'] / panel['prisoners']) * 1000.0,
        np.nan,
    )
    panel['staff_per_1000_inmates'] = np.where(
        (panel['tot_emp'].notna()) & (panel['prisoners'].notna()) & (panel['prisoners'] > 0),
        (panel['tot_emp'] / panel['prisoners']) * 1000.0,
        np.nan,
    )

    panel_path = OUT_DIR / 'prea_panel_state_year_2012_2020_with_population_violent_staff_controls.csv'
    panel.to_csv(panel_path, index=False)

    rows = []
    for dep in ['alleged_per_1000_inmates', 'substantiated_per_1000_inmates']:
        base, year_ctrl, twfe_vc_ctrl, n_base, n_ctrl = fit_models(panel, dep)
        rows.append(tidy(base, dep, 'twfe_cluster_state_no_controls', n_base))
        rows.append(tidy(year_ctrl, dep, 'year_fe_cluster_state_with_population_violent_staff_controls', n_ctrl))
        rows.append(tidy(twfe_vc_ctrl, dep, 'twfe_cluster_state_with_violent_staff_controls_population_absorbed', n_ctrl))

    res = pd.DataFrame(rows)
    res_path = OUT_DIR / 'prea_compliance_panel_regression_per_1000_with_population_violent_staff_controls.csv'
    res.to_csv(res_path, index=False)

    print('Coverage summary:')
    print('  outcomes rows:', len(outcomes))
    print('  staffing rows:', len(staff), '| years:', sorted(staff.year.unique()))
    print('  crime rows:', len(crime), '| years:', sorted(crime.year.unique()))
    print('  population rows:', len(pop))
    print('  merged panel rows:', len(panel))

    print('\nKey compliance coefficient by specification:')
    for _, r in res.iterrows():
        print(
            f"{r['dependent']} | {r['spec']} | "
            f"coef={r['coef_cum_compliance_years']:.4f} "
            f"se={r['std_err_cum_compliance_years']:.4f} "
            f"p={r['p_value_cum_compliance_years']:.4g} "
            f"N={int(r['n_obs_model'])}"
        )

    print('\nSaved:')
    print(panel_path)
    print(res_path)


if __name__ == '__main__':
    main()
