# Interpretation of All Regression Results (PREA Evaluation)

This document interprets the regression analyses run in scripts 11–14, 16, and 19. Outcomes are inmate-on-inmate sexual victimization rates (alleged and substantiated per 1,000 prisoners) unless noted. Treatment is PREA compliance in various forms.

---

## 1. Simple cross-sectional regressions (Scripts 11, 12)

**Setup:** One observation per state. Outcome = **state average** of alleged (or substantiated) per 1,000 over 2012–2018. Regressor = **total years of compliance** (count of years coded compliant in 2015–2024). No fixed effects, no panel.

| Outcome            | Coefficient (years compliance) | SE    | p-value | Interpretation |
|--------------------|--------------------------------|-------|---------|----------------|
| Alleged per 1,000  | +0.49                          | 0.42  | 0.25    | Not significant. Point estimate: more compliance → slightly higher alleged rate. |
| Substantiated per 1,000 | +0.09                    | 0.06  | 0.13    | Not significant. Slight positive association. |

**Interpretation:** No statistically significant association between total years of compliance and average alleged or substantiated rates. Cross-sectional design cannot separate policy effect from state heterogeneity (e.g., states that adopt PREA may differ in reporting or underlying risk).

---

## 2. Cross-sectional regressions with covariates (Scripts 13; “with all covariates” outputs)

**Setup:** Same state-level aggregation. Regressor = total years of compliance. Controls = population (millions), incarceration rate, violent crime rate, staff per 1,000 inmates.

| Outcome            | Coef. (compliance) | p (compliance) | Notable covariates |
|--------------------|--------------------|----------------|--------------------|
| Alleged per 1,000  | +0.14              | 0.76           | None significant. |
| Substantiated per 1,000 | +0.007         | 0.90           | Violent crime rate: −0.006, **p ≈ 0.02** (higher violent crime → lower substantiated rate). |

**Interpretation:** Again no significant effect of compliance. Adding covariates does not change that. Violent crime rate is negatively associated with substantiated inmate-on-inmate rate in this cross-section (interpret with caution—e.g., reporting priorities, measurement).

---

## 3. Panel regressions with two-way fixed effects (Script 14) — **Primary specification**

**Setup:** State–year panel (2012–2020). **N = 271** (complete cases on outcome, compliance, and covariates including staff). Two-way fixed effects (state + year), standard errors **clustered at state**. Controls: population (millions), incarceration rate, staff per 1,000 inmates, violent crime rate.

### 3a. Treatment = **Cumulative years of PREA compliance** (continuous)

| Variable                         | Alleged per 1,000   | Substantiated per 1,000   |
|---------------------------------|---------------------|---------------------------|
| **Cumulative years compliance** | −0.734 (SE 1.027), p = 0.476 | −0.121 (SE 0.104), p = 0.247 |
| State population (millions)     | +0.603, p = 0.688  | **−0.684, p &lt; 0.001**   |
| Incarceration rate (per 100k)    | +0.019, p = 0.749  | +0.009, p = 0.171         |
| Staff per 1,000 inmates          | +0.002, p = 0.942  | +0.001, p = 0.737         |
| Violent crime rate (per 100k)   | −0.003, p = 0.941 | **−0.009, p = 0.044**     |
| R²                               | 0.667              | 0.684                     |

**Interpretation:**

- **PREA (cumulative):** One additional cumulative year of compliance is associated with lower alleged and lower substantiated rates, but **neither is statistically significant** in this rerun.
- **Substantiated model:** Population remains strongly negative and significant; violent crime is also negative and significant.
- **Alleged model:** No covariate is significant at conventional levels.

---

### 3b. Treatment = **State compliant in year t+1** (indicator, “lead”)

**Setup:** Same panel and controls. Regressor = **prea_compliant_lead**: 1 if the state is PREA compliant in the **next** year, 0 otherwise. Used to probe anticipation or reverse causality.

| Variable              | Alleged per 1,000        | Substantiated per 1,000   |
|-----------------------|--------------------------|---------------------------|
| **PREA compliant (t+1)** | **+3.03 (SE 1.56), p = 0.055** | +0.25 (SE 0.26), p = 0.33 |
| State population (millions) | +0.75, p = 0.69   | **−0.65, p = 0.007**      |
| Other controls        | Not significant          | Violent crime p ≈ 0.07    |

**Interpretation:**

- **Alleged:** States that **will be** compliant next year have **higher** alleged rates **this** year (marginally significant, p ≈ 0.055). That is consistent with **anticipation** (more reporting as states prepare for compliance), **selection** (states with higher reported rates adopt compliance), or **reverse causality**—not with a pure “treatment reduces reports” story for alleged.
- **Substantiated:** No significant effect of next-year compliance. Population remains strongly negative for substantiated rate.

---

## 4. Staff-on-inmate victimization (Script 19)

**Setup:** Same style of regression (years of compliance + covariates) but **outcome = staff-on-inmate** alleged or substantiated per 1,000.

| Outcome                  | Coef. (years compliance) | p     | Notable covariate        |
|--------------------------|--------------------------|-------|--------------------------|
| Staff-on-inmate alleged  | +0.13                    | 0.78  | None significant.        |
| Staff-on-inmate substantiated | +0.006             | 0.92  | Violent crime: −0.006, **p ≈ 0.004**. |

**Interpretation:** No evidence that PREA compliance (years) affects staff-on-inmate alleged or substantiated rates. Violent crime is again negatively associated with substantiated staff-on-inmate rate.

---

## 5. IV results with EverTreated instrument (Script 14)

**Instrument definition currently used:** `EverTreated(t)` = state has ever been compliant up to year `t`.

### 5a. Baseline IV (outcome at `t`)

- **First-stage strength:** F ≈ **24.17** (strong).
- **Second stage (effect of instrumented cumulative compliance years):**
  - Alleged: +3.975 (p = 0.208), not significant.
  - Substantiated: +0.024 (p = 0.941), not significant.

### 5b. Lead outcome IV (outcome at `t+1`)

- **First-stage strength:** F ≈ **25.21** (strong).
- **Second stage:**
  - Alleged (`t+1`): +2.921 (p = 0.444), not significant.
  - Substantiated (`t+1`): +0.273 (p = 0.567), not significant.

### 5c. Lag outcome IV (outcome at `t-1`)

- **First-stage strength:** F ≈ **6.65** (**weak-instrument warning**).
- **Second stage:**
  - Alleged (`t-1`): +11.968 (p = 0.019), significant but should be interpreted cautiously due to weak first stage.
  - Substantiated (`t-1`): +0.386 (p = 0.552), not significant.

---

## 6. Normalized (z-score) outcome (Script 16)

**Setup:** Alleged rate transformed to z-score; regressed on years of compliance.

- Coefficient on years compliance: **−0.03** (SE 0.04), **p = 0.48**.

**Interpretation:** No significant association; consistent with main panel results for alleged rates.

---

## Overall summary

| Specification                    | Alleged: PREA effect      | Substantiated: PREA effect   |
|---------------------------------|---------------------------|------------------------------|
| Simple cross-section (11, 12)   | Not significant (+0.49)   | Not significant (+0.09)      |
| With covariates (13)            | Not significant (+0.14)   | Not significant (+0.007)     |
| **Panel TWFE, cumulative (14)** | Not significant (−0.734)  | Not significant (−0.121) |
| IV (EverTreated, outcome `t`)   | Not significant (+3.975)  | Not significant (+0.024)     |
| IV (EverTreated, outcome `t+1`) | Not significant (+2.921)  | Not significant (+0.273)     |
| IV (EverTreated, outcome `t-1`) | Significant (+11.968), weak first stage | Not significant (+0.386) |
| Staff-on-inmate (19)            | Not significant           | Not significant              |
| Z-score (16)                    | Not significant (−0.03)   | —                            |

**Conclusions:**

1. **TWFE main result (latest rerun):** Cumulative compliance is negatively signed for both outcomes, but not statistically significant for alleged or substantiated.
2. **IV result (with EverTreated at `t`):** Baseline and lead-outcome IV estimates are not significant despite strong first stages; lag-outcome IV alleged effect is significant but has a weak first stage and should be treated as non-credible for causal inference.
3. **Covariates:** Population is robustly negative for substantiated outcomes; several other covariates are sensitive to specification.
4. **Interpretation:** The strongest causal-style takeaway remains limited; evidence is mixed across OLS TWFE and IV, and weak-instrument risk appears in the lag-outcome IV.

**Caveats:** (1) Power may be limited (noisy outcomes, staggered adoption). (2) Compliance may be endogenous (e.g., states with worse conditions adopt earlier). (3) 2019–2020 data and corrections to counts can change estimates slightly; re-run scripts after any data updates to refresh numbers and this interpretation.
