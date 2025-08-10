Inventory Simulation App
A lightweight Shiny app to simulate monthly inventory positions under flexible replenishment cycles. It supports reorder-point logic, understock/overstock flagging, optional Monte Carlo stress tests, and manual inclusion of in-transit and factory on-order arrivals.

Why this exists:

Fast, transparent what-if analysis.

Communicates math and assumptions clearly to operations, finance, and planning teams.

Core Assumptions
Demand distribution: Monthly demand per SKU ~ Normal(μ, σ) using historical averages (μ) and monthly SD (σ).

If history < 2 months, use σ = 0.2 × μ (conservative).

Lead time (L): Tested at 2 or 4 months (user-selectable).

Service level: Z = 1.65 (≈95%).

On-order treatment: In-transit / factory orders are not automatically deducted from your policy; instead, you enter arrivals by month, and the simulator adds them to the month they arrive.

Monte Carlo: Optional; 100,000 draws per SKU to estimate a robust max level at 95th percentile demand.

Math

Safety Stock (SS)
SS = Z × σ × √L

Lead Time Demand (LTD)
LTD = D̄ × L

Max Level (policy target)
Max Level = LTD + SS

Monte Carlo Max 
Simulate Di ~ N(D̄, σ) across months; take the 95th percentile of cumulative demand over L to set a probabilistic max.


App Features
Flexible cycles: Order every 1 / 2 / 4 months.

Reorder check: Order on the cycle month if starting inventory ≤ reorder point.

Understock tolerance: Flag risk when ending inventory falls below:

Threshold = LTD × (1 − Tolerance%)

On-order arrivals: Enter comma-separated monthly arrivals for in-transit and factory — they’re added to the respective months.

Pack-size rounding: Round orders up to the nearest pack size.

Visuals
Ending inventory (bar, colour-coded by flag)

Order quantities by month

Flags
Stockout: ending < 0

Understock Risk: 0 ≤ ending < threshold

Normal: threshold ≤ ending ≤ reorder point

Overstock: ending > reorder point
