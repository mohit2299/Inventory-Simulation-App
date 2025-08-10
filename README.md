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


