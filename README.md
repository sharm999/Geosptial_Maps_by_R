# Geosptial_Maps_by_R
Creating all the geosptial maps using R language. I am trying to create the maps here in repostories for helping researhers and grow with me.
Bivariate_Map_Midwest.r --- It's the bivariate map created by Dr. Mukta Sharma (inspiration: Milos Makes Maps tutorial https://www.youtube.com/watch?v=EECMHPn2Iuc&t=1319s), Postdoc for US Midwest using PRISM dataset from 2008 to 2025 for May to October month (growing season).

*What the map is showing*

Each grid cell/pixel summarizes two climate variables at once for the growing season:
May–Oct mean temperature (°C)
May–Oct total precipitation (mm)
Because your legend is 3 × 3 (dim = 3), both variables are grouped into three classes (typically low / medium / high, often using quantiles across the mapped region).
So, every color on the map encodes a combination of temperature class and precipitation class.

*How to read the legend (key part)*

X-axis (left → right): Temperature increases
left = cooler, middle = moderate, right = warmer
Y-axis (bottom → top): Precipitation increases
bottom = drier, middle = moderate, top = wetter
The corners mean:
Bottom-left (grey) = cool + dry
Bottom-right (magenta) = warm + dry
Top-left (teal) = cool + wet
Top-right (dark navy) = warm + wet
The middle colors are the “in-between” combinations (e.g., warm + moderate precip, cool + moderate precip, etc.).

*What patterns the map suggests (qualitatively)*

Even without exact numbers (since its class based), the spatial gradients are clear:
1) North–south temperature gradient
Upper Midwest / Great Lakes (MN, WI, MI) shows more cooler class colors (teals/cyans/lavenders), consistent with cooler growing seasons.
Lower Midwest (MO, IL, IN, OH) shifts toward warmer class colors (purples/navies/magentas).
2) West–east precipitation gradient
The western edge (Plains side) contains more drier class colors (greys/pinks/magentas), indicating relatively lower May–Oct rainfall totals.
Moving toward the central/eastern Midwest, colors trend into moderate to wetter classes (lavenders → blues/navies).
3) Where “warm + wet” concentrates
The dark navy areas correspond to higher temperature + higher precipitation (relative to the region). These often cluster in the central to southern/eastern portions of the Midwest in the map.

*What this means (why it’s useful)*

This bivariate view is great for agriculture because it distinguishes climate regimes that can drive different yield risks:
Warm + dry (magenta) → higher water stress risk, higher evaporative demand, stronger sensitivity to drought.
Warm + wet (navy) → adequate water supply but potentially higher disease pressure, nutrient leaching risk, and waterlogging sensitivity in some landscapes.
Cool + wet (teal/cyan) → lower heat stress, but can imply delayed development, lower GDD accumulation, or wet field constraints depending on timing.
Cool + dry (grey) → shorter/cooler season + limited moisture; crop response can depend heavily on soil water holding capacity.
