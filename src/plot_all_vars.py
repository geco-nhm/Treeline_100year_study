#%%
import pandas as pd
import matplotlib.pyplot as plt

# Load the full climate data and the matched climate data
climate_df = pd.read_csv('../output/climate_data.csv')
matched_df = pd.read_csv('../output/matched_climate_data.csv')

# Specify the site
site_name = '14Hei_F_NW_H'   # site name

# Filter data for the chosen site
site_climate = climate_df[climate_df['site_name'] == site_name]
site_matched = matched_df[matched_df['site_name'] == site_name]

print("site_climate empty?", site_climate.empty)
print("site_matched empty?", site_matched.empty)

# Get all available variables (common columns excluding 'site_name' and 'year')
variables = [col for col in site_climate.columns if col in site_matched.columns and col not in ['site_name', 'year']]

print("Variables to plot:", variables)

# Define a list of colors for marking different matched periods
colors = ['red', 'green', 'orange']  # Adjust if more periods exist

for variable in variables:
    # Create the canvas for plotting
    plt.figure(figsize=(16, 8))

    # Plot the full time series for the chosen variable from climate data
    plt.plot(
        site_climate['year'],
        site_climate[variable],
        label=f'{variable} (Full Record)',
        marker=',',
        linestyle='-',
        color='blue',
        alpha=0.5
    )

    # For each matched registration, draw a horizontal line spanning the 20-year period
    for i, (_, row) in enumerate(site_matched.iterrows()):
        reg_year = row['year']
        avg_value = row[variable]
        start_year = reg_year - 20  # 20 years back from registration year

        # Draw a horizontal line over the 20-year window at the computed average value
        plt.hlines(
            y=avg_value,
            xmin=start_year,
            xmax=reg_year,
            colors=colors[i % len(colors)],
            linestyles='-',
            linewidth=3,
            label=f'Period {i+1}: {int(start_year)}–{int(reg_year)}'
        )

        # Add a larger marker at the registration year
        plt.plot(reg_year, avg_value, marker='o', color=colors[i % len(colors)], markersize=10)

        # Annotate the registration point
        plt.annotate(
            f'Registration {i+1}',
            xy=(reg_year, avg_value),
            xytext=(-40, 15),
            textcoords='offset points',
            fontsize=12,
            color=colors[i % len(colors)],
            ha='left',
            va='bottom',
            bbox=dict(
                boxstyle='round,pad=0.3',
                edgecolor=colors[i % len(colors)],
                facecolor='white',
                alpha=0.8
            )
        )

    # Add vertical shading for the reference period 1981-2010
    plt.axvspan(1981, 2010, color='grey', alpha=0.2,
                label='Reference Period\n(1981-2010)')

    # Optionally, remove the frame (axes spines)
    ax = plt.gca()
    for spine in ax.spines.values():
        spine.set_visible(False)

    # Customize the plot
    plt.xlabel('Year')
    plt.ylabel(variable)
    plt.title(
        f'{variable} Over Time for {site_name}\n'
        f'with 20-Year Averaged Pre-Registration Periods'
    )
    plt.legend()
    plt.grid(True)
    plt.tight_layout()

    # Save the plot as a PNG file
    out_path = f'../output/all_vars/{variable}_{site_name}_new_300dpi.png'
    plt.savefig(out_path, dpi=300, bbox_inches='tight')
    print("Saved:", out_path)

    # Show the plot on the canvas
    plt.show()
# %%
