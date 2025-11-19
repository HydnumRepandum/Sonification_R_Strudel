!pip install mido

import pandas as pd
import mido
from mido import MidiFile, MidiTrack, Message, MetaMessage, bpm2tempo
import glob
import os
import traceback

def get_data_from_csv(file_path):
    """Reads data from CSV and standardizes columns (without normalization)."""
    try:
        df = pd.read_csv(file_path)
        # Map columns if needed (fallback to first two columns)
        cols = df.columns.tolist()
        if 'time_sec' not in cols or 'value_raw' not in cols:
            if len(cols) >= 2:
                # Assume col 0 is time, col 1 is the value
                df = df.rename(columns={cols[0]: 'time_sec', cols[1]: 'value_raw'})
            else:
                print(f"Skipping {file_path}: Not enough columns.")
                return None
        return df
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return None

def create_track_from_df(df, track_name="Automation", bpm=120, ppq=480, cc_number=1, channel=0):
    """Generates a MidiTrack from a DataFrame."""
    track = MidiTrack()

    # Set Tempo and Track Name
    tempo_val = bpm2tempo(bpm)
    track.append(MetaMessage('set_tempo', tempo=tempo_val, time=0))
    track.append(MetaMessage('track_name', name=track_name, time=0))

    # Prepare data for conversion
    df = df.sort_values('time_sec').reset_index(drop=True)
    # Convert time to absolute ticks: time * (beats/sec) * ticks/beat
    df['abs_ticks'] = (df['time_sec'] * (bpm / 60.0) * ppq).round().astype(int)
    # Convert value to CC (0-127). Assumes 'value_norm' is already present and [0,1]
    df['cc_val'] = (df['value_norm'].fillna(0).clip(0, 1) * 127).round().astype(int)

    prev_tick = 0
    count = 0
    for _, row in df.iterrows():
        current_tick = int(row['abs_ticks'])
        delta_time = max(0, current_tick - prev_tick)
        cc_val = int(row['cc_val'])

        msg = Message('control_change', channel=channel, control=cc_number, value=cc_val, time=delta_time)
        track.append(msg)
        prev_tick = current_tick
        count += 1

    track.append(MetaMessage('end_of_track', time=0))
    return track, count

# --- Main Execution Block ---

BPM = 120
PPQ = 480
CC_NUMBER = 1
OUTPUT_DIR = '/content/midi_exports'

# Create output directory
os.makedirs(OUTPUT_DIR, exist_ok=True)
print(f"Output directory created: {OUTPUT_DIR}")

# 1. Find Files
all_files = glob.glob('/content/*.csv')
csv_files = [f for f in all_files if 'sample_data' not in f]
print(f"Found {len(csv_files)} CSV files.")

# 2. First Pass: Load Data & Determine Global Min/Max
data_list = []
all_values = []

for file_path in csv_files:
    df = get_data_from_csv(file_path)
    if df is not None:
        # Collect valid values for global range calculation
        valid_vals = df['value_raw'].dropna().tolist()
        all_values.extend(valid_vals)

        # Store for next step
        data_list.append({'path': file_path, 'df': df})

if not all_values:
    print("No data found in files.")
else:
    global_min = min(all_values)
    global_max = max(all_values)
    print(f"Global Range determined: {global_min} to {global_max}")

    # Initialize the Master MIDI file
    master_mid = MidiFile(ticks_per_beat=PPQ)

    # 3. Second Pass: Normalize & Generate MIDI
    for i, item in enumerate(data_list):
        df = item['df']
        file_path = item['path']

        file_name = os.path.basename(file_path)
        track_name = file_name.split(' Transparent')[0] if ' Transparent' in file_name else file_name.replace('.csv', '')

        print(f"\nProcessing: {track_name}")

        # Global Normalization
        if global_max > global_min:
            df['value_norm'] = (df['value_raw'] - global_min) / (global_max - global_min)
        else:
            df['value_norm'] = 0.5

        # Cycle through channels 0-15
        channel = i % 16

        # Create the track
        track, event_count = create_track_from_df(df, track_name=track_name, bpm=BPM, ppq=PPQ, cc_number=CC_NUMBER, channel=channel)

        # Add to Master MIDI File
        master_mid.tracks.append(track)

        # Save as Individual MIDI File
        single_mid = MidiFile(ticks_per_beat=PPQ)
        single_mid.tracks.append(track)

        output_filename = os.path.join(OUTPUT_DIR, f"output_{track_name.replace(' ', '_')}.mid")
        single_mid.save(output_filename)
        print(f"  -> Saved individual: {output_filename}")

    # Save the combined Master MIDI file
    master_output = os.path.join(OUTPUT_DIR, "all_curves_combined.mid")
    master_mid.save(master_output)
    print(f"\n[SUCCESS] Saved combined MIDI file to: {master_output}")


import matplotlib.pyplot as plt
import math

# Setup figure with 2 subplots sharing the X axis
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(14, 12), sharex=True)

# Ensure globals exist
if 'global_min' not in locals() or 'global_max' not in locals():
    # Recalculate if variables are missing (e.g., partial run)
    all_vals = []
    for f in csv_files:
        d = get_data_from_csv(f)
        if d is not None: all_vals.extend(d['value_raw'].dropna().tolist())
    global_min, global_max = min(all_vals), max(all_vals)

for i, file_path in enumerate(csv_files):
    file_name = os.path.basename(file_path)
    track_name = file_name.split(' Transparent')[0] if ' Transparent' in file_name else file_name.replace('.csv', '')

    df = get_data_from_csv(file_path)

    if df is not None:
        # --- Plot 1: Original Data ---
        ax1.plot(df['time_sec'], df['value_raw'], label=track_name, linewidth=2, alpha=0.8)

        # --- Plot 2: MIDI CC Data ---
        # Calculate CC values based on Global Norm
        val_norm = (df['value_raw'] - global_min) / (global_max - global_min)
        cc_vals = (val_norm.clip(0, 1) * 127).round()

        # Plot as steps
        ax2.step(df['time_sec'], cc_vals, label=track_name, linewidth=1.5, where='mid')

# Styling Plot 1 (Original)
ax1.set_title(f'Original Raw Values (Global Range: {global_min:.2f} - {global_max:.2f})', fontsize=14)
ax1.set_ylabel('Original Value')
ax1.grid(True, alpha=0.3)
ax1.legend(bbox_to_anchor=(1.01, 1), loc='upper left')

# Styling Plot 2 (MIDI)
ax2.set_title('Generated MIDI CC Values (0-127)', fontsize=14)
ax2.set_ylabel('MIDI CC Value')
ax2.set_xlabel('Time')
ax2.grid(True, alpha=0.3)
ax2.legend(bbox_to_anchor=(1.01, 1), loc='upper left')

plt.suptitle('Global Comparison: All Countries', fontsize=16)
plt.tight_layout()
plt.show()

from google.colab import files
import shutil
import os

output_dir = '/content/midi_exports'
zip_file_name = 'midi_exports.zip'

# Create a zip archive of the directory
shutil.make_archive(os.path.join('/content', os.path.splitext(zip_file_name)[0]), 'zip', output_dir)

print(f"Created {zip_file_name} in /content/")

# Offer the zip file for download
files.download(os.path.join('/content', zip_file_name))
