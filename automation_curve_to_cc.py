!pip install mido

import pandas as pd
import mido
from mido import MidiFile, MidiTrack, Message, MetaMessage, bpm2tempo
import matplotlib.pyplot as plt
import traceback

def curve_to_midi(df, bpm=120, ppq=480, cc_number=1, outfile="automation_curve.mid"):
    """
    Converts a DataFrame with 'time_sec' and 'value_norm' columns into a MIDI CC automation file.
    """
    # 1. Create MIDI File and Track
    mid = MidiFile(ticks_per_beat=ppq)
    track = MidiTrack()
    mid.tracks.append(track)

    # 2. Set Tempo
    tempo_val = bpm2tempo(bpm)
    track.append(MetaMessage('set_tempo', tempo=tempo_val, time=0))
    track.append(MetaMessage('track_name', name='Automation Curve', time=0))

    # 3. Prepare Data
    df = df.sort_values('time_sec').reset_index(drop=True)

    # --- Normalization Check ---
    # If values are not in [0, 1], normalize them to preserve the curve shape
    v_min = df['value_norm'].min()
    v_max = df['value_norm'].max()
    if v_min < 0 or v_max > 1:
        print(f"Values outside [0, 1] detected (min={v_min}, max={v_max}). Normalizing data to [0, 1]...")
        if v_max != v_min:
            df['value_norm'] = (df['value_norm'] - v_min) / (v_max - v_min)
        else:
            df['value_norm'] = 0.5  # Fallback if flat line

    # 4. Convert to MIDI units
    # beats = time_sec * (bpm / 60)
    # ticks = beats * ppq
    df['abs_ticks'] = (df['time_sec'] * (bpm / 60.0) * ppq).round().astype(int)

    # Convert value_norm [0, 1] -> MIDI CC [0, 127]
    df['cc_val'] = (df['value_norm'].clip(0, 1) * 127).round().astype(int)

    # 5. Generate MIDI Messages
    prev_tick = 0
    count = 0

    for index, row in df.iterrows():
        # Explicitly cast numpy types to native python int for mido
        current_tick = int(row['abs_ticks'])
        delta_time = current_tick - prev_tick

        if delta_time < 0:
            delta_time = 0

        cc_val = int(row['cc_val'])

        # Create Control Change Message
        msg = Message('control_change', channel=0, control=cc_number, value=cc_val, time=delta_time)
        track.append(msg)

        prev_tick = current_tick
        count += 1

    # End of track
    track.append(MetaMessage('end_of_track', time=0))

    # 6. Save File
    mid.save(outfile)
    print(f"Successfully saved MIDI file to: {outfile}")
    print(f"Processed {count} events.")

# --- Main Execution Block ---

file_path = '/content/world Transparent laws with predictable enforcement.csv'
BPM = 120
PPQ = 480
CC_NUMBER = 1
OUTPUT_FILE = "automation_curve.mid"

try:
    print(f"Loading data from {file_path}...")
    df = pd.read_csv(file_path)

    # --- Column Mapping ---
    # Map first two columns if standard names don't exist
    if 'time_sec' not in df.columns or 'value_norm' not in df.columns:
        print("Standard column names not found. Mapping first two columns to 'time_sec' and 'value_norm'...")
        cols = df.columns.tolist()
        # Ensure we have enough columns
        if len(cols) >= 2:
            df = df.rename(columns={cols[0]: 'time_sec', cols[1]: 'value_norm'})
        else:
            raise ValueError("CSV file must have at least two columns.")

    print("Data Preview:")
    print(df[['time_sec', 'value_norm']].head())

    # Execute Conversion
    curve_to_midi(df, bpm=BPM, ppq=PPQ, cc_number=CC_NUMBER, outfile=OUTPUT_FILE)

    # ------------------------------------------------------------------
    # 3. Plot your curve vs. exported MIDI (visual verification)
    # ------------------------------------------------------------------
    print("Reading back MIDI file for visual verification...")
    mid = MidiFile(OUTPUT_FILE)

    ticks_per_beat = mid.ticks_per_beat
    tempo = None  # We’ll pick up the tempo from the MIDI, or fall back to BPM

    cc_times = []
    cc_vals = []

    current_ticks = 0
    for track in mid.tracks:
        for msg in track:
            current_ticks += msg.time  # accumulate delta times
            if msg.type == 'set_tempo' and tempo is None:
                tempo = msg.tempo
            if msg.type == 'control_change' and msg.control == CC_NUMBER:
                # Use tempo from file if present, else default to BPM
                if tempo is None:
                    tempo = bpm2tempo(BPM)
                seconds = mido.tick2second(current_ticks, ticks_per_beat, tempo)
                cc_times.append(seconds)
                cc_vals.append(msg.value)

    if not cc_times:
        print("No CC messages found in the MIDI file for the specified CC number.")
    else:
        midi_df = pd.DataFrame({
            "time_sec": cc_times,
            "cc_value": cc_vals
        })
        # Normalize CC values [0,127] -> [0,1] for comparison
        midi_df["value_norm"] = midi_df["cc_value"] / 127.0

        print("MIDI Data Preview:")
        print(midi_df.head())

        # Plot original curve vs. MIDI reconstruction
        plt.figure(figsize=(10, 4))
        plt.plot(df["time_sec"], df["value_norm"], label="Original curve")
        plt.step(midi_df["time_sec"], midi_df["value_norm"], where="post", label="From MIDI")
        plt.xlabel("Time (s)")
        plt.ylabel("Normalized value")
        plt.title("Original automation vs. MIDI-exported automation")
        plt.legend()
        plt.tight_layout()
        plt.show()

except FileNotFoundError:
    print(f"Error: The file '{file_path}' was not found.")
except Exception as e:
    print(f"An error occurred: {e}")
    traceback.print_exc()
