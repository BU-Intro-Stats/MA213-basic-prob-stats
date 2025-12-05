# ---- dependencies ----
import numpy as np
import pandas as pd
from pynwb import NWBHDF5IO

# ---- user settings ----
nwb_path   = "../../../000140/sub-Jenkins/sub-Jenkins_ses-small_desc-train_behavior+ecephys.nwb"
csv_out   = "mc_maze_small_train.csv"   # or _test.csv
# specify which fields inside NWB correspond to spikes & kinematics
spike_unit_field = "units"   # example: NWB units table
spike_counts_field = "spike_counts"  # may need to compute counts per bin
kinematics_field = "hand_velocity"   # example dataset field
time_field       = "timestamps"

# ---- load NWB file ----
with NWBHDF5IO(nwb_path, mode='r') as io:
    nwbfile = io.read()
    
    # get timestamps and dt
    t = np.array(nwbfile.acquisition[time_field].timestamps) \
           if hasattr(nwbfile.acquisition[time_field], "timestamps") \
           else np.array(nwbfile.acquisition[time_field].data[:,0])
    dt = np.diff(t, prepend=t[0])  # first bin dt = 0
    
    # get kinematics (vx, vy)
    # Example: assume hand_velocity is an (N×3) array, columns [vx, vy, vz]
    hv       = np.array(nwbfile.acquisition[kinematics_field].data)
    vx       = hv[:,0]
    vy       = hv[:,1]
    
    # get spike counts per bin for each unit
    units    = nwbfile.units  # DataFrame-like table
    # Suppose each unit has spike_times attribute list
    spike_times_list = units.spike_times[:]
    # Define bin edges (we’ll use t)
    # Count for each unit and each bin
    spike_counts = []
    for st in spike_times_list:
        # count spikes in each time bin
        sc, _ = np.histogram(st, bins=np.append(t, t[-1]+dt[-1]))
        spike_counts.append(sc)
    spike_counts = np.vstack(spike_counts).T  # shape: (T × K)
    
    # Build DataFrame
    df = pd.DataFrame({
        "time": t,
        "dt": dt,
        "vx": vx,
        "vy": vy
    })
    # Add spike columns u1..uK
    for i in range(spike_counts.shape[1]):
        df[f"u{i+1}"] = spike_counts[:, i]
    
    # Drop first row if dt==0 (optional)
    df = df.iloc[1:].reset_index(drop=True)
    
    # Save to CSV
    df.to_csv(csv_out, index=False)
    print(f"Wrote {csv_out} with shape {df.shape}")
