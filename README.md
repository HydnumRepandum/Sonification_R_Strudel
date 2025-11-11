

# 🎶 PoliSonification

**PoliSonification** is an adaptation of the original *Sonification* project by [Frank Pattyn](https://github.com/FrankPat/Sonification).
All rights for the original concept and structure belong to him.

This repository translates and extends Pattyn’s work to the field of **Political Science**, enabling the **sonification of time series data** such as democracy indices, voting trends, or institutional evolution.

---

## 🔄 Main Changes

1. **Full rewrite** of the original MATLAB code into **R**.
2. **Adaptation** for CSV inputs containing **temporal series in political science**, such as the [V-Dem Democracy Index](https://v-dem.net/data_analysis/VariableGraph/).
3. **Framework extension** to convert `.abc` files into **Strudel** code — a JavaScript live-coding environment for generative music.

---

## 🧠 Concept

Sonification transforms numerical data into musical structures.
In *PoliSonification*, political time series (e.g., democracy scores over time) are translated into notes, allowing researchers and artists to *listen* to institutional evolution.

---

## 🚀 How to Use

### 1. Generate an ABC file in R

Open `Data2Music.R` and import your **CSV time series**.
The script outputs an `.abc` file — a music notation format.

> You can convert `.abc` files into sheet music or MIDI online via [abcnotation.com](https://abcnotation.com/).

---

### 2. Convert ABC to MIDI

Use [EasyABC](https://sourceforge.net/projects/easyabc/) to open your `.abc` file and export it as a `.midi`.

---

### 3. Convert MIDI to Strudel Code

Use [Emanuel de Jong’s MIDI-To-Strudel tool](https://github.com/Emanuel-de-Jong/MIDI-To-Strudel) to translate your MIDI into **Strudel JavaScript** code.

---

### 4. Play it in Strudel

Paste the Strudel code into [Strudel.cc](https://strudel.cc/) and experiment with sound design, tempo, and structure.

When you’re happy with your result, **save your Strudel composition** as a `.js` file.

---

## 📁 Repository Structure

```
PoliSonification/
├── Data2Music.R               # Main R script to convert time series to ABC
├    └──Sonification.R
├    └──MakeSoundScale.R               
├── Political_Dataset/         # Example political datasets (e.g., democracy index)
├── JS_for_Strudel/            # Example Strudel compositions
└── README.md                  # You are here
```

---

## 🎧 Example Outputs

You can find example Strudel compositions in the folder `JS_for_Strudel`,
and example datasets in `Political_Dataset`.

---

## 🧩 Credits

* Original MATLAB code by **Frank Pattyn** — [Sonification repository](https://github.com/FrankPat/Sonification)
* MIDI-to-Strudel converter by **Emanuel de Jong** — [GitHub](https://github.com/Emanuel-de-Jong/MIDI-To-Strudel)
* R rewrite and political adaptation by **Hydnum Repandum**

