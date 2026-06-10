Modulare Shiny-App, die direkt das R6‑Objekt konsumiert.

🧱 Zielstruktur
inst/app/
  │
├── app.R                # optional
├── server.R
├── ui.R
│
├── modules/
  │   ├── mod_table.R
│   └── mod_series.R
│
├── utils/
  │   └── dashboard_helpers.R
│
└── www/
  └── styles.css
