# 📊 Smart Chart Assistant

AI-powered chart generation and visualization tool combining React frontend with R Shiny backend.

## Features

### 1. CSV to 10 Graphics
- Upload CSV file
- AI analyzes data and suggests 10 optimal chart types
- Powered by Google Gemini AI

### 2. Image & CSV to Graphic
- Upload chart image for type identification
- Recreate chart with your own data
- AI-powered chart type detection

### 3. Reactive Chart (R Shiny) ⭐
- **Full reactive data flow**
- Upload CSV and columns auto-detect
- Dynamic UI updates
- 7 chart types: Scatterplot, Line, Bar, Boxplot, Violin, Histogram, Density
- Interactive variable selection (X, Y, color, facet)
- Real-time chart generation with ggplot2

## Quick Start

### Prerequisites
- Node.js (v16+)
- R (v4.0+)
- Required R packages: `shiny`, `ggplot2`, `dplyr`, `readr`

### Installation

```bash
# Install R packages
Rscript -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'readr'))"

# Install Node dependencies
npm install
```

### Running the Application

**Terminal 1 - Start Shiny Backend:**
```bash
Rscript -e "shiny::runApp('backend_r/shiny_app.R', port=8002, host='0.0.0.0')"
```

## Project Structure

```
ai-chart-suggester/
├── backend_r/
│   ├── shiny_app.R          # R Shiny reactive chart app
│   └── test_ogrenci.csv     # Sample data
├── components/
│   ├── ShinyChart.tsx       # Shiny iframe component
│   ├── ChartCard.tsx
│   └── ...
├── services/
│  └── geminiService.ts     # AI service

```

## Usage

### Reactive Chart Mode

1. Click **"3. Reactive Chart (R Shiny)"** tab
2. Upload your CSV file
3. Select chart type
4. Choose X and Y variables
5. Optionally add color and facet grouping
6. Chart updates automatically!

### Sample Data

Use `backend_r/test_ogrenci.csv` for testing:
- 20 rows of student data
- Columns: OgrenciID, Ders, Puan, Sube, Cinsiyet
- Mix of numeric and categorical variables

## Architecture

```
┌─────────────────┐
│  React Frontend │ (Port 5173)
│   - 3 Modes     │
│   - Tabs UI     │
└────────┬────────┘
         │
         │ iframe embed
         ↓
┌─────────────────┐
│   R Shiny App   │ (Port 8002)
│   - File Upload │
│   - Reactive UI │
│   - ggplot2     │
└─────────────────┘
```

## Development

### Adding New Chart Types

Edit `backend_r/shiny_app.R`:

```r
# Add to chart type selector
selectInput("chart_type", "Grafik Türü",
  choices = c(..., "New Type" = "new_type"))

# Add rendering logic
if (chart_type == "new_type") {
  p <- p + geom_new_type()
}
```

### Customizing Shiny UI

Modify `backend_r/shiny_app.R` UI section for styling and layout changes.

## Troubleshooting

**Shiny app not loading:**
- Check if port 8002 is available
- Verify R packages are installed
- Check terminal for error messages

**Charts not generating:**
- Ensure CSV has valid data
- Check column types (numeric vs categorical)
- Verify variable selections are compatible with chart type

## Credits

- **R Shiny** - Reactive web framework
- **ggplot2** - Grammar of graphics
- **Google Gemini** - AI chart suggestions
