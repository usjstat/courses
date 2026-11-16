# Corrected dashboard package

## Use a separate folder

Extract this ZIP into a NEW folder outside your course project, for example:

C:/Users/YourName/Documents/USJ_Sports_Dashboard/

Keep _quarto.yml and USJ_Sports_Dashboard.qmd together. Do not place the folder inside the STA 313 course project. The supplied project configuration renders only this QMD.

No Excel file or data.json is needed. All data and dashboard code are embedded in the QMD. Keep the entire long encodedDashboard line intact.

## Render in RStudio

Close the STA 313 project. Open the QMD from the new folder. Click Render, or open the RStudio Terminal in that folder and run:

quarto render USJ_Sports_Dashboard.qmd

The result is rendered/USJ_Sports_Dashboard.html. Open that file in your browser. To publish through your existing website, upload the rendered HTML as a standalone page.

The root USJ_Sports_Dashboard.html included in the ZIP is a ready-to-open version and does not require Quarto. The rendered subfolder is created when you render the QMD.

## Why this version differs

The previous QMD placed the dashboard's JavaScript-generated HTML tables directly inside Quarto's raw HTML processing. This version encapsulates the dashboard in an iframe srcdoc and stores its contents as encoded text, so Quarto does not attempt to parse its tables. The page requires JavaScript, as do its filters and charts.

Your log also contains inherited course settings (STA 313 title prefix and ../styles.css). The JSON parser was reading content beginning '# Rugby An...' as a notebook. That points to another file processed by the project, potentially a Markdown file with an .ipynb extension. The exact file cannot be identified from the supplied trace. Do not rename or delete course files based on this guess. Keeping this dashboard in its own project avoids scanning those files.

If the same '# Rugby An...' error occurs in the new folder, confirm that you are rendering this QMD, that the supplied _quarto.yml is present, and that RStudio is no longer using the course project. Send the new complete log if it persists.

## Verification

Embedded HTML was decoded and checked against the original standalone dashboard byte for byte. The JavaScript payload is unchanged. Quarto is not installed in the preparation environment, so actual Quarto rendering has not been tested here.
