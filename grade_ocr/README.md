## OCR grade checker

### Prerequisites

- Ollama install with minicpm-v
- Android phone with droidcam

### Usage

- Download student list and save as participants.csv in same directory
- Place phone overlooking papers
- Run Ollama and droidcam, open droidcam stream in browser
- Enter droidcam ip in main()
- Run script on one window and watch stream on the other
- Check name, enter grade if correct, n to retry, m to enter name manually, q to stop entry
- Change to next paper, press enter to confirm and save last one. (n to retry last one)
- At the end, press q. Enter output filename.
- Merge all csvs together with merge_csv.py {filenames}
