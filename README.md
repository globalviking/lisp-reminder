# Termux Lisp Reminder

A lightweight, hackable notification scheduler written in Common Lisp, designed to run natively on Android via Termux.

## Features
- **Zero-Dependency Core:** Runs on standard SBCL.
- **Android Integration:** Uses `termux-api` to push native system notifications.
- **Snooze Functionality:** Interactive notifications with buttons to snooze tasks.

## Requirements
1. **Termux** app (Install from F-Droid/Neo Store).
2. **Termux:API** app (Install from F-Droid/Neo Store).
3. Inside Termux, install packages:
   ```bash
   pkg install sbcl termux-api

## Setup & Editing
The best way to edit your files is to use a native Android editor, or a simple terminal editor.
### Option 1: Using Nano (The Simple Terminal Editor)
Nano is included in Termux and is the simplest tool for quick edits.
To edit or create the todo list:
`nano todo.txt`
Press Ctrl+X to exit and Y to save.

### Option 2 (Recommended): Using Acode (The Android GUI Editor)
Acode (or any other Android code editor) is highly recommended for a better editing experience.
 * Install Acode from the Play Store/F-Droid/Neo Store.
 * Set up Termux Storage Bridge (One-Time Setup): This lets Acode see your Termux files.
   `termux-setup-storage`

   (Click 'Allow' when the permission popup appears.)
 * Find Your Project: In Acode, navigate to your Termux project folder, which is typically under Internal Storage > Documents > code > lisp-code.
Usage
 * Set up wake lock in Termux.
   Run `termux-wake-lock` in Termux to ensure the script runs reliably in the background without being killed by Android's battery saving features.
 * Create the todo file.
   `touch todo.txt`

   (This must be in the same directory as `remind.lisp` and `snooze.lisp`).
 * Input your tasks using the format HH:MM Task Name (24-hour time):

07:00 Exercise

21:00 Sleep

 * Run the script.
   `sbcl --script remind.lisp`

   (To stop the scheduler, press `Ctrl+C` in the Termux window, and run `termux-wake-unlock` to release the battery lock.)
