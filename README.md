# Termux Lisp Reminder

A lightweight, hackable notification scheduler written in Common Lisp, designed to run natively on Android via Termux.

## Features
- **Zero-Dependency Core:** Runs on standard SBCL.
- **Android Integration:** Uses `termux-api` to push native system notifications.
- **Snooze Functionality:** Interactive notifications with buttons to snooze tasks.

## Requirements
1. **Termux** app install from F-Droid/Neo Store). 
2. **Termux:API** app (install from F-Droid/Neo Store).
3. Inside Termux install packages:
   ```bash
   pkg install sbcl termux-api
   
## Usage
1. Set up wake lock in Termux. 
   Run `termux-wake-lock` in Termux to run reliably in the background.
2. Run the script. 
  `sbcl --script remind.list`
