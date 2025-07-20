
import os
import sys
from pathlib import Path
from playwright.sync_api import sync_playwright, expect

# Define paths relative to the project root
PROJECT_ROOT = Path(__file__).parent
INDEX_HTML_PATH = PROJECT_ROOT / "web" / "index.html"

SCREENSHOTS_DIR = PROJECT_ROOT / "screenshots"
BASELINE_DIR = SCREENSHOTS_DIR / "baseline"
CURRENT_DIR = SCREENSHOTS_DIR / "current"

# Ensure directories exist
BASELINE_DIR.mkdir(parents=True, exist_ok=True)
CURRENT_DIR.mkdir(parents=True, exist_ok=True)

def run_screenshot_test():
    print(f"Running screenshot test for: {INDEX_HTML_PATH.resolve()}")
    print(f"Baseline screenshots will be stored in: {BASELINE_DIR.resolve()}")
    print(f"Current test screenshots will be stored in: {CURRENT_DIR.resolve()}")

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page()

        # Navigate to the local index.html file
        # Using a file:// URL directly
        page.goto(f"file://{INDEX_HTML_PATH.resolve()}")

        # Wait for the page to load and render. Adjust as needed.
        # For simple HTML, a short wait might be enough, or wait for a specific element.
        page.wait_for_load_state("networkidle")

        # Take a screenshot and compare it to the baseline
        # Playwright will automatically save the current screenshot to CURRENT_DIR
        # and compare it with the baseline in BASELINE_DIR.
        # If there's a diff, it will save the diff image as well.
        try:
            expect(page).to_have_screenshot(
                name="index_page.png",
                max_diff_pixels=100,  # Allow for minor differences (e.g., anti-aliasing)
                threshold=0.1,        # Percentage of pixels that can differ
                full_page=True,
                animations="disabled",
                caret="hide",
                # Path to the directory where baseline and current screenshots are stored
                # Playwright will look for baseline in BASELINE_DIR and save current in CURRENT_DIR
                # This requires setting the PLAYWRIGHT_SNAPSHOT_PATH environment variable
                # or configuring it in a test runner like pytest-playwright.
                # For this standalone script, we'll rely on the default behavior or manual setup.
            )
            print("Screenshot test passed: index_page.png matches baseline.")
        except AssertionError as e:
            print(f"Screenshot test failed: {e}")
            print(f"Differences can be found in: {CURRENT_DIR.resolve()}")
            sys.exit(1) # Exit with a non-zero code to indicate failure
        finally:
            browser.close()

if __name__ == "__main__":
    # To update the baseline, run the script with PLAYWRIGHT_UPDATE_SNAPSHOTS=1
    # Example: PLAYWRIGHT_UPDATE_SNAPSHOTS=1 python screenshot_test.py
    # Or, manually copy the image from screenshots/current to screenshots/baseline
    run_screenshot_test()
