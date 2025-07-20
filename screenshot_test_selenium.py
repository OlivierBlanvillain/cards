
import os
import sys
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from PIL import Image, ImageChops
import numpy as np

# Define paths relative to the project root
PROJECT_ROOT = Path(__file__).parent
INDEX_HTML_PATH = PROJECT_ROOT / "web" / "index.html"

SCREENSHOTS_DIR = PROJECT_ROOT / "screenshots_selenium"
BASELINE_DIR = SCREENSHOTS_DIR / "baseline"
CURRENT_DIR = SCREENSHOTS_DIR / "current"
DIFF_DIR = SCREENSHOTS_DIR / "diff"

# Ensure directories exist
BASELINE_DIR.mkdir(parents=True, exist_ok=True)
CURRENT_DIR.mkdir(parents=True, exist_ok=True)
DIFF_DIR.mkdir(parents=True, exist_ok=True)

def run_screenshot_test():
    print(f"Running screenshot test for: {INDEX_HTML_PATH.resolve()}")
    print(f"Baseline screenshots will be stored in: {BASELINE_DIR.resolve()}")
    print(f"Current test screenshots will be stored in: {CURRENT_DIR.resolve()}")
    print(f"Difference images will be stored in: {DIFF_DIR.resolve()}")

    # Configure Chrome options for headless mode
    chrome_options = Options()
    chrome_options.add_argument("--headless")
    chrome_options.add_argument("--no-sandbox") # Required for some environments
    chrome_options.add_argument("--disable-dev-shm-usage") # Overcomes limited resource problems
    chrome_options.add_argument("--window-size=1920,1080") # Set a consistent window size

    driver = None
    try:
        # Initialize the Chrome WebDriver
        # Ensure chromedriver is in your PATH or specify its executable_path
        driver = webdriver.Chrome(options=chrome_options)

        # Navigate to the local index.html file
        driver.get(f"file://{INDEX_HTML_PATH.resolve()}")

        # Give the page some time to render completely
        # You might need to adjust this based on your page's complexity
        driver.implicitly_wait(5) # seconds

        # Take screenshot
        current_screenshot_path = CURRENT_DIR / "index_page.png"
        driver.save_screenshot(str(current_screenshot_path))
        print(f"Current screenshot saved to: {current_screenshot_path}")

        baseline_screenshot_path = BASELINE_DIR / "index_page.png"
        diff_screenshot_path = DIFF_DIR / "index_page_diff.png"

        if not baseline_screenshot_path.exists():
            print("Baseline screenshot not found. Please set the baseline first.")
            print(f"Copy '{current_screenshot_path}' to '{baseline_screenshot_path}' to set the baseline.")
            sys.exit(0) # Exit gracefully, not a failure yet

        # Compare images
        baseline_img = Image.open(baseline_screenshot_path)
        current_img = Image.open(current_screenshot_path)

        # Ensure images are the same size before comparison
        if baseline_img.size != current_img.size:
            print("Error: Baseline and current screenshots have different dimensions.")
            print(f"Baseline: {baseline_img.size}, Current: {current_img.size}")
            sys.exit(1)

        # Calculate difference
        diff = ImageChops.difference(baseline_img, current_img)
        diff_pixels = np.array(diff.getdata()).sum()

        if diff_pixels > 0: # If there are any differences
            diff.save(str(diff_screenshot_path))
            print(f"Visual regression detected! Differences saved to: {diff_screenshot_path}")
            print(f"Total differing pixels: {diff_pixels}")
            sys.exit(1) # Indicate failure
        else:
            print("Visual regression test passed: No differences detected.")

    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)
    finally:
        if driver:
            driver.quit()

if __name__ == "__main__":
    run_screenshot_test()
