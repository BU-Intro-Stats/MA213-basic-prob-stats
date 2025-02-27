# How to Use Gradescope to Autograde R Script Files  

Setting up autograding in Gradescope requires several files:  

```
setup.sh  
run_autograder  
grade_one_submission.R  
assignment1_tests.R  
data.csv  
```

## File Descriptions  

### 1. `setup.sh`  
A Bash script that Gradescope runs to install R and any required R packages. **Do not rename this file**—Gradescope expects it to have this exact name.  

### 2. `run_autograder`  
This script **must be named `run_autograder`**, as Gradescope relies on this filename. It handles each student's submission by:  
- Copying the submitted R script to the expected directory.  
- Running `grade_one_submission.R` to evaluate the submission.  

### 3. `assignment1_tests.R`  
Contains unit tests using the `testthat` package to evaluate student submissions.  
- You can rename this file, but if you do, update the reference in `grade_one_submission.R` accordingly.  

### 4. `grade_one_submission.R`  
An R script that:  
- Loads the student's submission.  
- Runs `assignment1_tests.R` to evaluate the submission.  

### 5. `data.csv` (Optional)  
If your assignment requires data, you can include a `.csv` file. This file can have any name, but ensure your script references it correctly.  

---  

## Workflow  

1. **Create a new programming assignment** on Gradescope.  
2. **Download or clone this repository** and modify the necessary files.  
3. **Zip the modified files** and upload them to Gradescope.  

> ⚠ **Note:** Uploading the zip file may take a few minutes, as Gradescope sets up a fresh environment each time. A more complex `setup.sh` will increase upload times.  

For a step-by-step guide on using Gradescope, refer to their official documentation:  
[Gradescope Autograders Guide](https://gradescope-autograders.readthedocs.io/en/latest/getting_started/)  

---  

## Customizing for Your Assignment  

When adapting this setup for your own assignment, keep the following in mind:  

### 1. File Naming  
Ensure that all student submissions follow a consistent naming convention. For example, if submissions are named `assignment1.R`, make sure all relevant scripts reference this filename.  

### 2. Updating `grade_one_submission.R`  
Verify that:  
- It references the correct filenames.  
- It correctly integrates `assignment1_tests.R`.  

### 3. Providing Clear Assignment Instructions  
Consider including explicit instructions for students, similar to this [example assignment](https://github.com/tbrown122387/Using-gradeR-for-the-Gradescope-Autograder/blob/master/example_hw_assignment/fake_hw1.pdf). This helps prevent confusion.  

### 4. Installing Required Packages  
In `setup.sh`, install only the necessary R packages to optimize setup time.  

### 5. Keeping Gradescope's Expected Directory Structure  
Do **not** modify the default directory paths in `run_autograder`, as Gradescope expects a specific structure.  

### 6. Managing Test Visibility  
By default, all tests are hidden from students. To make a test visible, add `(visible)` to its filename. See this [example](https://github.com/tbrown122387/Using-gradeR-for-the-Gradescope-Autograder/blob/master/autograding_code_and_data/assignment1_tests.r).  

### 7. Matching Max Score to Number of Tests  
Ensure that the total number of tests aligns with the max score set on Gradescope.  

### 8. Testing Before Finalizing  
After uploading your autograder zip file:  
- Click **"Test Autograder"** in Gradescope.  
- Upload a correct solution to verify everything works as expected.  
- Ensure the solution file follows the same naming convention as student submissions.  

---  

## Additional Resources  

- [Gradescope Autograder Setup Guide](https://tbrown122387.github.io/autograder_site/)  
- [gradeR Package Documentation](https://cran.r-project.org/web/packages/gradeR/vignettes/gradeR.html)  
