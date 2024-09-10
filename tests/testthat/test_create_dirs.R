# Create a temporary directory for testing
test_dir <- tempdir()

test_that("createDirs creates directories in the current working directory", {
    # Set up
    old_wd <- getwd()
    setwd(test_dir)
    on.exit(setwd(old_wd), add = TRUE)

    # Ensure directories do not exist initially
    expect_false(dir.exists(file.path(test_dir, "data")))
    expect_false(dir.exists(file.path(test_dir, "src")))
    expect_false(dir.exists(file.path(test_dir, "output")))
    expect_false(dir.exists(file.path(test_dir, "notebook")))

    # Run the function
    createDirs()

    # Test if directories were created
    expect_true(dir.exists(file.path(test_dir, "data")))
    expect_true(dir.exists(file.path(test_dir, "src")))
    expect_true(dir.exists(file.path(test_dir, "output")))
    expect_true(dir.exists(file.path(test_dir, "notebook")))
})

test_that("createDirs creates directories in a specified parent directory", {
    # Create a temporary parent directory
    parent_dir <- file.path(test_dir, "project_folder")
    dir.create(parent_dir)

    # Ensure directories do not exist initially
    expect_false(dir.exists(file.path(parent_dir, "data")))
    expect_false(dir.exists(file.path(parent_dir, "src")))
    expect_false(dir.exists(file.path(parent_dir, "output")))
    expect_false(dir.exists(file.path(parent_dir, "notebook")))

    # Run the function
    createDirs(parent_dir)

    # Test if directories were created in the specified parent directory
    expect_true(dir.exists(file.path(parent_dir, "data")))
    expect_true(dir.exists(file.path(parent_dir, "src")))
    expect_true(dir.exists(file.path(parent_dir, "output")))
    expect_true(dir.exists(file.path(parent_dir, "notebook")))
})

test_that("createDirs throws an error if the parent directory does not exist", {
    non_existent_dir <- file.path(test_dir, "non_existent_folder")

    expect_error(createDirs(non_existent_dir))
})

test_that("createDirs does not recreate existing directories", {
    # Create a temporary parent directory and an existing subdirectory
    parent_dir <- file.path(test_dir, "project_folder_with_existing")
    dir.create(parent_dir)
    dir.create(file.path(parent_dir, "data"))

    # Run the function
    createDirs(parent_dir)

    # Check that directories are created and no errors occur for existing ones
    expect_true(dir.exists(file.path(parent_dir, "data")))
    expect_true(dir.exists(file.path(parent_dir, "src")))
    expect_true(dir.exists(file.path(parent_dir, "output")))
    expect_true(dir.exists(file.path(parent_dir, "notebook")))
})


