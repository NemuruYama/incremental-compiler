import os

# Find the executable in a child of the dist-newstyle/build directory
# (the exact path will depend on the version of GHC)
def find_executable(target):
  path = ""
  cwd = os.getcwd()
  for root, _, files in os.walk("dist-newstyle/build"):
    for file in files:
      if file == target:
        path = cwd + "/" + os.path.join(root, file)
        break
  return path

if os.path.isfile("build.py"):
  os.system("python build.py")

path = find_executable("IncrementalCompiler.exe")

# Check if file found
if path == "":  
  print("Could not find executable")
  exit(1)

# Run the executable
os.system(path)
