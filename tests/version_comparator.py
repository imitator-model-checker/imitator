import difflib
import subprocess
import os
import os.path
import uuid
import argparse
import sys
import tempfile

# Prepare sandbox
temp_dir = tempfile.gettempdir()
sandbox_dir = os.path.join(temp_dir, 'version_comparator_' + str(uuid.uuid1()))
os.mkdir(sandbox_dir)

print("Sandbox created : " + sandbox_dir)

# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument('-model_path', help='Model path to convert')
# parser.add_argument('option', help='Conversion mode')
args = parser.parse_args()

# Check arguments
if not os.path.isfile(args.model_path):
    print("Model " + args.model_path + " not found.")
    sys.exit()

option = "-imi2IMI"

# binaries execution to compare
binaries = [
    {'path':'imitator-v3.0.0-amd64', 'version':'v3'},
    {'path':'/home/me/Desktop/imitator/bin/imitator', 'version':'current'}
]

# get model info
model = args.model_path
model_name_with_ext = os.path.basename(args.model_path)
model_name = os.path.splitext(model_name_with_ext)[0]

# Run IMITATOR and convert models
def convert_model(binary):
    subprocess.run([binary['path'], model, option], cwd=sandbox_dir)
    target_name = os.path.join(sandbox_dir, model_name+'-regenerated-' + binary['version'] + "-" + str(uuid.uuid1()) + '.imi')
    source_name = os.path.join(sandbox_dir, model_name+'-regenerated.imi')
    os.rename(source_name, target_name)
    return target_name

# Compare generated models
def compare_generated_models(files):
    # read files content
    with open(files[0], 'r') as f1:
        with open(files[1], 'r') as f2:
            lines1 = f1.readlines()
            lines2 = f2.readlines()

    # compare
    for line in difflib.unified_diff(lines1, lines2, fromfile='version : ' + binaries[0]['version'], tofile='version : ' + binaries[1]['version'], lineterm=''):
        print(line)

# Cleanup generated files
def cleanup_sandbox(files):
    # Cleanup files
    for file in files:
        os.remove(file)
    # Cleanup sandbox directory
    os.rmdir(sandbox_dir)

def main():
    # Convert model with two version of IMITATOR
    gen_files = [convert_model(binary) for binary in binaries]
    # Compare converted models between versions of IMITATOR
    compare_generated_models(gen_files)
    # Cleanup sandbox containing generated models
    cleanup_sandbox(gen_files)

# Run program
main()
