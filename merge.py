# merge.py
import sys
import os
import re

# VSCode 通过 args 传来的当前活动文件
if len(sys.argv) < 2:
    print("Usage: python merge.py <entry_file>")
    exit(1)

ENTRY_FILE = sys.argv[1]

# 输出文件名： entry_basename + "_merge.cpp"
base = os.path.splitext(os.path.basename(ENTRY_FILE))[0]
OUTPUT_FILE = base + "_merge.cpp"

included = set()
include_regex = re.compile(r'^\s*#\s*include\s*"([^"]+)"')

def expand_file(path, out):
    if path in included:
        return
    included.add(path)

    out.write(f"\n// ===== Begin {path} =====\n")

    with open(path, encoding="utf8") as f:
        for line in f:
            m = include_regex.match(line)
            if m:
                header = m.group(1)
                if os.path.exists(header):
                    expand_file(header, out)
                continue
            out.write(line)

    out.write(f"// ===== End {path} =====\n")

def main():
    with open(OUTPUT_FILE, "w", encoding="utf8") as out:
        expand_file(ENTRY_FILE, out)

    print("Generated:", OUTPUT_FILE)

if __name__ == "__main__":
    main()
