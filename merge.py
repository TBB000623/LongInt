# merge.py
import sys
import os
import re

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
    # 标准化路径
    path = os.path.normpath(path)
    if path in included:
        return
    if not os.path.exists(path):
        print(f"Warning: {path} not found, skipped.")
        return

    included.add(path)

    out.write(f"\n// ===== Begin {path} =====\n")

    with open(path, encoding="utf8") as f:
        for line in f:
            m = include_regex.match(line)
            if m:
                header_name = m.group(1)
                header_path = os.path.join(os.path.dirname(path), header_name)

                # 展开头文件
                if os.path.exists(header_path):
                    expand_file(header_path, out)

                    # 如果是 .h 或 .hpp，尝试包含对应的 .cpp
                    ext = os.path.splitext(header_name)[1].lower()
                    if ext in (".h", ".hpp"):
                        impl_path = os.path.splitext(header_path)[0] + ".cpp"
                        if os.path.exists(impl_path):
                            expand_file(impl_path, out)
                continue

            out.write(line)

    out.write(f"// ===== End {path} =====\n")

def main():
    with open(OUTPUT_FILE, "w", encoding="utf8") as out:
        expand_file(ENTRY_FILE, out)

    print("Generated:", OUTPUT_FILE)

if __name__ == "__main__":
    main()
