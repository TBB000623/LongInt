# merge.py
import re
import os
import sys

# 主文件名（你说的是 LongInt.cpp）
ENTRY_FILE = "LongInt.cpp"

included = set()  # 防止重复展开同一头文件
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
                if os.path.exists(header):  # 本地头文件
                    expand_file(header, out)
                # 系统头（<xxx>）直接忽略，不输出
                continue
            out.write(line)

    out.write(f"\n// ===== End {path} =====\n")

def main():
    with open("submit.cpp", "w", encoding="utf8") as out:
        expand_file(ENTRY_FILE, out)

    print("submit.cpp generated successfully!")

if __name__ == "__main__":
    main()
