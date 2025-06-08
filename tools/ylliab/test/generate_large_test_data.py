#!/usr/bin/env python3
def generate_large_yaml(filename, array_size=5000):
    with open(filename, 'w') as f:
        f.write("# Large YAML test data with {} element array\n".format(array_size))
        f.write("parsers:\n")
        f.write("  - name: \"large_array_test\"\n")
        f.write("    pos: 0\n")
        f.write("    root:\n")
        f.write("      name: \"large_data\"\n")
        f.write("      type: \"block\"\n")
        f.write("      span_start: 0\n")
        f.write("      span_size: {}\n".format(array_size * 4))
        f.write("      active: true\n")
        f.write("      children:\n")
        f.write("        - name: \"count\"\n")
        f.write("          type: \"integer\"\n")
        f.write("          value: \"{}\"\n".format(array_size))
        f.write("          span_start: 0\n")
        f.write("          span_size: 4\n")
        f.write("          active: true\n")
        f.write("        - name: \"numbers\"\n")
        f.write("          type: \"array\"\n")
        f.write("          span_start: 4\n")
        f.write("          span_size: {}\n".format((array_size - 1) * 4))
        f.write("          active: true\n")
        f.write("          children:\n")

        for i in range(array_size):
            f.write("            - name: \"[{}]\"\n".format(i))
            f.write("              type: \"integer\"\n")
            f.write("              value: \"{}\"\n".format(i))
            f.write("              span_start: {}\n".format(4 + i * 4))
            f.write("              span_size: 4\n")
            f.write("              active: true\n")

if __name__ == "__main__":
    generate_large_yaml("large_test_data.yaml", 5000)