while (<>) {
    s/\(.*?\)/\(\)/g;
    print;
}
