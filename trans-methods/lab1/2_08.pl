while (<>) {
    s/\b([0-9]+)0\b/\1/g;
    print;
}
