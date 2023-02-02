while (<>) {
    s/([a-zA-Z])(\1)*/\1/g;
    print;
}
