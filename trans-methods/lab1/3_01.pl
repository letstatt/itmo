$insert_blank_line = 0;
$text_begins = 0;
 
while (<>) {
    if (/\S/) {
        $text_begins = 1;
    }
     
    if ($text_begins) {
        # remove heading and trailing spaces
        s/^\s+//;
        s/\s+$//;
        # shrink spaces
        s/\s+/ /g;
         
        if (/^$/) {
            $insert_blank_line = 1;
        } else {
            if ($insert_blank_line) {
                $insert_blank_line = 0;
                print "\n";
            }
            print;
            print "\n";
        }
    }
}
