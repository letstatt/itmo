#! /bin/bash

if ! [[ -f "$1.sh" ]]
then
echo '#!/bin/bash' > $1.sh
chmod +x $1.sh
fi

if [[ "$2" == "s" ]]
then
./$1.sh
exit
fi

nano $1.sh
