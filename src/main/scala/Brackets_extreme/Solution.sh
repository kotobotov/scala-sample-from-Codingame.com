declare -A ClosingPartForBracet
ClosingPartForBracet[\{]="}"
ClosingPartForBracet[\(]=")"
ClosingPartForBracet[\<]=">"
ClosingPartForBracet[\[]="]"


function helper(){
local head=${1:0:1}
local tail=${1:1}
local stack=$2

case $head in
"")
    echo $stack EMPTY
    ;;
\{|\<|\[\).*)
    echo $(helper $tail $head$stack)
    ;;
\}|\>|\]\).*)
    [[ ${ClosingPartForBracet[${stack:0:1}]}==$head ]] && echo $(helper $tail ${stack:1}) || echo "false"
    ;;
*)
    echo $(helper $tail $stack)
    ;;
esac
}

[[ $(helper $1 "")=='' ]] && echo 'true' || echo 'false'