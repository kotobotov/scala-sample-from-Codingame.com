declare -A ClosingPartForBracet
ClosingPartForBracet[\{]="}"
ClosingPartForBracet[\(]=")"
ClosingPartForBracet[\<]=">"
ClosingPartForBracet[\[]="]"

read -r expression

function helper(){
local head=${1:0:1}
local tail=${1:1}
local stack=$2

case $head in
"")
    echo $stack
    ;;
[\[\(\{\(\<])
    echo $(helper "$tail" "$head$stack")
    ;;
[\]\)\}\)\>])
   [[ "$stack" != "" && "${ClosingPartForBracet["${stack:0:1}"]}" = $head ]] && echo $(helper "$tail" "${stack:1}") || echo "false"
    ;;
*)
    echo $(helper "$tail" "$stack")
    ;;
esac
}

[[ $(helper $expression "") == "" ]] && echo 'true' || echo 'false'