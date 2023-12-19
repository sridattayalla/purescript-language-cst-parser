export function logthis(x){
    console.log(indent, x)
    return;
}

export function logTwo(s){
    return (x)=>{
        console.log(indent, s, JSON.stringify(x))
        return
    }
}

let indent = "";

export function addIndent(){
    indent += "  "
}

export function removeIndent(){
    indent = indent.substring(0, indent.length - 2)
}

export function join(x){
    return (arr)=>
        arr.join(x);
}