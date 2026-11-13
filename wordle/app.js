function choose_word() {
    return fetch("http://127.0.0.1:5500/data/mots.txt")
        .then(response => response.text())
        .then(data => data.split(/\r?\n/))
        .then(mots => {
            const mot_aleatoire = mots[Math.floor(Math.random() * mots.length)];
            return mot_aleatoire
        })    
    
}

function add_letter(nl, nc, letter){
    let line = document.getElementById(nl)
    const ch = line.children
    ch[nc].innerText = letter.toUpperCase()

}

function remove_letter(nl, nc){    
    let line = document.getElementById(nl)
    const ch = line.children
    ch[nc].innerText = ""
}

function check_win(nl, mot){
    
    const line = document.getElementById(nl)
    let motFinal = ""
    const cases = line.children
    for(const lettre of cases){
        motFinal += lettre.textContent
    }

    if(mot == motFinal){
        for(const lettre of cases){
            lettre.classList.add("case_good")
        }
        return true
    }else{

        // Calcul + précis du feedback
        // On commence d'abord par les verts
        let first_fb = []
        let restes = []
        for (let i = 0; i < 5; i++) {
            if(mot[i] == motFinal[i]){
                cases[i].classList.add("case_good")
                first_fb.push(2)
            }else{
                first_fb.push(0)
                restes.push(mot[i])
            }
        }

        for(let i = 0; i < 5; i++){
            if(first_fb[i] === 0){
                if(restes.includes(motFinal[i])){
                    cases[i].classList.add("case_mid")
                    // On enleve une occurence de la lettre
                    restes.splice(restes.indexOf(motFinal[i]), 1)
                }else{
                    cases[i].classList.add("case_wrong")
                }
            }
        }

        return false
    }

}

async function main(){
    const word = await choose_word()
    let currentLine = 1
    let currentCase = 0
    document.addEventListener("keydown", (event) => {
        if(event.key == "Enter"){
            if(currentCase > 4){
                if(check_win(currentLine, word)){
                    alert("Gagné !")
                }else{
                    
                    currentLine += 1
                    currentCase = 0
                    if(currentLine > 6){
                        alert("Vous avez perdu... Le mot était : " + word)
                    }
                }
            }
        }
        else if(event.key == "Backspace"){
            if(currentCase - 1 >= 0){
                remove_letter(currentLine, currentCase - 1)
                if(currentCase > 0)
                    currentCase -= 1
            }
        }
        else{
            if(currentCase <= 4 && /^[a-zA-Z]$/.test(event.key)){
                add_letter(currentLine, currentCase, event.key)
                currentCase += 1
            }
        }
    })

}

main()
