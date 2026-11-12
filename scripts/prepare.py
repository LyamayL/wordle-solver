import csv 

corresp = {

    "é è ê ë": "e",
    "ö ô": "o",
    "ï î": "i",
    "à â": "a",
    "ç": "c",
    "ù": "u"
    

}

mots = []

with open("../data/lexique.tsv", encoding="utf-8") as f:
    reader = csv.DictReader(f, delimiter="\t")

    for d in reader:
        orth = d["ortho"]
        f = ""
        for l in orth:
            final_letter = l
            for c in corresp:
                if l in c:
                    final_letter = corresp[c]

            f += final_letter.upper()

        mots.append(f)

final_mots = set(mots)

with open("../data/mots.txt", "w") as f:
    for elt in final_mots:
        f.writelines(elt + "\n")