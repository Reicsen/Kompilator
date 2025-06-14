# Ryszard Chereźniak, 272333

import sys
import typing
import ply
import ply.lex
import ply.yacc

brakBledow: bool = True
brakOstrzezen: bool = True

class zmienna:
    def __init__(self, nazwaFunkcji: str, identyfikator: str, typ:str, rodzaj: str, dodatkoweDane):
        self.funkcja: str = nazwaFunkcji
        self.nazwa: str = identyfikator
        self.typ:str = typ
        self.typZmiennej: str = rodzaj
        self.dodatkoweDane = dodatkoweDane
        if(typ=="tablica" and rodzaj=="lokalna"):
            if(dodatkoweDane[0]>dodatkoweDane[1]):
                global brakBledow
                brakBledow=False
                print("Błąd - podczas deklaracji tablicy "+identyfikator+" w procedurze "+nazwaFunkcji+" podano indeks górny niższy od dolnego: "+identyfikator+"["+str(dodatkoweDane[0])+":"+str(dodatkoweDane[1])+"]!\n")

    def __eq__(self, innaZmienna):
        return ((self.funkcja==innaZmienna.funkcja) and (self.nazwa==innaZmienna.nazwa))

    def __str__(self):
        return(self.funkcja+"\t"+self.nazwa+"\t"+self.typ+"\t"+self.typZmiennej+"\t"+str(self.dodatkoweDane))


class procedura:
    def __init__(self, nazwa: str, parametry: typing.List[zmienna], zmienne: typing.List[zmienna], iteratory: typing.List[zmienna]):
        self.nazwa = nazwa
        self.listaParametrow = parametry
        self.listaZmiennych = zmienne
        self.listaIteratorow = iteratory
        self.liczbaWywolan = 0

    def __eq__(self, innaProcedura):
        return (self.nazwa==innaProcedura.nazwa)

    def __str__(self):
        return self.nazwa




####################### LEKSER #######################





# lista tokenów:
tokens = ('SREDNIK', 'PRZECINEK', 'MAIN', 'PROCEDURA', 'IS', 'BEGIN', 'END', 'IDENTYFIKATOR', 'PRZYPISANIE', 'IF', 'THEN', 'ELSE', 'ENDIF', 'WHILE', 'DO', 'ENDWHILE', 'REPEAT', 'UNTIL', 'FOR', 'FROM', 'TO', 'DOWNTO', 'ENDFOR', 'READ', 'WRITE', 'LEWYNAWIAS', 'PRAWYNAWIAS', 'LEWYNAWIASKWADRATOWY', 'PRAWYNAWIASKWADRATOWY', 'DWUKROPEK', 'TABLICA', 'PLUS', 'MINUS', 'MNOZENIE', 'DZIELENIE', 'MODULO', 'CZYROWNE', 'CZYROZNE', 'CZYWIEKSZE', 'CZYMNIEJSZE', 'CZYWIEKSZEROWNE', 'CZYMNIEJSZEROWNE', 'LICZBA', 'KOMENTARZ')


# zasady proste:
#symbole
t_PRZYPISANIE = r':='
t_DWUKROPEK = ':'
t_SREDNIK = ';'
t_PRZECINEK = ','
t_LEWYNAWIAS = r'\('
t_PRAWYNAWIAS = r'\)'
t_LEWYNAWIASKWADRATOWY = r'\['
t_PRAWYNAWIASKWADRATOWY = r'\]'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MNOZENIE = r'\*'
t_DZIELENIE = r'/'
t_MODULO = r'\%'
t_CZYMNIEJSZEROWNE = r'<='
t_CZYWIEKSZEROWNE = r'>='
t_CZYMNIEJSZE = r'<'
t_CZYWIEKSZE = r'>'
t_CZYROZNE = r'\!='
t_CZYROWNE = r'='
#słowa kluczowe i identyfikatory - w przypadku prostych reguł jak poniżej, stosowana jest zasada najdłuższego dopasowania
t_MAIN = r'PROGRAM'
t_PROCEDURA = r'PROCEDURE'
t_IS = r'IS'
t_BEGIN = r'BEGIN'
t_END = r'END'
t_IF = r'IF'
t_THEN = r'THEN'
t_ELSE = r'ELSE'
t_ENDIF = r'ENDIF'
t_WHILE = r'WHILE'
t_DO = r'DO'
t_ENDWHILE = r'ENDWHILE'
t_REPEAT = r'REPEAT'
t_UNTIL = r'UNTIL'
t_FOR = r'FOR'
t_FROM = r'FROM'
t_TO = r'TO'
t_DOWNTO = r'DOWNTO'
t_ENDFOR = r'ENDFOR'
t_READ = r'READ'
t_WRITE = r'WRITE'
t_TABLICA = r'T'


# ignorowane wyrażenia regularne - komentarze i przedłużenia linii
t_ignore_KOMENTARZ = r'\#.*'

# liczba:
def t_LICZBA(t):
    r'\d+'
    t.value = int(t.value) #wartość liczbowa
    return t

# indentyfikator:
def t_IDENTYFIKATOR(t):
    r'[_a-z]+'
    t.value = str(t.value) #nazwa
    return t


# ignorowane symbole (białe znaki) - lista znaków, a nie wyrażenie regularne:
t_ignore = ' \t'


# licznik linii

def t_newline(t):
    r'\n'
    t.lexer.lineno += 1

# obsługa „błędów" - nieznanych symboli i wyrażeń:
def t_error(t):
    global brakBledow
    brakBledow = False
    linie = str(t.value).splitlines()
    print("Błąd - nieznane wyrażenie w linii "+str(t.lineno)+".: "+linie[0])
    t.lexer.skip(1)


#stworzenie obiektu leksera
lekser = ply.lex.lex()




####################### Zmienne i funkcje wykorzystywane w parserze #######################




nazwaF = ""
deklaracjeProcedur: typing.List[procedura] = []
tempParametrow = []
tempZmiennych = []
tempIteratorow = []
tempArgumentow = []
tempWywolanProcedur = []
tempOperacji = []
zakresyBlokow = []
listaOperacji = []
listaStalych = [0,1]
czyMnozenie: bool = False
czyDzielenieLubModulo: bool = False

def dodawanieDeklaracji(linia: int):
    global nazwaF, deklaracjeProcedur, tempParametrow, tempZmiennych, tempIteratorow, brakBledow

    temp1=[]
    temp2=[]
    temp3=[]

    #dodawanie deklaracji parametrów

    for dana in tempParametrow:
        nowaZmienna = zmienna(nazwaF,dana[0],dana[1],"parametr",[[False,False,False,False]])    # [czy jest nadpisywany; czy jest nadpisywany, o ile jakieś bloki nie zostaną przeskoczone; czy wymagane, żeby podstawiany argument był zainicjowany; czy może być wymagane, żeby podstawiany argument był zainicjowany, jeśli któryś blok zostanie pominięty]
        for zm in temp1+temp2+temp3:
            if(zm==nowaZmienna):
                brakBledow=False
                print("Błąd - ponowne użycie nazwy "+dana[0]+" podczas deklaracji parametru w procedurze "+nazwaF+"!\n")
        temp1.append(nowaZmienna)

    #dodawanie deklaracji zmiennych

    for dana in tempZmiennych:
        if(dana[1]=="tablica"):
            nowaZmienna = zmienna(nazwaF,dana[0],dana[1],"lokalna",[dana[2],dana[3]])   # [zakres indeksów]
        else:
            nowaZmienna = zmienna(nazwaF,dana[0],dana[1],"lokalna",[])
        for zm in temp1+temp2+temp3:
            if(zm==nowaZmienna):
                brakBledow=False
                print("Błąd - ponowne użycie nazwy "+dana[0]+" podczas deklaracji zmiennej w procedurze "+nazwaF+"!\n")
        temp2.append(nowaZmienna)

    # dodawanie deklaracji iteratorów

    for dana in tempIteratorow:
        nowaZmienna = zmienna(nazwaF,dana[0],"liczba","iterator",[[dana[1],dana[2]]])   # [pary „zasięgów" iteratora]
        kontrolkaNazw: bool = False     # mogą w dwóch róznych miejscach procedury być iteratory o tej samej nazwie, więc kontrolka, żeby nie wstawić ich dwa razy (ułatwienie w kontekście zarządzania pamięcią)
        for zm in temp1+temp2+temp3:
            if(zm==nowaZmienna):
                kontrolkaNazw=True
                if(zm.typZmiennej!="iterator"):
                    brakBledow=False
                    print("Błąd - ponowne użycie nazwy "+dana[0]+" podczas deklaracji iteratora w procedurze "+nazwaF+"!\n")

        if(not kontrolkaNazw):
            temp3.append(nowaZmienna)

        else:
            for zm in temp3:
                if(nowaZmienna==zm):
                    zm.dodatkoweDane.append([dana[1],dana[2]])


    #test czy nie ma dwóch pętli „jedna w drugiej" o tym samym iteratorze

    for i in range(len(tempIteratorow)):
        for j in range(0,i):
            if(tempIteratorow[i][0]==tempIteratorow[j][0]):
                if((tempIteratorow[i][1]>=tempIteratorow[j][1] and tempIteratorow[i][1]<=tempIteratorow[j][2]) and (tempIteratorow[j][1]>=tempIteratorow[i][1] and tempIteratorow[j][1]<=tempIteratorow[i][2])):
                    brakBledow=False
                    print("Błąd - pętla wewnętrzna zaczynająca się w linii "+str(min(tempIteratorow[i][3],tempIteratorow[j][3]))+". używa iteratora o tej samej nazwie, co pętla zewnętrzna!\n")


    # dodanie deklaracji procedury

    nowaProcedura = procedura(nazwaF,temp1,temp2,temp3)
    for proc in deklaracjeProcedur:
        if(proc==nowaProcedura):
            brakBledow=False
            print("Błąd - ponowne użycie nazwy procedury "+nazwaF+" w linii "+str(linia)+".!\n")

    deklaracjeProcedur.append(nowaProcedura)

    tempParametrow=[]
    tempZmiennych=[]
    tempIteratorow=[]



def testIstnieniaProcedury(nazwa: str, nrLinii: int) -> bool:
    global deklaracjeProcedur, brakBledow

    for proc in deklaracjeProcedur:
        if(proc.nazwa==nazwa):
            return True

    brakBledow = False
    print("Błąd - wywołano niezadeklarowaną procedurę "+nazwa+" w linii "+str(nrLinii)+".!\n")
    return False



def testPoprawnosciArgumentowWywolania():
    global deklaracjeProcedur, tempWywolanProcedur, brakBledow, tempOperacji

    procWywolujaca = deklaracjeProcedur[-1]

    for wywolanie in tempWywolanProcedur:
        procWywolywana: procedura
        for proc in deklaracjeProcedur:
            if(proc.nazwa==wywolanie[0]):   # na pewno istnieje, bo tylko w takiej sytuacji dodane do listy wywołań
                procWywolywana=proc
        procWywolywana.liczbaWywolan+=1

        if(len(wywolanie[1])!=len(procWywolywana.listaParametrow)):
            brakBledow=False
            print("Błąd - użyto złej ilości argumentów przy wywoływaniu procedury "+procWywolywana.nazwa+" w procedurze "+procWywolujaca.nazwa+"!\n")

            nowaListaOperacji=[]
            for op in tempOperacji:
                if(op[0]!="wywołanie"):
                    nowaListaOperacji.append(op)
                elif(op[1]!=wywolanie[0]):
                    nowaListaOperacji.append(op)
            tempOperacji=nowaListaOperacji


        else:
            for i in range(len(wywolanie[1])):
                typ = ""
                for zm in (procWywolujaca.listaZmiennych+procWywolujaca.listaParametrow+procWywolujaca.listaIteratorow):
                    if(zm.nazwa==wywolanie[1][i]):
                        typ = zm.typ                # jeśli taka zmienna/parametr nie istnieje w procedurze wywołującej, typ będzie dalej "", więc będzie różny od poprawnego, co da błąd

                if(typ!=procWywolywana.listaParametrow[i].typ):
                    brakBledow=False
                    if(typ==""):
                        print("Błąd - użyto nieznanego argumentu "+wywolanie[1][i]+" przy wywoływaniu procedury "+procWywolywana.nazwa+" w procedurze "+procWywolujaca.nazwa+"!\n")
                    else:
                        print("Błąd - przy wywoływaniu procedury "+procWywolywana.nazwa+" w procedurze "+procWywolujaca.nazwa+" użyto jako "+str(i+1)+". argumentu zmienną "+wywolanie[1][i]+" typu "+typ+", gdy oczekiwano typu "+procWywolywana.listaParametrow[i].typ+"!\n")

                    nowaListaOperacji=[]
                    for op in tempOperacji:
                        if(op[0]!="wywołanie"):
                            nowaListaOperacji.append(op)
                        elif(op[1]!=wywolanie[0]):
                            nowaListaOperacji.append(op)
                    tempOperacji=nowaListaOperacji

    tempWywolanProcedur=[]


def usunOperacjeNieosiagalne():
    global tempOperacji, zakresyBlokow

    for blok in zakresyBlokow:
        if(blok[3] not in ["repeat-until","always","never"]):
            if(not blok[0]):
                od: int = blok[1]
                do: int = blok[2]

                for op in tempOperacji:
                    if(od<op[-1] and op[-1]<do):
                        op[-1]=-1

                for bl in zakresyBlokow:
                    if(od<bl[1] and bl[2]<do):      # bloki nieosiągalne, bo leżą wewnątrz
                        bl[0]=False

    noweBloki = []
    noweOperacje = []

    for op in tempOperacji:
        if(op[-1]!=-1):
            noweOperacje.append(op)

    for blok in zakresyBlokow:
        if(blok[0]):
            noweBloki.append(blok)

    tempOperacji=noweOperacje
    zakresyBlokow=noweBloki



def czyPoprawneTypy() -> bool:
    global brakBledow, deklaracjeProcedur, tempOperacji

    obecnaProc: procedura = deklaracjeProcedur[-1]
    wynik: bool = True

    for op in tempOperacji:
        if(op[0]!="el. tablicy" and op[0]!="wywołanie"):        # wywołania były już wcześniej
            for i in range(1,len(op)-2):    # oprócz typu operacji oraz numeru linii i pozycji
                if(type(op[i])==str and op[i] not in ["DZIAŁ","WAR","ELTABLICY"]):
                    typ: str = ""
                    typZm: str = ""
                    zasiegi=[]
                    for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych+obecnaProc.listaIteratorow:
                        if(zm.nazwa==op[i]):
                            typ = zm.typ
                            typZm = zm.typZmiennej
                            if(typZm=="iterator"):
                                zasiegi = zm.dodatkoweDane

                    if(typ!="liczba"):
                        if(typ==""):
                            brakBledow = False
                            print("Błąd - użyto nieznanej zmiennej "+op[i]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                            wynik = False
                        else:
                            brakBledow = False
                            print("Błąd - niepoprawnie użyto tablicy "+op[i]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                            wynik = False

                    else:
                        if(typZm=="iterator"):
                            czyWewn: bool = False
                            for zasieg in zasiegi:
                                if(zasieg[0]<op[-1] and op[-1]<zasieg[1]):
                                    czyWewn=True

                            if(not czyWewn):
                                brakBledow=False
                                print("Błąd - odwołanie się do iteratora "+op[i]+" poza pętlą w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")

        elif(op[0]=="el. tablicy"):
            czyTablica: bool
            if(type(op[1])!=str or op[1] in ["DZIAŁ","WAR","ELTABLICY"]):
                brakBledow = False
                print("Błąd - użyto odwołania do elementu tablicy dla elementu niebędącego tablicą w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                wynik = False
                wywolywanaTablica = None
                czyTablica = False
            else:
                typ: str = ""
                for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych+obecnaProc.listaIteratorow:
                    if(zm.nazwa==op[1]):
                        typ = zm.typ
                        wywolywanaTablica = zm
                        czyTablica = True

                if(typ!="tablica"):
                    wywolywanaTablica = None    # w przeciwnym razie, zostaje wywołana tablica
                    czyTablica = False
                    if(typ==""):
                        brakBledow = False
                        print("Błąd - użyto nieznanej zmiennej "+op[1]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                        wynik = False
                    else:
                        brakBledow = False
                        print("Błąd - użyto odwołania do elementu tablicy dla elementu niebędącego tablicą: "+op[1]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                        wynik = False

            if(type(op[2])==str):
                if(op[2] in ["DZIAŁ","WAR","ELTABLICY"]):
                    brakBledow = False
                    print("Błąd - użyto niepoprawnego wyrażenia jako indeksu tablicy w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                    wynik = False
                else:
                    typ: str = ""
                    typZm: str = ""
                    zasiegi=[]
                    for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych+obecnaProc.listaIteratorow:
                        if(zm.nazwa==op[2]):
                            typ = zm.typ
                            typZm = zm.typZmiennej
                            if(typZm=="iterator"):
                                zasiegi = zm.dodatkoweDane

                    if(typ!="liczba"):
                        if(typ==""):
                            brakBledow = False
                            print("Błąd - użyto nieznanej zmiennej "+op[2]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                            wynik = False
                        else:
                            brakBledow = False
                            print("Błąd - niepoprawnie użyto tablicy "+op[2]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")
                            wynik = False

                    else:
                        if(typZm=="iterator"):
                            czyWewn: bool = False
                            for zasieg in zasiegi:
                                if(zasieg[0]<op[-1] and op[-1]<zasieg[1]):
                                    czyWewn=True

                            if(not czyWewn):
                                brakBledow=False
                                print("Błąd - odwołanie się do iteratora "+op[2]+" poza pętlą w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")

            else:
                if(czyTablica):
                    if(wywolywanaTablica.typZmiennej=="lokalna"):
                        zakres = wywolywanaTablica.dodatkoweDane
                        if(op[2]<zakres[0] or op[2]>zakres[1]):
                            brakBledow=False
                            print("Błąd - odwołanie się wprost do indeksu "+str(op[2])+". tablicy "+op[1]+" leżącego poza jej zakresem "+str(zakres[0])+":"+str(zakres[1])+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")


    return wynik



def gdzieZmienneSaZadeklarowane():
    global deklaracjeProcedur, brakBledow, tempOperacji, zakresyBlokow

    obecnaProc: procedura = deklaracjeProcedur[-1]

    for op in tempOperacji:
        if((op[0]=="read" or op[0]==":=") and (op[1] not in ["DZIAŁ","WAR","ELTABLICY"])):
            for it in obecnaProc.listaIteratorow:
                if(it.nazwa==op[1]):
                    for zasieg in it.dodatkoweDane:
                        if(zasieg[0]<op[-1] and op[-1]<zasieg[1]):
                            brakBledow=False
                            print("Błąd - próba modyfikacji iteratora "+op[1]+" wewnątrz pętli w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")

            for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych:
                if(zm.nazwa==op[1]):

                    zasieg=[-1,-1]
                    for blok in zakresyBlokow:
                        if(blok[1]<op[-1] and op[-1]<blok[2]):  # przypisanie leży wewnątrz bloku pętli/ifa
                            if(blok[1]>zasieg[0]):              # pierwszy taki blok lub blok wewnątrz innego
                                zasieg=[blok[1],blok[2]]

                    if(zasieg[0]==-1):                  # deklaracja w głównej części procedury
                        if(zm.typZmiennej=="parametr"):
                            zm.dodatkoweDane[0][0] = True
                        czyJuzJest: bool = False
                        for d in zm.dodatkoweDane:
                            if(d[0] and type(d[1])==int):
                                d[1]=min(d[1],op[-1])
                                czyJuzJest=True
                        if(not czyJuzJest):
                            zm.dodatkoweDane.append([True,op[-1]])  # True - modyfikacja w tej procedurze lub pewna podczas wywołania w innej

                    else:                               # deklaracja w jakimś bloku wewnętrznym
                        if(zm.typZmiennej=="parametr"):
                            zm.dodatkoweDane[0][1] = True
                        czyJuzJest: bool = False
                        for d in zm.dodatkoweDane:
                            if(d[0] and type(d[1])==int):
                                if(d[1]<zasieg[0]):
                                    czyJuzJest=True
                            elif(d[0] and type(d[1])!=bool):
                                if(d[1][0]<zasieg[0] and zasieg[1]<d[1][1]):
                                    czyJuzJest=True
                        if(not czyJuzJest):
                            zm.dodatkoweDane.append([True,[op[-1],zasieg[1]]])

        elif(op[0]=="wywołanie"):       # wiadomo, że pod względem typów i ilości argumentów, wywołanie poprawne
            procWywolywana: procedura
            for proc in deklaracjeProcedur:
                if(proc.nazwa==op[1]):   # na pewno istnieje, bo tylko w takiej sytuacji dodane do listy wywołań
                    procWywolywana=proc

            for i in range(len(procWywolywana.listaParametrow)):
                par=procWywolywana.listaParametrow[i]
                if(par.typ=="liczba"):
                    if(par.dodatkoweDane[0][0] or par.dodatkoweDane[0][1]):
                        for it in obecnaProc.listaIteratorow:
                            if(it.nazwa==op[2+i]):
                                for zasieg in it.dodatkoweDane:
                                    if(zasieg[0]<op[-1] and op[-1]<zasieg[1]):
                                        brakBledow=False
                                        print("Błąd - podanie iteratora "+op[1]+" wewnątrz pętli w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+" jako parametr procedury "+op[1]+" modyfikowany przez tę procedurę!\n")

                        for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych:
                            if(zm.nazwa==op[2+i]):

                                zasieg=[-1,-1]
                                for blok in zakresyBlokow:
                                    if(blok[1]<op[-1] and op[-1]<blok[2]):  # przypisanie leży wewnątrz bloku pętli/ifa
                                        if(blok[1]>zasieg[0]):              # pierwszy taki blok lub blok wewnątrz innego
                                            zasieg=[blok[1],blok[2]]

                                if(zasieg[0]==-1):                  # deklaracja w głównej części procedury
                                    if(par.dodatkoweDane[0][0]):
                                        if(zm.typZmiennej=="parametr"):
                                            zm.dodatkoweDane[0][0] = True
                                        czyJuzJest: bool = False
                                        for d in zm.dodatkoweDane:
                                            if(d[0] and type(d[1])==int):
                                                d[1]=min(d[1],op[-1])
                                                czyJuzJest=True
                                        if(not czyJuzJest):
                                            zm.dodatkoweDane.append([True,op[-1]])

                                    else:
                                        if(zm.typZmiennej=="parametr"):
                                            zm.dodatkoweDane[0][1] = True
                                        czyJuzJest: bool = False
                                        for d in zm.dodatkoweDane:
                                            if(d[0] and type(d[1])==int):
                                                if(d[1]<op[-1]):
                                                    czyJuzJest=True
                                            elif(d[0] and type(d[1])==int):
                                                d[1]=min(d[1],op[-1])
                                                czyJuzJest=True
                                        if(not czyJuzJest):
                                            zm.dodatkoweDane.append([False,op[-1]])

                                else:                               # deklaracja w jakimś bloku wewnętrznym
                                    if(par.dodatkoweDane[0][0]):
                                        if(zm.typZmiennej=="parametr"):
                                            zm.dodatkoweDane[0][1] = True
                                        czyJuzJest: bool = False
                                        for d in zm.dodatkoweDane:
                                            if(d[0] and type(d[1])==int):
                                                if(d[1]<zasieg[0]):
                                                    czyJuzJest=True
                                            elif(d[0] and type(d[1])!=bool):
                                                if(d[1][0]<zasieg[0] and zasieg[1]<d[1][1]):
                                                    czyJuzJest=True
                                        if(not czyJuzJest):
                                            zm.dodatkoweDane.append([True,[op[-1],zasieg[1]]])

                                    else:
                                        if(zm.typZmiennej=="parametr"):
                                            zm.dodatkoweDane[0][1] = True
                                        czyJuzJest: bool = False
                                        for d in zm.dodatkoweDane:
                                            if(d[0] and type(d[1])==int):
                                                if(d[1]<op[-1]):
                                                    czyJuzJest=True
                                            elif(d[0] and type(d[1])==int):
                                                d[1]=min(d[1],op[-1])
                                                czyJuzJest=True
                                            elif(type(d[1])!=bool):
                                                if(d[1][0]<zasieg[0] and zasieg[1]<d[1][1]):
                                                    czyJuzJest=True
                                        if(not czyJuzJest):
                                            zm.dodatkoweDane.append([False,[op[-1],zasieg[1]]])




def sprawdzOdwolania():
    global tempOperacji, deklaracjeProcedur, brakBledow, brakOstrzezen

    obecnaProc: procedura = deklaracjeProcedur[-1]

    for op in tempOperacji:
        if(op[0]!="read" and op[0]!=":=" and op[0]!="el. tablicy" and op[0]!="wywołanie"):
            for i in range(1,len(op)-2):    # oprócz typu operacji oraz numeru linii i pozycji
                if(type(op[i])==str and op[i] not in ["DZIAŁ","WAR","ELTABLICY"]):
                    for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych:     # iteratory zostały już sprawdzone wcześniej
                        if(zm.nazwa==op[i]):

                            czyZadeklarowana: bool = False
                            czyCzesciowoZadeklarowana: bool = False
                            czyZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                            czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                            for d in zm.dodatkoweDane:
                                if(type(d[1])==int):
                                    if(d[1]<op[-1]):
                                        if(d[0]):
                                            czyZadeklarowana = True
                                        else:
                                            czyZadeklarowanaPoprzezNiepewnaProcedure = True
                                else:
                                    if(type(d[1])!=bool):
                                        if(d[1][0]<op[-1]):
                                            if(op[-1]<d[1][1]):
                                                if(d[0]):
                                                    czyZadeklarowana = True
                                                else:
                                                    czyZadeklarowanaPoprzezNiepewnaProcedure = True
                                            else:
                                                if(d[0]):
                                                    czyCzesciowoZadeklarowana = True
                                                else:
                                                    czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure = True

                            if(zm.typZmiennej=="lokalna"):
                                if(not czyZadeklarowana):
                                    if(czyZadeklarowanaPoprzezNiepewnaProcedure):
                                        brakOstrzezen = False
                                        print("Ostrzeżenie - zmienna "+op[i]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                    elif(czyCzesciowoZadeklarowana):
                                        brakOstrzezen = False
                                        print("Ostrzeżenie - zmienna "+op[i]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie w blokach warunkowych lub pętli. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                    elif(czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                        brakOstrzezen = False
                                        print("Ostrzeżenie - zmienna "+op[i]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie w bloku warunkowym lub pętli, poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach. W przypadku, jeśli któreś z wymienionych bloków zostaną pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                    else:
                                        brakBledow=False
                                        print("Błąd - odwołano się do niezainicjowanej zmiennej "+op[i]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")

                            else:
                                if(not czyZadeklarowana):
                                    if(czyCzesciowoZadeklarowana or czyZadeklarowanaPoprzezNiepewnaProcedure or czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                        zm.dodatkoweDane[0][3] = True
                                    else:
                                        zm.dodatkoweDane[0][2] = True

        elif(op[0]!="read" and op[0]!="wywołanie"):
            if(type(op[2])==str and op[2] not in ["DZIAŁ","WAR","ELTABLICY"]):
                for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych:     # iteratory zostały już sprawdzone wcześniej
                    if(zm.nazwa==op[2]):

                        czyZadeklarowana: bool = False
                        czyCzesciowoZadeklarowana: bool = False
                        czyZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                        czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                        for d in zm.dodatkoweDane:
                            if(type(d[1])==int):
                                if(d[1]<op[-1]):
                                    if(d[0]):
                                        czyZadeklarowana = True
                                    else:
                                        czyZadeklarowanaPoprzezNiepewnaProcedure = True
                            else:
                                if(type(d[1])!=bool):
                                    if(d[1][0]<op[-1]):
                                        if(op[-1]<d[1][1]):
                                            if(d[0]):
                                                czyZadeklarowana = True
                                            else:
                                                czyZadeklarowanaPoprzezNiepewnaProcedure = True
                                        else:
                                            if(d[0]):
                                                czyCzesciowoZadeklarowana = True
                                            else:
                                                czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure = True

                        if(zm.typZmiennej=="lokalna"):
                            if(not czyZadeklarowana):
                                if(czyZadeklarowanaPoprzezNiepewnaProcedure):
                                    brakOstrzezen = False
                                    print("Ostrzeżenie - zmienna "+op[2]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                elif(czyCzesciowoZadeklarowana):
                                    brakOstrzezen = False
                                    print("Ostrzeżenie - zmienna "+op[2]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie w blokach warunkowych lub pętli. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                elif(czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                    brakOstrzezen = False
                                    print("Ostrzeżenie - zmienna "+op[2]+" w procedurze "+obecnaProc.nazwa+" przed wywołaniem w linii "+str(op[-2])+". inicjowana była jedynie w bloku warunkowym lub pętli, poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach. W przypadku, jeśli któreś z wymienionych bloków zostaną pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                else:
                                    brakBledow=False
                                    print("Błąd - odwołano się do niezainicjowanej zmiennej "+op[2]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+"!\n")

                        else:
                            if(not czyZadeklarowana):
                                if(czyCzesciowoZadeklarowana or czyZadeklarowanaPoprzezNiepewnaProcedure or czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                    zm.dodatkoweDane[0][3] = True
                                else:
                                    zm.dodatkoweDane[0][2] = True

        elif(op[0]=="wywołanie"):           # na pewno poprawne
            procWywolywana: procedura
            for proc in deklaracjeProcedur:
                if(proc.nazwa==op[1]):
                    procWywolywana=proc

            for i in range(len(procWywolywana.listaParametrow)):
                par=procWywolywana.listaParametrow[i]
                if(par.typ=="liczba"):
                    if(par.dodatkoweDane[0][2] or par.dodatkoweDane[0][3]):
                        for zm in obecnaProc.listaParametrow+obecnaProc.listaZmiennych:         # iteratory zostały już sprawdzone wcześniej
                            if(zm.nazwa==op[2+i]):

                                czyZadeklarowana: bool = False
                                czyCzesciowoZadeklarowana: bool = False
                                czyZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                                czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure: bool = False
                                for d in zm.dodatkoweDane:
                                    if(type(d[1])==int):
                                        if(d[1]<op[-1]):
                                            if(d[0]):
                                                czyZadeklarowana = True
                                            else:
                                                czyZadeklarowanaPoprzezNiepewnaProcedure = True
                                    else:
                                        if(type(d[1])!=bool):
                                            if(d[1][0]<op[-1]):
                                                if(op[-1]<d[1][1]):
                                                    if(d[0]):
                                                        czyZadeklarowana = True
                                                    else:
                                                        czyZadeklarowanaPoprzezNiepewnaProcedure = True
                                                else:
                                                    if(d[0]):
                                                        czyCzesciowoZadeklarowana = True
                                                    else:
                                                        czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure = True

                                if(zm.typZmiennej=="lokalna"):
                                    if(not czyZadeklarowana):
                                        if(czyZadeklarowanaPoprzezNiepewnaProcedure):
                                            brakOstrzezen = False
                                            if(par.dodatkoweDane[0][2]):
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach, zaś procedura ta wymaga, aby ten parametr był zainicjowany. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                            else:
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach, zaś procedura ta może wymagać, aby ten parametr był zainicjowany, jeżeli ominięte zostaną w niej bloki inicjujące ten parametr. W przypadku, jeśli zostaną one pominięte również w procedurze wcześniej potencjalnie inicjującej, zmienna może być niezainicjowana w momencie odwołania!\n")
                                        elif(czyCzesciowoZadeklarowana):
                                            brakOstrzezen = False
                                            if(par.dodatkoweDane[0][2]):
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie w blokach warunkowych lub pętli, zaś procedura ta wymaga, aby ten parametr był zainicjowany. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                            else:
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie w blokach warunkowych lub pętli, zaś procedura ta może wymagać, aby ten parametr był zainicjowany, jeżeli ominięte zostaną w niej bloki inicjujące ten parametr. W przypadku, jeśli zostaną one pominięte również w procedurze wywołującej, zmienna może być niezainicjowana w momencie odwołania!\n")
                                        elif(czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                            brakOstrzezen = False
                                            if(par.dodatkoweDane[0][2]):
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie w bloku warunkowym lub pętli, poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach, zaś procedura ta wymaga, aby ten parametr był zainicjowany. W przypadku, jeśli któreś z wymienionych bloków zostaną pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")
                                            else:
                                                print("Ostrzeżenie - zmienna "+op[2+i]+" w procedurze "+obecnaProc.nazwa+" przed wykorzystaniem jako argument procedury "+op[1]+" w linii "+str(op[-2])+". inicjowana była jedynie w bloku warunkowym lub pętli, poprzez użycie w innej procedurze, w której inicjacja następuje jedynie w blokach warunkowych lub pętlach, zaś procedura ta może wymagać, aby ten parametr był zainicjowany, jeżeli ominięte zostaną w niej bloki inicjujące ten parametr. W przypadku, jeśli zostaną pominięte również wspomniane bloki w procedurze wywołującej lub wcześniej potencjalnie inicjującej, zmienna może być niezainicjowana w momencie odwołania!\n")
                                        else:
                                            if(par.dodatkoweDane[0][2]):
                                                brakBledow=False
                                                print("Błąd - użyto niezainicjowanej zmiennej "+op[2+i]+" jako parametr przy wywołaniu procedury "+op[1]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+", zaś procedura ta wymaga, aby ten parametr był zainicjowany!\n")
                                            else:
                                                brakOstrzezen = False
                                                print("Ostrzeżenie - użyto niezainicjowanej zmiennej "+op[2+i]+" jako parametr przy wywołaniu procedury "+op[1]+" w linii "+str(op[-2])+". w procedurze "+obecnaProc.nazwa+", zaś procedura ta może wymagać, aby ten parametr był zainicjowany, jeżeli ominięte zostaną w niej bloki inicjujące ten parametr. W przypadku, jeśli zostaną one pominięte, zmienna może być niezainicjowana w momencie odwołania!\n")

                                else:
                                    if(not czyZadeklarowana):
                                        if(czyCzesciowoZadeklarowana or czyZadeklarowanaPoprzezNiepewnaProcedure or czyCzesciowoZadeklarowanaPoprzezNiepewnaProcedure):
                                            zm.dodatkoweDane[0][3] = True
                                        else:
                                            zm.dodatkoweDane[0][2] = True


def ogolnyBlokOperacyjny(p):
    global nazwaF, tempOperacji, zakresyBlokow, listaOperacji
    dodawanieDeklaracji(p.lineno(1))
    testPoprawnosciArgumentowWywolania()
    usunOperacjeNieosiagalne()
    czyDobre: bool = czyPoprawneTypy()
    if(czyDobre):
        gdzieZmienneSaZadeklarowane()
        sprawdzOdwolania()

    zakresyBlokow = przeksztalcBlokiDoPostaciOperacyjnej(zakresyBlokow)
    posortujOperacjeIBloki(tempOperacji,zakresyBlokow)
    tempOperacji = wlozBlokiDoListyOperacji(tempOperacji,zakresyBlokow)
    listaOperacji.append([nazwaF,tempOperacji])
    tempOperacji=[]
    zakresyBlokow=[]


def przydzielPamiec(deklaracje: typing.List[procedura]) -> typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]]:
    global brakBledow, listaStalych

    pamiec: typing.List[str] = ["ACC","POMOCNICZY1","POMOCNICZY2"]
    adresy: typing.List[int] = [0,1,2]
    wartosciPolTablicowych: typing.List[typing.Tuple[int,int]] = []
    licznikAdresu: int = 3

    pamiec += ["MNOZENIE1","MNOZENIE2","MNOZENIE.TEMP_1","MNOZENIE.TEMP_2","MNOZENIE.KONTR","MNOZENIE.WYNIK","MNOZENIE.RETURN"]
    for i in range(7):
        adresy.append(licznikAdresu)
        licznikAdresu +=1

    pamiec += ["DZIELENIE1","DZIELENIE2","DZIELENIE.KONTR1","DZIELENIE.KONTR2","DZIELENIE.TEMP1","DZIELENIE.TEMP2","DZIELENIE.WYNIK","DZIELENIE.MODULO","DZIELENIE.RETURN"]
    for i in range(9):
        adresy.append(licznikAdresu)
        licznikAdresu +=1

    for proc in deklaracje:
        for zm in proc.listaZmiennych:
            pamiec.append(proc.nazwa+"."+zm.nazwa)
            adresy.append(licznikAdresu)
            licznikAdresu+=1
            if(zm.typ=="tablica"):
                od: int = zm.dodatkoweDane[0]
                do: int = zm.dodatkoweDane[1]
                wartosciPolTablicowych.append([licznikAdresu-1,licznikAdresu-od])   # bo adres(tab[0])=adres(tab)+1-minindeks, ale licznik już zwiększony o 1; jeśli tablica numerowana od 0, to wskaźnik jest na kolejne pole po polu wskazującym
                licznikAdresu+=(do-od+1)

        for par in proc.listaParametrow:
            pamiec.append(proc.nazwa+"."+par.nazwa)
            adresy.append(licznikAdresu)
            licznikAdresu+=1

        for it in proc.listaIteratorow:
            pamiec.append(proc.nazwa+"."+it.nazwa)
            adresy.append(licznikAdresu)
            licznikAdresu+=1
            pamiec.append(proc.nazwa+"."+it.nazwa+".KONIEC")
            adresy.append(licznikAdresu)
            licznikAdresu+=1

        if(proc.nazwa!="PROGRAM"):
            pamiec.append(proc.nazwa+".RETURN")
            adresy.append(licznikAdresu)
            licznikAdresu+=1

    listaStalych.sort()
    ind: int = 0
    while(ind<len(listaStalych)-1):
        if(listaStalych[ind]==listaStalych[ind+1]):
            listaStalych.pop(ind)
        else:
            ind+=1

    for i in listaStalych:
        pamiec.append("STAŁA "+str(i))
        adresy.append(licznikAdresu)
        licznikAdresu+=1

    licznikAdresu-=1

    if(licznikAdresu>(2**62)):     # w zasadzie niemożliwe; zawarte jedynie z powodów formalnych
        brakBledow=False
        print("Błąd - przekroczono dopuszczony limit rozmiaru pamięci!\n")
        return [],[],[]

    return pamiec,adresy,wartosciPolTablicowych



def posortujOperacjeIBloki(operacje, bloki):
    czyCosZmieniono: bool = True
    while(czyCosZmieniono):
        czyCosZmieniono = False
        for i in range (len(operacje)-1):
            if(operacje[i][-1]>operacje[i+1][-1]):      #takich sytuacji będzie mało i zazwyczaj dla przesunięć o 1-2, więc nie opłaca się robić w n*ln(n)
                temp = operacje[i+1]
                operacje[i+1] = operacje[i]
                operacje[i] = temp
                czyCosZmieniono=True

    czyCosZmieniono = True
    while(czyCosZmieniono):
        czyCosZmieniono = False
        for i in range (len(bloki)-1):
            if(bloki[i][-1]>bloki[i+1][-1]):      #takich sytuacji będzie mało i zazwyczaj dla przesunięć o 1-2, więc nie opłaca się robić w n*ln(n)
                temp = bloki[i+1]
                bloki[i+1] = bloki[i]
                bloki[i] = temp
                czyCosZmieniono=True

    i: int = 0
    while(i<len(operacje)):
        if(operacje[i][0] in ["+","-","*","/","%","<",">","<=",">=","!=","==","zakres fora to","zakres fora downto"]):      #nie zaburzy późniejszego wkładania w środek bloków, a będzie wykorzystane przy tłumaczeniu działań, gdzie drugim arguemntem będzie element tablicy
            if(str(operacje[i][2])=="ELTABLICY"):
                temp = operacje[i+1]
                operacje[i+1] = operacje[i]
                operacje[i] = temp
                i+=2         #żeby nie przesuwać „w nieskończoność"
            else:
                i+=1
        elif(operacje[i][0] in ["write", "read"]):
            if(str(operacje[i][1])=="ELTABLICY"):
                temp = operacje[i+1]
                operacje[i+1] = operacje[i]
                operacje[i] = temp
                i+=2         #żeby nie przesuwać „w nieskończoność"
            else:
                i+=1
        else:
            i+=1


def usunNieuzywaneProcedury():
    global listaOperacji, deklaracjeProcedur

    nowaListaProcedur: typing.List[procedura] = []

    for proc in deklaracjeProcedur:
        if(proc.nazwa!="PROGRAM" and proc.liczbaWywolan==0):
            nowaListaOperacji=[]
            for op in listaOperacji:
                if(op[0]!=proc.nazwa):
                    nowaListaOperacji.append(op)
            listaOperacji=nowaListaOperacji

        else:
            nowaListaProcedur.append(proc)

    deklaracjeProcedur=nowaListaProcedur


def przeksztalcBlokiDoPostaciOperacyjnej(listaBlokow):
    nowaListaBlokow = []
    for blok in listaBlokow:
        if(blok[3]=="while"):
            nowaListaBlokow.append(["while",blok[4],blok[4]])
            nowaListaBlokow.append(["endwhile",blok[4],blok[2]])
        elif(blok[3]=="repeat-until"):
            nowaListaBlokow.append(["repeat-until",blok[2],blok[1]])
            nowaListaBlokow.append(["endrepeat-until",blok[2],blok[2]])
        elif(blok[3]=="if"):
            nowaListaBlokow.append(["if",blok[4],blok[4]])
            nowaListaBlokow.append(["endif",blok[4],blok[2]])
        elif(blok[3]=="else"):
            for bl in nowaListaBlokow:
                if(bl[0]=="endif" and bl[1]==blok[4]):  # na pewno istnieje, bo nie always-if/always-else
                    bl[0]="else"
            nowaListaBlokow.append(["endif",blok[4],blok[2]])
        elif(blok[3]=="for to"):
            nowaListaBlokow.append(["for to",blok[4],blok[5],blok[5]])
            nowaListaBlokow.append(["endfor to",blok[4],blok[5],blok[2]])
        elif(blok[3]=="for downto"):
            nowaListaBlokow.append(["for downto",blok[4],blok[5],blok[5]])
            nowaListaBlokow.append(["endfor downto",blok[4],blok[5],blok[2]])

        elif(blok[3]=="never"):     # repeat-until z zawsze fałszywym warunkiem liczbowym - przechodzimy raz i nie ma żadnych skoków; reszta typów bloków, przy zawsze fałszywym warunku byłaby pomijana, więc została usunięta już wcześniej
            None

        elif(blok[3]=="always"):
            if(blok[4]=="if" or blok[4]=="else"):       # if/else z zawsze prawdziwym warunkiem liczbowym - przechodzimy raz i nie ma żadnych skoków
                None
            elif(blok[4]=="while"):
                nowaListaBlokow.append(["alwayswhile",blok[5],blok[5]])
                nowaListaBlokow.append(["endalwayswhile",blok[5],blok[2]])
            elif(blok[4]=="repeat-until"):
                nowaListaBlokow.append(["alwaysrepeat-until",blok[2],blok[1]])
                nowaListaBlokow.append(["endalwaysrepeat-until",blok[2],blok[2]])
            elif(blok[4]=="for to"):
                nowaListaBlokow.append(["for to",blok[5],blok[6],blok[6]])
                nowaListaBlokow.append(["endfor to",blok[5],blok[6],blok[2]])
            elif(blok[4]=="for downto"):
                nowaListaBlokow.append(["for downto",blok[5],blok[6],blok[6]])
                nowaListaBlokow.append(["endfor downto",blok[5],blok[6],blok[2]])

    return nowaListaBlokow


def wlozBlokiDoListyOperacji(operacje,bloki):
    nowaListaOperacji=[]

    indOp: int = 0
    indBl: int = 0
    while(indOp<len(operacje) and indBl<len(bloki)):
        if(operacje[indOp][-1]<bloki[indBl][-1]):
            nowaListaOperacji.append(operacje[indOp])
            indOp+=1
        else:
            nowaListaOperacji.append(bloki[indBl])
            indBl+=1

    if(indOp<len(operacje)):
        nowaListaOperacji.extend(operacje[indOp:])
    else:
        nowaListaOperacji.extend(bloki[indBl:])

    return nowaListaOperacji




def kompilacja1(operacjeProcedury,schematPamieci):
    global deklaracjeProcedur, czyMnozenie, czyDzielenieLubModulo

    pamiec=schematPamieci[0]
    adresy=schematPamieci[1]

    procedura: str = operacjeProcedury[0]
    for p in deklaracjeProcedur:
        if(p.nazwa==procedura):
            proc = p

    parametry = []
    for par in proc.listaParametrow:
        parametry.append(par.nazwa)

    instrukcje=["label: "+procedura]
    stos=[]
    ostatnioPrzypisane=[]
    ostatnieMnozenie=[]
    ostatnieDzielenie=[]
    slownik={}
    czyUsuwanie: bool = False
    dokadUsuwac=""
    dlaJakichIfowUsuwacGdyBedzieElse=[]
    dlaJakichEndifZostawiac=[]

    for zm in (proc.listaParametrow+proc.listaZmiennych+proc.listaIteratorow):
        slownik[zm.nazwa]=""

    for op in operacjeProcedury[1]:
        if(op[0] in ["else", "endif"] and op[1] in dlaJakichIfowUsuwacGdyBedzieElse):
            czyUsuwanie=True
            dokadUsuwac=["endif",op[1]]
            dlaJakichIfowUsuwacGdyBedzieElse.remove(op[1])


        if(czyUsuwanie):
            if(dokadUsuwac[0]=="endfor to" or dokadUsuwac[0]=="endfor downto"):
                if(op[0]==dokadUsuwac[0] and op[2]==dokadUsuwac[1]):
                    czyUsuwanie=False
                    dokadUsuwac=""
            elif(dokadUsuwac[0]=="endwhile"):
                if(op[0]==dokadUsuwac[0] and op[1]==dokadUsuwac[1]):
                    czyUsuwanie=False
                    dokadUsuwac=""
            elif(dokadUsuwac[0]=="endif"):
                if((op[0]==dokadUsuwac[0] or op[0]=="else") and op[1]==dokadUsuwac[1]):
                    czyUsuwanie=False
                    dokadUsuwac=""
                    if(op[0]=="else"):
                        dlaJakichEndifZostawiac.append(op[1])


        elif(op[0]=="el. tablicy"):
            tab=op[1]
            ind=op[2]
            temp=[]
            if(type(ind)==int):
                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+tab)])]
                if(ind!=0):
                    temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(ind))])]

            else:
                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+tab)])]
                if(ind in parametry):
                    l = slownik[ind]
                    if(type(l)==int and "STAŁA "+str(l) in pamiec):
                        if(l!=0):
                            temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(l))])]
                    else:
                        temp += ["ADDI "+str(adresy[pamiec.index(procedura+"."+ind)])]
                else:
                    temp += ["ADD "+str(adresy[pamiec.index(procedura+"."+ind)])]

            ostatnioPrzypisane=[]

            stos.append(temp)   # wynikiem operacji jest adres komórki, nie jej wartość


        elif(op[0] in ["+","-","*","/","%"]):
            arg1=op[1]
            arg2=op[2]
            temp=[]
            potencjalnaZmianaWLiczbe=[]

            if(type(arg1)==int):
                potencjalnaZmianaWLiczbe.append(arg1)
            elif(arg1 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg1])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg1])
            elif(arg1 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg1)
            else:
                potencjalnaZmianaWLiczbe.append(arg1)

            if(type(arg2)==int):
                potencjalnaZmianaWLiczbe.append(arg2)
            elif(arg2 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg2])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg2])
            elif(arg2 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg2)
            else:
                potencjalnaZmianaWLiczbe.append(arg2)

            p0=potencjalnaZmianaWLiczbe[0]
            p1=potencjalnaZmianaWLiczbe[1]

            if(op[0]=="+"):
                if(type(p0)==int and type(p1)==int and ("STAŁA "+str(p0+p1) in pamiec or (p0+p1) in ostatnioPrzypisane)):
                    arg1=p0
                    arg2=p1
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1

            elif(op[0]=="-"):
                if(type(p0)==int and type(p1)==int and ("STAŁA "+str(p0-p1) in pamiec or (p0-p1) in ostatnioPrzypisane)):
                    arg1=p0
                    arg2=p1
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1

            elif(op[0]=="*"):
                if(type(p0)==int and type(p1)==int and ((p0*p1) in range(-9223372036854775808,9223372036854775808) or (p0*p1) in ostatnioPrzypisane)):
                    arg1=p0
                    arg2=p1
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1

            elif(op[0]=="/"):
                if(type(p1)==int and p1==0):
                    arg2 = p1
                    if(type(p0)==int):
                        arg1=p0
                elif(type(p0)==int and type(p1)==int and ((p0//p1) in range(-9223372036854775808,9223372036854775808) or (p0//p1) in ostatnioPrzypisane)):
                    arg1=p0
                    arg2=p1
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1

            elif(op[0]=="%"):
                if(type(p1)==int and p1==0):
                    arg2 = p1
                    if(type(p0)==int):
                        arg1=p0
                elif(type(p0)==int and type(p1)==int and ((p0%p1) in range(-9223372036854775808,9223372036854775808) or (p0%p1) in ostatnioPrzypisane)):
                    arg1=p0
                    arg2=p1
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1



            if(str(arg1)!="ELTABLICY" and str(arg2)!="ELTABLICY"):
                if(type(arg1)==int and type(arg2)==int):            # tylko dla albo zbyt dużych/małych wyników działań podanych stałymi, albo, gdy zmienne zostały zmienione w liczby dla przyspieszenia
                    if(op[0]=="+"):
                        if(arg1+arg2 not in ostatnioPrzypisane):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["ADD 0"]
                            elif("STAŁA "+str(arg1+arg2) in pamiec):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1+arg2))])]
                            elif(arg1 in ostatnioPrzypisane):
                                temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            elif(arg2 in ostatnioPrzypisane):
                                temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]

                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(arg1+arg2)

                    elif(op[0]=="-"):
                        if(arg1-arg2 not in ostatnioPrzypisane):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane or arg1==arg2):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            elif("STAŁA "+str(arg1-arg2) in pamiec):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1-arg2))])]
                            elif(arg1 in ostatnioPrzypisane):
                                temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]

                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(arg1-arg2)

                    elif(op[0]=="%"):       # na pewno w zakresie
                        if(arg2!=0):
                            if(arg1%arg2 not in ostatnioPrzypisane):
                                if("STAŁA "+str(arg1%arg2) in pamiec):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1%arg2))])]
                                else:
                                    temp += ["SET "+str(arg1%arg2)]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1%arg2)
                        else:
                            if(0 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)

                    elif(op[0]=="/"):
                        if(arg2!=0):
                            if(arg1//arg2 in range(-9223372036854775808,9223372036854775808) or arg1//arg2 in ostatnioPrzypisane):
                                if(arg1//arg2 not in ostatnioPrzypisane):
                                    if("STAŁA "+str(arg1//arg2) in pamiec):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1//arg2))])]
                                    else:
                                        temp += ["SET "+str(arg1//arg2)]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg1//arg2)
                            else:
                                if(-arg1 not in ostatnioPrzypisane):        # możliwe tylko dla (min int)/-1
                                    if(0 not in ostatnioPrzypisane):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(-arg1)

                        else:
                            if(0 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)

                    elif(op[0]=="*"):
                        if(arg1*arg2 in range(-9223372036854775808,9223372036854775808) or arg1*arg2 in ostatnioPrzypisane):
                            if(arg1*arg2 not in ostatnioPrzypisane):
                                if("STAŁA "+str(arg1*arg2) in pamiec):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1*arg2))])]
                                else:
                                    temp += ["SET "+str(arg1*arg2)]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1*arg2)
                        else:
                            if(abs(arg1)>17 and abs(arg1) not in [31,32,33,63,64,65,128] and abs(arg2)>17 and abs(arg2) not in [31,32,33,63,64,65,128]):
                                if(abs(arg2)>abs(arg1)):
                                    aaa: int = arg2
                                    arg2 = arg1
                                    arg1 = aaa

                                if(arg1*arg2 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==int and type(ostatnieMnozenie[1])==int and ostatnieMnozenie[0]*ostatnieMnozenie[1]==arg1*arg2) or (type(ostatnieMnozenie[0])==int and type(ostatnieMnozenie[1])==str and ((arg1==ostatnieMnozenie[0] and arg2 in ostatnioPrzypisane and ostatnieMnozenie[1] in ostatnioPrzypisane) or (arg2==ostatnieMnozenie[0] and arg1 in ostatnioPrzypisane and ostatnieMnozenie[1] in ostatnioPrzypisane))) or (type(ostatnieMnozenie[1])==int and type(ostatnieMnozenie[0])==str and ((arg1==ostatnieMnozenie[1] and arg2 in ostatnioPrzypisane and ostatnieMnozenie[0] in ostatnioPrzypisane) or (arg2==ostatnieMnozenie[1] and arg1 in ostatnioPrzypisane and ostatnieMnozenie[0] in ostatnioPrzypisane))))):
                                        temp += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]
                                    else:
                                        if(arg1 in ostatnioPrzypisane):
                                            temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                                            if(arg1!=arg2):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                            temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]
                                        elif(arg2 in ostatnioPrzypisane):
                                            temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                                            if(arg1!=arg2):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                                            temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                                            if(arg1!=arg2):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                            temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]

                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg1*arg2)

                            else:
                                if(abs(arg1)>abs(arg2)):
                                    aaa: int = arg2
                                    arg2 = arg1
                                    arg1 = aaa

                                if(arg1*arg2 not in ostatnioPrzypisane):
                                    if(arg1>0):
                                        if(arg2 not in ostatnioPrzypisane):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        if(arg1==2):                                                        # arg1 = 0 lub 1 niemożliwe
                                            temp += ["ADD 0"]
                                        elif(arg1==3):
                                            temp += ["ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==4):
                                            temp += ["ADD 0","ADD 0"]
                                        elif(arg1==5):
                                            temp += ["ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==6):
                                            temp += ["ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==7):
                                            temp += ["ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==8):
                                            temp += ["ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==9):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==10):
                                            temp += ["ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==11):
                                            temp += ["ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==12):
                                            temp += ["ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0"]
                                        elif(arg1==13):
                                            temp += ["ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==14):
                                            temp += ["ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==15):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==16):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==17):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==31):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==32):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==33):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==63):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==64):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==65):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==128):
                                            temp += ["ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                    else:
                                        if(arg1==-1):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==2):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==3):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-4):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0"]
                                        elif(arg1==-5):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-6):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==-7):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-8):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==-9):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-10):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==-11):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-12):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0"]
                                        elif(arg1==-13):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-14):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0"]
                                        elif(arg1==-15):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-16):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==-17):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-31):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-32):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==-33):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-63):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-64):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        elif(arg1==-65):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                        elif(arg1==-128):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg1*arg2)


                elif(type(arg1)==int):
                    if(op[0]=="+"):
                        if(arg2 in ostatnioPrzypisane):
                            if(arg1!=0):
                                temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                                ostatnioPrzypisane=[]
                        else:
                            if(arg1!=0 and arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                            if(arg2 in parametry):
                                temp += ["ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            else:
                                temp += ["ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                            ostatnioPrzypisane=[]
                            if(arg1==0):
                                ostatnioPrzypisane.append(arg2)


                    elif(op[0]=="-"):
                        if(arg1 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                        if(arg2 in parametry):
                            temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                        else:
                            temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                        ostatnioPrzypisane=[]

                    elif(op[0]=="%"):
                        if(arg1!=0):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)
                            else:
                                if(arg1 in ostatnioPrzypisane):
                                    if(0 not in ostatnioPrzypisane):
                                        if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                            if(arg2 in parametry):
                                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                            temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        else:
                                            temp += ["STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                            if(arg2 in parametry):
                                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                            czyDzielenieLubModulo = True
                                            ostatnieDzielenie=[arg1,arg2]
                                        ostatnioPrzypisane=[]
                                elif(arg2 in ostatnioPrzypisane):
                                    if(0 not in ostatnioPrzypisane):
                                        if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==int and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                            temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        else:
                                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                            czyDzielenieLubModulo = True
                                            ostatnieDzielenie=[arg1,arg2]
                                        ostatnioPrzypisane=[]
                                else:
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==int and ostatnieDzielenie[0]==arg1))):
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                        else:
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1)

                    elif(op[0]=="/"):
                        if(arg1!=0):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):      # ta sama, ale niezerowa wartość
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 1")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(1)
                            elif(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==int and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            else:
                                if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==int and ostatnieDzielenie[0]==arg1))):
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyDzielenieLubModulo = True
                                    ostatnieDzielenie=[arg1,arg2]
                                ostatnioPrzypisane=[]

                        else:
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1)

                    elif(op[0]=="*"):
                        if(abs(arg1)>17 and abs(arg1) not in [31,32,33,63,64,65,128]):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                if(arg1*arg1 in range(-9223372036854775808,9223372036854775808)):
                                    if("STAŁA "+str(arg1*arg1) in pamiec):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1*arg1))])]
                                    else:
                                        temp += ["SET "+str(arg1*arg1)]
                                else:
                                    if(len(ostatnieMnozenie)>0 and (ostatnieMnozenie[0] in ostatnioPrzypisane and ostatnieMnozenie[1] in ostatnioPrzypisane)):
                                        temp += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1*arg1)
                            elif(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg2 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg2 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==int and ostatnieMnozenie[0]==arg1 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==int and ostatnieMnozenie[1]==arg1 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            else:
                                if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg2 and type(ostatnieMnozenie[1])==int and ostatnieMnozenie[1]==arg1) or (type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg2 and type(ostatnieMnozenie[0])==int and ostatnieMnozenie[0]==arg1))):
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyMnozenie = True
                                    ostatnieMnozenie=[arg1,arg2]
                                ostatnioPrzypisane=[]

                        else:
                            if(arg1==0):
                                if(arg1 not in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg1)
                            elif(arg1>0):
                                if(arg2 not in ostatnioPrzypisane):
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                if(arg1==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                elif(arg1==3):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                elif(arg1==5):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==6):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                elif(arg1==7):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                elif(arg1==9):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==10):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                elif(arg1==11):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==12):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                elif(arg1==13):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==14):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                elif(arg1==15):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==17):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==31):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==33):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==63):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==65):
                                    if(arg2 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                elif(arg1==128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                if(arg1==1):
                                    if(arg2 not in ostatnioPrzypisane):
                                        ostatnioPrzypisane=[]
                                        ostatnioPrzypisane.append(arg2)
                                else:
                                    ostatnioPrzypisane=[]

                            else:
                                if(arg2 in ostatnioPrzypisane):
                                    temp.append("JZERO podzialaniu"+str(op[-1]))
                                    if(arg1==-1):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==2):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==3):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-4):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                    elif(arg1==-5):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-6):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-7):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-8):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-9):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-10):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-11):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-12):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                    elif(arg1==-13):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-14):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-15):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-16):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-17):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-31):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-32):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-33):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-63):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-64):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-65):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-128):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                else:
                                    if(arg1==-1):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==2):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                    elif(arg1==3):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-4):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                    elif(arg1==-5):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-6):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-7):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-8):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-9):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-10):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-11):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-12):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0"]
                                    elif(arg1==-13):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-14):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"ADD 0"]
                                    elif(arg1==-15):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-16):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-17):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-31):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-32):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-33):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-63):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-64):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg1==-65):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    elif(arg1==-128):
                                        if(arg2 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                ostatnioPrzypisane=[]
                            temp.append("label: podzialaniu"+str(op[-1]))


                elif(type(arg2)==int):
                    if(op[0]=="+"):
                        if(arg1 in ostatnioPrzypisane):
                            if(arg2!=0):
                                temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                ostatnioPrzypisane=[]
                        else:
                            if(arg2!=0 and arg2 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            if(arg1 in parametry):
                                temp += ["ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            else:
                                temp += ["ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]

                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(arg1)


                    elif(op[0]=="-"):
                        if(arg1 in ostatnioPrzypisane):
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                                ostatnioPrzypisane=[]
                        else:
                            if(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]

                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(arg1)

                    elif(op[0]=="%"):
                        if(abs(arg2)>2):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)
                            elif(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==int and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            else:
                                if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==int and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    czyDzielenieLubModulo = True
                                    ostatnieDzielenie=[arg1,arg2]
                                ostatnioPrzypisane=[]

                        else:
                            if(abs(arg2)<=1):
                                if(0 not in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(0)
                            elif(arg2==2):
                                if(arg1 not in ostatnioPrzypisane):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                if(arg1 in parametry):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","ADD 0","STORE 1","LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB 1"]
                                else:
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","ADD 0","STORE 1","LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB 1"]

                                ostatnioPrzypisane=[]
                            else:
                                if(arg1 not in ostatnioPrzypisane):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                if(arg1 in parametry):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                else:
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]

                                ostatnioPrzypisane=[]

                            temp.append("label: podzialaniu"+str(op[-1]))

                    elif(op[0]=="/"):
                        if(abs(arg2) not in [0,1,2,4,8,16,32,64,128,256,512,1024]):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 1")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(1)
                            elif(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==int and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            else:
                                if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==int and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyDzielenieLubModulo = True
                                    ostatnieDzielenie=[arg1,arg2]
                                ostatnioPrzypisane=[]

                        else:
                            if(arg2==0):
                                if(arg2 not in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg2)
                            elif(arg2>0):
                                if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                    if(arg1!=1):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 1")])]
                                        ostatnioPrzypisane=[]
                                        ostatnioPrzypisane.append(1)
                                else:
                                    if(arg1 not in ostatnioPrzypisane):
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    if(arg2==2):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF"]
                                    elif(arg2==4):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF"]
                                    elif(arg2==8):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF"]
                                    elif(arg2==16):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF"]
                                    elif(arg2==32):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF"]
                                    elif(arg2==64):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                                    elif(arg2==128):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                    elif(arg2==256):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                    elif(arg2==512):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                    elif(arg2==1024):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]

                                    if(arg2!=1):
                                        ostatnioPrzypisane=[]

                                    else:
                                        if(arg1 not in ostatnioPrzypisane):
                                            ostatnioPrzypisane=[]
                                            ostatnioPrzypisane.append(arg1)

                            else:
                                if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 1")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(1)
                                else:
                                    if(arg1 in ostatnioPrzypisane):
                                        temp.append("JZERO podzialaniu"+str(op[-1]))
                                        if(arg2==-1):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        elif(arg2==-2):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF"]
                                        elif(arg2==-4):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF"]
                                        elif(arg2==-8):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF"]
                                        elif(arg2==-16):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF"]
                                        elif(arg2==-32):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-64):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-128):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-256):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-512):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-1024):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                    else:
                                        if(arg2==-1):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        elif(arg2==-2):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF"]
                                        elif(arg2==-4):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF"]
                                        elif(arg2==-8):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF"]
                                        elif(arg2==-16):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF"]
                                        elif(arg2==-32):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-64):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-128):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-256):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-512):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                        elif(arg2==-1024):
                                            if(arg1 in parametry):
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                                            else:
                                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]

                                ostatnioPrzypisane=[]
                            temp.append("label: podzialaniu"+str(op[-1]))

                    elif(op[0]=="*"):
                        if(abs(arg2)>17 and abs(arg2) not in [31,32,33,63,64,65,128]):
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                if(arg2*arg2 in range(-9223372036854775808,9223372036854775808)):
                                    if("STAŁA "+str(arg2*arg2) in pamiec):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2*arg2))])]
                                    else:
                                        temp += ["SET "+str(arg2*arg2)]
                                else:
                                    if(len(ostatnieMnozenie)>0 and (ostatnieMnozenie[0] in ostatnioPrzypisane and ostatnieMnozenie[1] in ostatnioPrzypisane)):
                                        temp += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg2*arg2)
                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg1 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg1 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            elif(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==int and ostatnieMnozenie[0]==arg2 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==int and ostatnieMnozenie[1]==arg2 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]
                            else:
                                if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg1 and type(ostatnieMnozenie[1])==int and ostatnieMnozenie[1]==arg2) or (type(ostatnieMnozenie[0])==int and ostatnieMnozenie[0]==arg2 and type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg1))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyMnozenie = True
                                    ostatnieMnozenie=[arg1,arg2]
                                ostatnioPrzypisane=[]

                        else:
                            if(arg2==0):
                                if(arg2 not in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg2)
                            elif(arg2>0):
                                if(arg1 not in ostatnioPrzypisane):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                if(arg2==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                elif(arg2==3):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                elif(arg2==5):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==6):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                elif(arg2==7):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                elif(arg2==9):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==10):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                elif(arg2==11):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==12):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                elif(arg2==13):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==14):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                elif(arg2==15):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==17):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==31):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==33):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==63):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==65):
                                    if(arg1 in parametry):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                elif(arg2==128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                if(arg2==1):
                                    if(arg1 not in ostatnioPrzypisane):
                                        ostatnioPrzypisane=[]
                                        ostatnioPrzypisane.append(arg1)
                                else:
                                    ostatnioPrzypisane=[]

                            else:
                                if(arg1 in ostatnioPrzypisane):
                                    temp.append("JZERO podzialaniu"+str(op[-1]))
                                    if(arg2==-1):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==2):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==3):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-4):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                    elif(arg2==-5):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-6):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-7):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-8):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-9):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-10):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-11):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-12):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                    elif(arg2==-13):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-14):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-15):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-16):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-17):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-31):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-32):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-33):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-63):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-64):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-65):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-128):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                else:
                                    if(arg2==-1):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==2):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                    elif(arg2==3):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-4):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                    elif(arg2==-5):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-6):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-7):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-8):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-9):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-10):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-11):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-12):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0"]
                                    elif(arg2==-13):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-14):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                    elif(arg2==-15):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-16):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-17):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-31):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-32):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-33):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-63):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-64):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                    elif(arg2==-65):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    elif(arg2==-128):
                                        if(arg1 in parametry):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg1)]),"JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                ostatnioPrzypisane=[]
                            temp.append("label: podzialaniu"+str(op[-1]))


                else:
                    if(op[0]=="+"):
                        if(arg1==arg2):
                            if(arg1 in ostatnioPrzypisane):
                                temp += ["ADD 0"]
                            else:
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD 0"]
                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(arg2 in parametry):
                                    temp += ["ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    temp += ["ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            elif(arg2 in ostatnioPrzypisane):
                                if(arg1 in parametry):
                                    temp += ["ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                else:
                                    temp += ["ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            else:
                                if(arg1 in parametry):
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    if(arg2 in parametry):
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                        ostatnioPrzypisane=[]

                    elif(op[0]=="-"):
                        if(arg1==arg2 or (arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane)):
                            if(0 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)
                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(arg2 in parametry):
                                    temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            else:
                                if(arg1 in parametry):
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    if(arg2 in parametry):
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            ostatnioPrzypisane=[]

                    elif(op[0]=="%"):
                        if((arg1==arg2) or (arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane)):
                            if(0 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)
                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            else:
                                if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                                    czyDzielenieLubModulo = True
                                    ostatnieDzielenie=[arg1,arg2]

                                ostatnioPrzypisane=[]

                    elif(op[0]=="/"):
                        if((arg1==arg2) or (arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane)):
                            if(arg2 not in ostatnioPrzypisane):
                                if(arg2 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                temp += ["JZERO 2","LOAD "+str(adresy[pamiec.index("STAŁA 1")])]        # bo a/a=1, ale dla /0 ma być 0
                                ostatnioPrzypisane=[]

                            else:
                                if(0 not in ostatnioPrzypisane):
                                    w: int = 0
                                    for p in ostatnioPrzypisane:
                                        if(type(p)==int):
                                            w=p

                                    if(w!=0):
                                        if(w!=1):
                                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 1")])]
                                            ostatnioPrzypisane=[]
                                            ostatnioPrzypisane.append(1)
                                    else:
                                        temp += ["JZERO 2","LOAD "+str(adresy[pamiec.index("STAŁA 1")])]        # bo a/a=1, ale dla /0 ma być 0
                                        ostatnioPrzypisane=[]

                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (ostatnieDzielenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1) and (ostatnieDzielenie[1] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyDzielenieLubModulo = True
                                        ostatnieDzielenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            else:
                                if(len(ostatnieDzielenie)>0 and ((type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg2) and (type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg1))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyDzielenieLubModulo = True
                                    ostatnieDzielenie=[arg1,arg2]

                                ostatnioPrzypisane=[]

                    elif(op[0]=="*"):
                        if((arg1==arg2) or (arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane)):
                            if(0 not in ostatnioPrzypisane):
                                if(len(ostatnieMnozenie)>0 and (ostatnieMnozenie[0] in ostatnioPrzypisane and ostatnieMnozenie[1] in ostatnioPrzypisane)):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in ostatnioPrzypisane):
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    else:
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                ostatnioPrzypisane=[]


                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg2 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg2 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                                        if(arg2 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            elif(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg1 and ostatnieMnozenie[1] in ostatnioPrzypisane) or (type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg1 and ostatnieMnozenie[0] in ostatnioPrzypisane))):
                                        temp += ["JZERO podzialaniu"+str(op[-1])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    else:
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                                        if(arg1 in parametry):
                                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        else:
                                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                        temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                        czyMnozenie = True
                                        ostatnieMnozenie=[arg1,arg2]
                                    ostatnioPrzypisane=[]

                            else:
                                if(len(ostatnieMnozenie)>0 and ((type(ostatnieMnozenie[0])==str and type(ostatnieMnozenie[0])==str) and ((ostatnieMnozenie[0]==arg1 and ostatnieMnozenie[1]==arg2) or (ostatnieMnozenie[1]==arg1 and ostatnieMnozenie[0]==arg2)))):
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                else:
                                    if(arg1 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                                    if(arg2 in parametry):
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                                    czyMnozenie = True
                                    ostatnieMnozenie=[arg1,arg2]

                                ostatnioPrzypisane=[]


            elif(str(arg1)=="ELTABLICY" and str(arg2)!="ELTABLICY"):
                if(type(arg2)==int):
                    if(op[0]=="+"):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["LOADI 0"]
                        if(arg2!=0):
                            temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]

                    elif(op[0]=="-"):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["LOADI 0"]
                        if(arg2!=0):
                            temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]

                    elif(op[0]=="%"):
                        if(abs(arg2)>2):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        else:
                            if(abs(arg2)<=1):
                                stos.pop()
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            elif(arg2==2):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE 2","HALF","ADD 0","STORE 1","LOAD 2","SUB 1"]
                            else:
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE 1","HALF","ADD 0","SUB 1"]
                            temp.append("label: podzialaniu"+str(op[-1]))

                    elif(op[0]=="/"):
                        if(abs(arg2) not in [0,1,2,4,8,16,32,64,128,256,512,1024]):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        else:
                            if(arg2==0):
                                stos.pop()
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            elif(arg2==1):
                                temp += stos.pop()
                                temp += ["LOADI 0"]
                            elif(arg2==2):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF"]
                            elif(arg2==4):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF"]
                            elif(arg2==8):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF"]
                            elif(arg2==16):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF"]
                            elif(arg2==32):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==64):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==128):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==256):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==512):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==1024):
                                temp += stos.pop()
                                temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-1):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1"]
                            elif(arg2==-2):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF"]
                            elif(arg2==-4):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF"]
                            elif(arg2==-8):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF"]
                            elif(arg2==-16):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF"]
                            elif(arg2==-32):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-64):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-128):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-256):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-512):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]
                            elif(arg2==-1024):
                                temp += stos.pop()
                                temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA 0")]),"SUBI 1","JZERO podzialaniu"+str(op[-1]),"HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF","HALF"]

                            temp.append("label: podzialaniu"+str(op[-1]))

                    elif(op[0]=="*"):
                        if(abs(arg2)>17 and abs(arg2) not in [31,32,33,63,64,65,128]):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                            temp += ["SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                        else:
                            if(arg2==0):
                                stos.pop()
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            else:
                                temp += stos.pop()
                                temp.append("LOADI 0")  # dla arg2=1 to już koniec
                                if(arg2==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                elif(arg2==3):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1"]
                                elif(arg2==4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                elif(arg2==5):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==6):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1"]
                                elif(arg2==7):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                elif(arg2==9):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==10):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 1","ADD 0"]
                                elif(arg2==11):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==12):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0"]
                                elif(arg2==13):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==14):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","SUB 1","ADD 0"]
                                elif(arg2==15):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==17):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==31):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==33):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==63):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==65):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                elif(arg2==-1):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1"]
                                elif(arg2==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0"]
                                elif(arg2==3):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1"]
                                elif(arg2==-4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0"]
                                elif(arg2==-5):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-6):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0"]
                                elif(arg2==-7):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==-8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==-9):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-10):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","SUB 1","ADD 0"]
                                elif(arg2==-11):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==-12):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0"]
                                elif(arg2==-13):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-14):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 1","ADD 0"]
                                elif(arg2==-15):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==-16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==-17):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-31):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==-32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==-33):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-63):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg2==-64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg2==-65):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg2==-128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                temp.append("label: podzialaniu"+str(op[-1]))


                else:
                    if(arg2 in parametry):
                        if(op[0]=="+"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                        elif(op[0]=="-"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                        elif(op[0]=="%"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="/"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="*"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                    else:
                        if(op[0]=="+"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                        elif(op[0]=="-"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                        elif(op[0]=="%"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="/"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="*"):
                            if(arg2 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                ostatnioPrzypisane=[]
                if(type(arg2)==int and ((arg2==0 and op[0] in ["*","/","%"]) or (abs(arg2)<2 and op[0]=="%"))):
                    ostatnioPrzypisane.append(0)


            elif(str(arg1)!="ELTABLICY" and str(arg2)=="ELTABLICY"):
                if(type(arg1)==int):
                    if(op[0]=="+"):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["LOADI 0"]
                        if(arg1!=0):
                            temp += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]

                    elif(op[0]=="-"):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["STORE 1"]
                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"SUBI 1"]

                    elif(op[0]=="%"):
                        if(arg1!=0):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        else:
                            stos.pop()
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]

                    elif(op[0]=="/"):
                        if(arg1!=0):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        else:
                            stos.pop()
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]

                    elif(op[0]=="*"):
                        if(abs(arg1)>17 and abs(arg1) not in [31,32,33,63,64,65,128]):
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                            temp += ["SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                        else:
                            if(arg1==0):
                                stos.pop()
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            else:
                                temp += stos.pop()
                                temp.append("LOADI 0")  # dla arg1=1 to już koniec
                                if(arg1==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0"]
                                elif(arg1==3):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1"]
                                elif(arg1==4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0"]
                                elif(arg1==5):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==6):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1"]
                                elif(arg1==7):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0"]
                                elif(arg1==9):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==10):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 1","ADD 0"]
                                elif(arg1==11):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==12):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0"]
                                elif(arg1==13):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==14):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","SUB 1","ADD 0"]
                                elif(arg1==15):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==17):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==31):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==33):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==63):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==65):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                elif(arg1==-1):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1"]
                                elif(arg1==2):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0"]
                                elif(arg1==3):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1"]
                                elif(arg1==-4):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0"]
                                elif(arg1==-5):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-6):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0"]
                                elif(arg1==-7):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==-8):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==-9):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-10):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","SUB 1","ADD 0"]
                                elif(arg1==-11):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==-12):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0"]
                                elif(arg1==-13):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","SUB 1","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-14):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 1","ADD 0"]
                                elif(arg1==-15):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==-16):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==-17):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-31):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==-32):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==-33):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-63):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 1"]
                                elif(arg1==-64):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]
                                elif(arg1==-65):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","SUB 1"]
                                elif(arg1==-128):
                                    temp += ["JZERO podzialaniu"+str(op[-1]),"STORE 1","SUB 1","SUB 1","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0","ADD 0"]

                                temp.append("label: podzialaniu"+str(op[-1]))


                else:
                    if(arg1 in parametry):
                        if(op[0]=="+"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","ADDI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                        elif(op[0]=="-"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["STORE 1","LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI 1"]

                        elif(op[0]=="%"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="/"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="*"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                    else:
                        if(op[0]=="+"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["LOADI 0","ADD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                        elif(op[0]=="-"):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["STORE 1","LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI 1"]

                        elif(op[0]=="%"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="/"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                            temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyDzielenieLubModulo = True
                            ostatnieDzielenie=[]

                        elif(op[0]=="*"):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                            temp += ["JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")])]
                            temp += stos.pop()
                            temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                            czyMnozenie = True
                            ostatnieMnozenie=[]

                ostatnioPrzypisane=[]
                if(type(arg1)==int and (arg1==0 and op[0] in ["*","/","%"])):
                    ostatnioPrzypisane.append(arg1)


            else:
                if(op[0]=="+"):
                    if(stos[-1]==stos[-2]):
                        temp += stos.pop()
                        temp += ["LOADI 0","ADD 0"]
                        stos.pop()
                    else:
                        temp += stos.pop()
                        temp += ["STORE 1"]
                        temp += stos.pop()
                        temp += ["LOADI 0","ADDI 1"]
                    ostatnioPrzypisane=[]
                elif(op[0]=="-"):
                    if(stos[-1]==stos[-2]):
                        if(0 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(0)
                        stos.pop()
                        stos.pop()
                    else:
                        temp += stos.pop()
                        temp += ["STORE 1"]
                        temp += stos.pop()
                        temp += ["LOADI 0","SUBI 1"]

                elif(op[0]=="%"):
                    if(stos[-1]==stos[-2]):
                        if(0 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(0)
                        stos.pop()
                        stos.pop()
                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                        temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]),"label: podzialaniu"+str(op[-1])]
                        czyDzielenieLubModulo = True
                        ostatnieDzielenie=[]
                        ostatnioPrzypisane=[]

                elif(op[0]=="/"):
                    if(stos[-1]==stos[-2]):
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO 2","LOAD "+str(adresy[pamiec.index("STAŁA 1")])]  # bo a/a=1, ale dla /0 ma być 0
                        stos.pop()
                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE2")])]
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE1")])]
                        temp += ["SET podzieleniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("DZIELENIE.RETURN")]),"JUMP DZIELENIE","label: podzieleniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                        czyDzielenieLubModulo = True
                        ostatnieDzielenie=[]
                    ostatnioPrzypisane=[]

                elif(op[0]=="*"):
                    if(stos[-1]==stos[-2]):
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                        stos.pop()
                        czyMnozenie = True
                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE2")])]
                        temp += stos.pop()
                        temp += ["LOADI 0","JZERO podzialaniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE1")]),"SET pomnozeniu"+str(op[-1]),"STORE "+str(adresy[pamiec.index("MNOZENIE.RETURN")]),"JUMP MNOZENIE","label: pomnozeniu"+str(op[-1]),"LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]),"label: podzialaniu"+str(op[-1])]
                        czyMnozenie = True

                    ostatnioPrzypisane=[]
                    ostatnieMnozenie=[]


            stos.append(temp)


        elif(op[0]==":="):
            cel = op[1]
            wartosc = op[2]
            if(type(wartosc)==str and wartosc not in ["ELTABLICY","DZIAŁ"]):
                l = slownik[wartosc]
                if(type(l)==int and "STAŁA "+str(l) in pamiec):
                    wartosc=l
            temp=[]
            if(cel=="ELTABLICY"):
                if(type(wartosc)==int):
                    temp += stos.pop()
                    temp += ["STORE 2"]
                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(wartosc))]),"STOREI 2"]

                    if(wartosc not in ostatnioPrzypisane):
                        ostatnioPrzypisane=[]
                        ostatnioPrzypisane.append(wartosc)

                elif(wartosc=="ELTABLICY"):
                    if(stos[-1]!=stos[-2]):     # w przeciwnym razie podstawiamy t[a]:=t[a];
                        temp += stos.pop(-2)
                        temp += ["STORE 2"]
                        temp += stos.pop()
                        temp += ["LOADI 0","STOREI 2"]
                        ostatnioPrzypisane=[]

                    else:
                        stos.pop()
                        stos.pop()


                elif(wartosc=="DZIAŁ"):
                    temp += stos.pop(-2)
                    temp += ["STORE 2"]
                    temp += stos.pop()
                    temp += ["STOREI 2"]

                else:
                    if(wartosc in parametry):
                        temp += stos.pop()
                        temp += ["STORE 2"]
                        l = slownik[wartosc]
                        if(type(l)==int and "STAŁA "+str(l) in pamiec):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(l))]),"STOREI 2"]
                        else:
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STOREI 2"]
                    else:
                        temp += stos.pop()
                        temp += ["STORE 2","LOAD "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STOREI 2"]

                    if(wartosc not in ostatnioPrzypisane):
                        ostatnioPrzypisane=[]
                        ostatnioPrzypisane.append(wartosc)


            else:
                if(type(wartosc)==int):
                    if(wartosc not in ostatnioPrzypisane or cel not in ostatnioPrzypisane):
                        if(cel in ostatnieMnozenie):
                            if(type(slownik[cel])==int):
                                if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                    ostatnieMnozenie[0]=slownik[cel]
                                if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                    ostatnieMnozenie[1]=slownik[cel]
                            elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                                nowy: str = cel
                                for oP in ostatnioPrzypisane:
                                    if(type(oP)==str and oP!=cel):
                                        nowy=oP
                                if(nowy!=cel):
                                    if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                        ostatnieMnozenie[0]=nowy
                                    if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                        ostatnieMnozenie[1]=nowy
                                else:
                                    ostatnieMnozenie=[]
                            else:
                                ostatnieMnozenie=[]
                        if(cel in ostatnieDzielenie):
                            if(type(slownik[cel])==int):
                                if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                    ostatnieDzielenie[0]=slownik[cel]
                                if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                    ostatnieDzielenie[1]=slownik[cel]
                            elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                                nowy: str = cel
                                for oP in ostatnioPrzypisane:
                                    if(type(oP)==str and oP!=cel):
                                        nowy=oP
                                if(nowy!=cel):
                                    if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                        ostatnieMnozenie[0]=nowy
                                    if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                        ostatnieDzielenie[1]=nowy
                                else:
                                    ostatnieDzielenie=[]
                            else:
                                ostatnieDzielenie=[]
                        if(wartosc not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(wartosc))])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(wartosc)
                        if(cel in parametry):
                            temp += ["STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                        else:
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                        ostatnioPrzypisane.append(cel)

                    slownik[cel]=wartosc

                elif(wartosc=="ELTABLICY"):
                    if(cel in ostatnieMnozenie):
                        if(type(slownik[cel])==int):
                            if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                ostatnieMnozenie[0]=slownik[cel]
                            if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                ostatnieMnozenie[1]=slownik[cel]
                        elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                            nowy: str = cel
                            for oP in ostatnioPrzypisane:
                                if(type(oP)==str and oP!=cel):
                                    nowy=oP
                            if(nowy!=cel):
                                if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                    ostatnieMnozenie[0]=nowy
                                if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                    ostatnieMnozenie[1]=nowy
                            else:
                                ostatnieMnozenie=[]
                        else:
                            ostatnieMnozenie=[]
                    if(cel in ostatnieDzielenie):
                        if(type(slownik[cel])==int):
                            if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                ostatnieDzielenie[0]=slownik[cel]
                            if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                ostatnieDzielenie[1]=slownik[cel]
                        elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                            nowy: str = cel
                            for oP in ostatnioPrzypisane:
                                if(type(oP)==str and oP!=cel):
                                    nowy=oP
                            if(nowy!=cel):
                                if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                    ostatnieMnozenie[0]=nowy
                                if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                    ostatnieDzielenie[1]=nowy
                            else:
                                ostatnieDzielenie=[]
                        else:
                            ostatnieDzielenie=[]
                    if(cel in parametry):
                        temp += stos.pop()
                        temp += ["LOADI 0","STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                    ostatnioPrzypisane=[]
                    ostatnioPrzypisane.append(cel)
                    slownik[cel]=""

                elif(wartosc=="DZIAŁ"):
                    if(cel in ostatnieMnozenie):
                        if(type(slownik[cel])==int):
                            if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                ostatnieMnozenie[0]=slownik[cel]
                            if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                ostatnieMnozenie[1]=slownik[cel]
                        elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                            nowy: str = cel
                            for oP in ostatnioPrzypisane:
                                if(type(oP)==str and oP!=cel):
                                    nowy=oP
                            if(nowy!=cel):
                                if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                    ostatnieMnozenie[0]=nowy
                                if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                    ostatnieMnozenie[1]=nowy
                            else:
                                ostatnieMnozenie=[]
                        else:
                            ostatnieMnozenie=[]
                    if(cel in ostatnieDzielenie):
                        if(type(slownik[cel])==int):
                            if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                ostatnieDzielenie[0]=slownik[cel]
                            if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                ostatnieDzielenie[1]=slownik[cel]
                        elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                            nowy: str = cel
                            for oP in ostatnioPrzypisane:
                                if(type(oP)==str and oP!=cel):
                                    nowy=oP
                            if(nowy!=cel):
                                if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                    ostatnieMnozenie[0]=nowy
                                if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                    ostatnieDzielenie[1]=nowy
                            else:
                                ostatnieDzielenie=[]
                        else:
                            ostatnieDzielenie=[]
                    temp += stos.pop()
                    if(cel not in ostatnioPrzypisane):
                        if(cel in parametry):
                            temp += ["STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                        else:
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                        ostatnioPrzypisane.append(cel)
                        slownik[cel]=""
                        for o in ostatnioPrzypisane:
                            if(type(o)==int):
                                slownik[cel]=o

                else:
                    if(cel!=wartosc):
                        if(wartosc in ostatnioPrzypisane):
                            if(cel not in ostatnioPrzypisane):
                                if(cel in ostatnieMnozenie):
                                    if(type(slownik[cel])==int):
                                        if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                            ostatnieMnozenie[0]=slownik[cel]
                                        if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                            ostatnieMnozenie[1]=slownik[cel]
                                    else:
                                        ostatnieMnozenie=[]
                                if(cel in ostatnieDzielenie):
                                    if(type(slownik[cel])==int):
                                        if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                            ostatnieDzielenie[0]=slownik[cel]
                                        if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                            ostatnieDzielenie[1]=slownik[cel]
                                    else:
                                        ostatnieDzielenie=[]
                                if(cel in parametry):
                                    temp += ["STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                else:
                                    temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                ostatnioPrzypisane.append(cel)
                            # w przeciwnym razie zmienne mają na pewno te same wartości, więc nie trzeba podstawiać
                            if(type(slownik[wartosc])==int):
                                slownik[cel]=slownik[wartosc]
                            else:
                                slownik[cel]=""
                                for o in ostatnioPrzypisane:
                                    if(type(o)==int):
                                        slownik[cel]=o
                                        slownik[wartosc]=o
                        else:
                            if(cel in ostatnieMnozenie):
                                if(type(slownik[cel])==int):
                                    if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                        ostatnieMnozenie[0]=slownik[cel]
                                    if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                        ostatnieMnozenie[1]=slownik[cel]
                                elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                                    nowy: str = cel
                                    for oP in ostatnioPrzypisane:
                                        if(type(oP)==str and oP!=cel):
                                            nowy=oP
                                    if(nowy!=cel):
                                        if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                                            ostatnieMnozenie[0]=nowy
                                        if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                                            ostatnieMnozenie[1]=nowy
                                    else:
                                        ostatnieMnozenie=[]
                                else:
                                    ostatnieMnozenie=[]
                            if(cel in ostatnieDzielenie):
                                if(type(slownik[cel])==int):
                                    if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                        ostatnieDzielenie[0]=slownik[cel]
                                    if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                        ostatnieDzielenie[1]=slownik[cel]
                                elif(cel in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                                    nowy: str = cel
                                    for oP in ostatnioPrzypisane:
                                        if(type(oP)==str and oP!=cel):
                                            nowy=oP
                                    if(nowy!=cel):
                                        if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                                            ostatnieMnozenie[0]=nowy
                                        if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                                            ostatnieDzielenie[1]=nowy
                                    else:
                                        ostatnieDzielenie=[]
                                else:
                                    ostatnieDzielenie=[]
                            if(cel in parametry):
                                if(wartosc in parametry):
                                    l = slownik[wartosc]
                                    if(type(l)==int and "STAŁA "+str(l) in pamiec):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(l))]),"STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                    else:
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STOREI "+str(adresy[pamiec.index(procedura+"."+cel)])]
                            else:
                                if(wartosc in parametry):
                                    l = slownik[wartosc]
                                    if(type(l)==int and "STAŁA "+str(l) in pamiec):
                                        temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(l))]),"STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                    else:
                                        temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+wartosc)]),"STORE "+str(adresy[pamiec.index(procedura+"."+cel)])]

                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(cel)
                            ostatnioPrzypisane.append(wartosc)
                            if(type(slownik[wartosc])==int):
                                slownik[cel]=slownik[wartosc]
                                ostatnioPrzypisane.append(slownik[wartosc])
                            else:
                                slownik[cel]=""

                    # w przeciwnym razie podstawiamy n:=n, więc brak instrukcji, czyli wartość zmiennej ostatnioPrzypisane się nie zmienia


            instrukcje.extend(temp)


        elif(op[0]=="read"):
            arg = op[1]
            temp=[]
            if(arg=="ELTABLICY"):
                temp += stos.pop()
                temp += ["STORE 2","GET 0","STOREI 2"]
                ostatnioPrzypisane=[]
            else:
                if(arg in ostatnieMnozenie):
                    if(type(slownik[cel])==int):
                        if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==cel):
                            ostatnieMnozenie[0]=slownik[cel]
                        if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==cel):
                            ostatnieMnozenie[1]=slownik[cel]
                    elif(arg in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                        nowy: str = arg
                        for oP in ostatnioPrzypisane:
                            if(type(oP)==str and oP!=arg):
                                nowy=oP
                        if(nowy!=arg):
                            if(type(ostatnieMnozenie[0])==str and ostatnieMnozenie[0]==arg):
                                ostatnieMnozenie[0]=nowy
                            if(type(ostatnieMnozenie[1])==str and ostatnieMnozenie[1]==arg):
                                ostatnieMnozenie[1]=nowy
                        else:
                            ostatnieMnozenie=[]
                    else:
                        ostatnieMnozenie=[]
                if(arg in ostatnieDzielenie):
                    if(type(slownik[cel])==int):
                        if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==cel):
                            ostatnieDzielenie[0]=slownik[cel]
                        if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==cel):
                            ostatnieDzielenie[1]=slownik[cel]
                    elif(arg in ostatnioPrzypisane and len(ostatnioPrzypisane)>1):
                        nowy: str = arg
                        for oP in ostatnioPrzypisane:
                            if(type(oP)==str and oP!=arg):
                                nowy=oP
                        if(nowy!=arg):
                            if(type(ostatnieDzielenie[0])==str and ostatnieDzielenie[0]==arg):
                                ostatnieMnozenie[0]=nowy
                            if(type(ostatnieDzielenie[1])==str and ostatnieDzielenie[1]==arg):
                                ostatnieDzielenie[1]=nowy
                        else:
                            ostatnieDzielenie=[]
                    else:
                        ostatnieDzielenie=[]
                if(arg in parametry):
                    temp += ["GET 0","STOREI "+str(adresy[pamiec.index(procedura+"."+arg)])]
                    ostatnioPrzypisane=[]
                    ostatnioPrzypisane.append(arg)
                    slownik[arg]=""
                else:
                    temp += ["GET "+str(adresy[pamiec.index(procedura+"."+arg)])]
                    slownik[arg]=""
            instrukcje.extend(temp)

        elif(op[0]=="write"):
            arg = op[1]
            temp=[]
            if(type(arg)==int):
                temp += ["PUT "+str(adresy[pamiec.index("STAŁA "+str(arg))])]
            elif(arg=="ELTABLICY"):
                temp += stos.pop()
                temp += ["LOADI 0","PUT 0"]
                ostatnioPrzypisane=[]
            else:
                if(arg in parametry):
                    if(arg not in ostatnioPrzypisane):
                        l = slownik[arg]
                        if(type(l)==int and "STAŁA "+str(l) in pamiec):
                            temp += ["PUT "+str(adresy[pamiec.index("STAŁA "+str(l))])]
                        else:
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg)]),"PUT 0"]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(arg)
                    else:
                        temp += ["PUT 0"]
                else:
                    temp += ["PUT "+str(adresy[pamiec.index(procedura+"."+arg)])]
            instrukcje.extend(temp)


        elif(op[0]=="while"):
            instrukcje.append("label: while"+str(op[1]))
            stos.append(op)                         # czyszczenie list pomocniczych gdzie indziej

        elif(op[0]=="endwhile"):
            instrukcje.append("JUMP while"+str(op[1]))
            instrukcje.append("label: endwhile"+str(op[1]))
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="alwayswhile"):
            instrukcje.append("label: alwayswhile"+str(op[1]))
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="endalwayswhile"):
            instrukcje.append("JUMP alwayswhile"+str(op[1]))
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""


        elif(op[0]=="repeat-until"):
            instrukcje.append("label: repeat-until"+str(op[1]))
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="endrepeat-until"):
            stos.append(op)
            #nie wpływa na wartość zmiennej ostatnioPrzypisane, ponieważ kolejną instrukcją zawsze jest warunek logiczny, który wykonuje się po ostatniej instrukcji bloku, a na ostatnieMnozenie i ostatnieDzielenie, bo zawsze choć raz się wykona

        elif(op[0]=="alwaysrepeat-until"):
            instrukcje.append("label: alwaysrepeat-until"+str(op[1]))
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="endalwaysrepeat-until"):
            instrukcje.append("JUMP alwaysrepeat-until"+str(op[1]))
            #nie wpływa na wartość zmiennej ostatnioPrzypisane, ponieważ kolejną instrukcją zawsze jest warunek logiczny, który wykonuje się po ostatniej instrukcji bloku, a na ostatnieMnozenie i ostatnieDzielenie, bo zawsze choć raz się wykona


        elif(op[0] in ["for to","for downto"]):
            stos.append(op)
            #nie wpływa na wartości zmiennych pomocniczych, ponieważ kolejną instrukcją jest przypisanie wartości iteratorowi

        elif(op[0]=="endfor to"):
            nazwaIteratora: str = op[1]
            temp=[]
            if(nazwaIteratora not in ostatnioPrzypisane):
                temp+=["LOAD "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
            temp+=["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JZERO 5","LOAD "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"ADD "+str(adresy[pamiec.index("STAŁA 1")]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JUMP "+op[0][3:]+str(op[2]),"label: endfor to"+str(op[2])]
            instrukcje.extend(temp)
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="endfor downto"):
            nazwaIteratora: str = op[1]
            temp=[]
            if(nazwaIteratora not in ostatnioPrzypisane):
                temp+=["LOAD "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
            temp+=["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JZERO 5","LOAD "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index("STAŁA 1")]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JUMP "+op[0][3:]+str(op[2]),"label: endfor downto"+str(op[2])]
            instrukcje.extend(temp)
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""


        elif(op[0]=="if"):
            stos.append(op)
            #nie wpływa na wartość zmiennych ostatnioPrzypisane, ostatnieMnozenie i ostatnieDzielenie ponieważ kolejną instrukcją zawsze jest warunek logiczny, do którego wchodzi się tylko raz

        elif(op[0]=="else"):
            for ind in range(len(instrukcje)):
                if(instrukcje[ind]=="JUMP endif"+str(op[1])):
                    instrukcje[ind]="JUMP else"+str(op[1])
                elif(instrukcje[ind]=="JPOS endif"+str(op[1])):
                    instrukcje[ind]="JPOS else"+str(op[1])
                elif(instrukcje[ind]=="JNEG endif"+str(op[1])):
                    instrukcje[ind]="JNEG else"+str(op[1])
                elif(instrukcje[ind]=="JZERO endif"+str(op[1])):
                    instrukcje[ind]="JZERO else"+str(op[1])


            temp=["JUMP endif"+str(op[1]),"label: else"+str(op[1])]
            instrukcje.extend(temp)
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for zm in slownik:
                slownik[zm]=""

        elif(op[0]=="endif"):
            instrukcje.append("label: endif"+str(op[1]))
            if(op[1] not in dlaJakichEndifZostawiac):
                ostatnioPrzypisane=[]
                ostatnieMnozenie=[]
                ostatnieDzielenie=[]
                for zm in slownik:
                    slownik[zm]=""
            else:
                dlaJakichEndifZostawiac.remove(op[1])


        elif(op[0] in ["<",">","<=",">=","!=","=="]):
            arg1 = op[1]
            arg2 = op[2]
            temp=[]
            potencjalnaZmianaWLiczbe=[]

            if(type(arg1)==int):
                potencjalnaZmianaWLiczbe.append(arg1)
            elif(arg1 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg1])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg1])
            elif(arg1 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg1)
            else:
                potencjalnaZmianaWLiczbe.append(arg1)

            if(type(arg2)==int):
                potencjalnaZmianaWLiczbe.append(arg2)
            elif(arg2 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg2])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg2])
            elif(arg2 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg2)
            else:
                potencjalnaZmianaWLiczbe.append(arg2)

            p0=potencjalnaZmianaWLiczbe[0]
            p1=potencjalnaZmianaWLiczbe[1]


            if(type(p0)==int and type(p1)==int):
                if(stos[-1][0]=="while"):
                    if(op[0]==">"):
                        if(p0<=p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                    elif(op[0]=="<"):
                        if(p0>=p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                    elif(op[0]=="=="):
                        if(p0!=p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                    elif(op[0]=="<="):
                        if(p0>p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                    elif(op[0]==">="):
                        if(p0<p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                    elif(op[0]=="!="):
                        if(p0==p1):
                            arg1=p0
                            arg2=p1
                            czyUsuwanie=True
                            dokadUsuwac=["endwhile",stos[-1][1]]
                        else:
                            ostatnioPrzypisane=[]
                            ostatnieMnozenie=[]
                            ostatnieDzielenie=[]
                            for dana in slownik:
                                slownik[dana]=""
                else:
                    arg1=p0
                    arg2=p1
            else:
                if(stos[-1][0]=="while"):
                    ostatnioPrzypisane=[]
                    ostatnieMnozenie=[]
                    ostatnieDzielenie=[]
                    for dana in slownik:
                        slownik[dana]=""
                else:
                    if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                        arg1=p0
                    if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                        arg2=p1

            if(type(arg1)==int and type(arg2)==int):
                strukturaWywolujaca=stos.pop()  #będzie tu if, while lub endrepeat-until; while obsłużony powyżej

                if(strukturaWywolujaca[0]=="if"):
                    if(op[0]==">"):
                        if(arg1>arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]
                    elif(op[0]=="<"):
                        if(arg1<arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]
                    elif(op[0]=="=="):
                        if(arg1==arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]
                    elif(op[0]=="<="):
                        if(arg1<=arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]
                    elif(op[0]==">="):
                        if(arg>=arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]
                    elif(op[0]=="!="):
                        if(arg1!=arg2):
                            dlaJakichIfowUsuwacGdyBedzieElse.append(strukturaWywolujaca[1])
                        else:
                            czyUsuwanie=True
                            dokadUsuwac=["endif",strukturaWywolujaca[1]]

                elif(strukturaWywolujaca[0]=="endrepeat-until"):
                    if(op[0]==">"):
                        if(arg1<=arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<"):
                        if(arg1>=arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="=="):
                        if(arg1!=arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<="):
                        if(arg1>arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]==">="):
                        if(arg<arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="!="):
                        if(arg1==arg2):
                            temp += ["JUMP repeat-until"+str(strukturaWywolujaca[1])]

            else:
                if(str(arg1)!="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    if(type(arg1)==int):
                        if(arg1 in ostatnioPrzypisane):
                            if(arg2 in ostatnioPrzypisane):
                                if(0 not in ostatnioPrzypisane):
                                    temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(0)
                            else:
                                if(arg2 in parametry):
                                    temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                ostatnioPrzypisane=[]
                        else:
                            if(arg2 in parametry):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                            ostatnioPrzypisane=[]

                    elif(type(arg2)==int):
                        if(arg2!=0):
                            if(arg1 in ostatnioPrzypisane):
                                temp += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            elif(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                            ostatnioPrzypisane=[]
                        else:
                            if(arg1 not in ostatnioPrzypisane):
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(arg1)


                    else:
                        if(arg1==arg2 or (arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane)):
                            if(0 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(0)
                        else:
                            if(arg1 in ostatnioPrzypisane):
                                if(arg2 in parametry):
                                    temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                else:
                                    temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                ostatnioPrzypisane=[]
                            else:
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                if(arg2 not in ostatnioPrzypisane or 0 not in ostatnioPrzypisane):
                                    if(arg2 in parametry):
                                        temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    else:
                                        temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                                    ostatnioPrzypisane=[]
                                else:
                                    ostatnioPrzypisane=[]
                                    ostatnioPrzypisane.append(arg1)



                elif(str(arg1)=="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    if(type(arg2)==int):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp.append("LOADI 0")
                        if(arg2!=0):
                            temp.append("SUB "+str(adresy[pamiec.index("STAŁA "+str(arg2))]))

                    else:
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["LOADI 0"]
                        if(arg2 in parametry):
                            if(arg2 not in ostatnioPrzypisane or 0 not in ostatnioPrzypisane):
                                temp += ["SUBI "+str(adresy[pamiec.index(procedura+"."+arg2)])]
                        else:
                            if(arg2 not in ostatnioPrzypisane or 0 not in ostatnioPrzypisane):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+arg2)])]

                    ostatnioPrzypisane=[]


                elif(str(arg1)!="ELTABLICY" and str(arg2)=="ELTABLICY"):
                    if(type(arg1)==int):
                        temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                        temp += ["STORE 1","LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"SUBI 1"]

                    else:
                        if(arg1 in parametry):
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["STORE 1","LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI 1"]
                        else:
                            temp += stos.pop()  # jest tam tłumaczenie wzięcia elementu tablicy
                            temp += ["STORE 1","LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"SUBI 1"]

                    ostatnioPrzypisane=[]


                else:
                    if(stos[-1]==stos[-2]):
                        stos.pop()
                        stos.pop()
                        if(0 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(0)
                    else:
                        temp += stos.pop()
                        temp += ["STORE 1"]
                        temp += stos.pop()
                        temp += ["LOADI 0"]
                        temp += ["SUBI 1"]
                        ostatnioPrzypisane=[]

                strukturaWywolujaca=stos.pop()  #będzie tu if, while lub endrepeat-until

                if(strukturaWywolujaca[0]=="if"):
                    if(op[0]==">"):
                        temp += ["JPOS 2","JUMP endif"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<"):
                        temp += ["JNEG 2","JUMP endif"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="=="):
                        temp += ["JZERO 2","JUMP endif"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<="):
                        temp += ["JPOS endif"+str(strukturaWywolujaca[1])]
                    elif(op[0]==">="):
                        temp += ["JNEG endif"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="!="):
                        temp += ["JZERO endif"+str(strukturaWywolujaca[1])]

                elif(strukturaWywolujaca[0]=="while"):
                    if(op[0]==">"):
                        temp += ["JPOS 2","JUMP endwhile"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<"):
                        temp += ["JNEG 2","JUMP endwhile"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="=="):
                        temp += ["JZERO 2","JUMP endwhile"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<="):
                        temp += ["JPOS endwhile"+str(strukturaWywolujaca[1])]
                    elif(op[0]==">="):
                        temp += ["JNEG endwhile"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="!="):
                        temp += ["JZERO endwhile"+str(strukturaWywolujaca[1])]

                elif(strukturaWywolujaca[0]=="endrepeat-until"):
                    if(op[0]==">"):
                        temp += ["JPOS 2","JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<"):
                        temp += ["JNEG 2","JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="=="):
                        temp += ["JZERO 2","JUMP repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="<="):
                        temp += ["JPOS repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]==">="):
                        temp += ["JNEG repeat-until"+str(strukturaWywolujaca[1])]
                    elif(op[0]=="!="):
                        temp += ["JZERO repeat-until"+str(strukturaWywolujaca[1])]

                instrukcje.extend(temp)


        elif(op[0]=="zakres fora to"):  # przy wartości zmiennej ostatnioPrzypisane uwględnione, że, jeśli pętla się wykona i wraca na początek, to w ACC jest iterator
            arg1 = op[1]
            arg2 = op[2]
            temp=[]
            potencjalnaZmianaWLiczbe=[]

            if(type(arg1)==int):
                potencjalnaZmianaWLiczbe.append(arg1)
            elif(arg1 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg1])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg1])
            elif(arg1 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg1)
            else:
                potencjalnaZmianaWLiczbe.append(arg1)

            if(type(arg2)==int):
                potencjalnaZmianaWLiczbe.append(arg2)
            elif(arg2 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg2])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg2])
            elif(arg2 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg2)
            else:
                potencjalnaZmianaWLiczbe.append(arg2)

            p0=potencjalnaZmianaWLiczbe[0]
            p1=potencjalnaZmianaWLiczbe[1]

            if(type(p0)==int and type(p1)==int and p0>p1):
                arg1=p0
                arg2=p1
            else:
                if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                    arg1=p0
                if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                    arg2=p1

            if(type(arg1)==int and type(arg2)==int and arg1>arg2):  # for, który się nie wykona
                petla=stos.pop()
                czyUsuwanie=True
                dokadUsuwac=["endfor to",petla[2]]

            else:
                if(str(arg1)!="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    petla=stos.pop()
                    nazwaIteratora = petla[1]

                    if(type(arg1)==int and type(arg2)==int):        #gdy są dwie liczby, zawsze się wykona, bo te sprzeczne zostały już wcześniej usunięte
                        if(arg1==arg2):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)
                        else:
                            if(arg1 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)

                    elif(type(arg1)==int):
                        if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        elif(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            if(arg2 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            if(arg1!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["JNEG endfor to"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        elif(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JPOS endfor to"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                            if(arg2 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            if(arg1!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["JNEG endfor to"+str(petla[2])]
                            ostatnioPrzypisane=[]

                    elif(type(arg2)==int):
                        if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        elif(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JNEG endfor to"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        elif(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                            if(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["JPOS endfor to"+str(petla[2])]
                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(nazwaIteratora)

                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                            if(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["JPOS endfor to"+str(petla[2])]

                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(nazwaIteratora)

                    else:
                        if(arg1==arg2):
                            if(arg1 not in ostatnioPrzypisane):
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)])]

                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        else:
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)

                            elif(arg1 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                                if(arg2 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JNEG endfor to"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JNEG endfor to"+str(petla[2])]
                                ostatnioPrzypisane=[]

                            elif(arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JPOS endfor to"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JPOS endfor to"+str(petla[2])]
                                ostatnioPrzypisane=[]

                            else:
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                                if(arg2 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JNEG endfor to"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JNEG endfor to"+str(petla[2])]
                                ostatnioPrzypisane=[]


                elif(str(arg1)!="ELTABLICY" and str(arg2)=="ELTABLICY"):
                    petla=stos.pop(-2)
                    nazwaIteratora = petla[1]

                    if(type(arg1)==int):
                        if(arg1 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                        temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                    else:
                        if(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                        elif(arg1 in parametry):
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                    temp += stos.pop()
                    temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                    if(type(arg1)!=int or (type(arg1)==int and arg1!=0)):
                        temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                    temp += ["JNEG endfor to"+str(petla[2])]
                    ostatnioPrzypisane=[]

                elif(str(arg1)=="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    petla=stos.pop(-2)
                    nazwaIteratora = petla[1]


                    if(type(arg2)==int):
                        if(arg2 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                        temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                    else:
                        if(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        elif(arg2 in parametry):
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                    temp += stos.pop()
                    temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                    if(type(arg2)!=int or (type(arg2)==int and arg2!=0)):
                        temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+"KONIEC")])]
                    temp += ["JPOS endfor to"+str(petla[2])]

                    ostatnioPrzypisane=[]
                    if(type(arg2)==int and arg2==0):
                        ostatnioPrzypisane.append(nazwaIteratora)

                else:
                    petla=stos.pop(-3)
                    nazwaIteratora = petla[1]

                    if(stos[-1]==stos[-2]):
                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        stos.pop()
                        ostatnioPrzypisane=[]
                        ostatnioPrzypisane.append(nazwaIteratora)

                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JPOS endfor to"+str(petla[2])]
                        ostatnioPrzypisane=[]

                temp.append("label: for to"+str(petla[2]))
                instrukcje.extend(temp)
                ostatnieMnozenie=[]
                ostatnieDzielenie=[]
                for zm in slownik:
                    slownik[zm]=""


        elif(op[0]=="zakres fora downto"):
            arg1 = op[1]
            arg2 = op[2]
            temp=[]

            potencjalnaZmianaWLiczbe=[]

            if(type(arg1)==int):
                potencjalnaZmianaWLiczbe.append(arg1)
            elif(arg1 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg1])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg1])
            elif(arg1 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg1)
            else:
                potencjalnaZmianaWLiczbe.append(arg1)

            if(type(arg2)==int):
                potencjalnaZmianaWLiczbe.append(arg2)
            elif(arg2 not in ["DZIAŁ","WAR","ELTABLICY"] and type(slownik[arg2])==int):
                potencjalnaZmianaWLiczbe.append(slownik[arg2])
            elif(arg2 in ostatnioPrzypisane):
                if(0 in ostatnioPrzypisane):
                    potencjalnaZmianaWLiczbe.append(0)
                else:
                    w: int = 0
                    for p in ostatnioPrzypisane:
                        if(type(p)==int):
                            w=p

                    if(w!=0):
                        potencjalnaZmianaWLiczbe.append(w)
                    else:
                        potencjalnaZmianaWLiczbe.append(arg2)
            else:
                potencjalnaZmianaWLiczbe.append(arg2)

            p0=potencjalnaZmianaWLiczbe[0]
            p1=potencjalnaZmianaWLiczbe[1]

            if(type(p0)==int and type(p1)==int and p0<p1):
                arg1=p0
                arg2=p1
            else:
                if(type(p0)==int and ("STAŁA "+str(p0) in pamiec or p0 in ostatnioPrzypisane)):
                    arg1=p0
                if(type(p1)==int and ("STAŁA "+str(p1) in pamiec or p1 in ostatnioPrzypisane)):
                    arg2=p1

            if(type(arg1)==int and type(arg2)==int and arg1<arg2):  # for, który się nie wykona
                petla=stos.pop()
                czyUsuwanie=True
                dokadUsuwac=["endfor downto",petla[2]]

            else:
                if(str(arg1)!="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    petla=stos.pop()
                    nazwaIteratora = petla[1]

                    if(type(arg1)==int and type(arg2)==int):        #gdy są dwie liczby, zawsze się wykona, bo te sprzeczne zostały już wcześniej usunięte
                        if(arg1==arg2):
                            if(arg1 not in ostatnioPrzypisane):
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        else:
                            if(arg1 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                ostatnioPrzypisane=[]
                            elif(arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)


                    elif(type(arg1)==int):
                        if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        elif(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                            if(arg2 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            if(arg1!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["JPOS endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        elif(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JNEG endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                            if(arg2 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            if(arg1!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["JPOS endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]

                    elif(type(arg2)==int):
                        if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)

                        elif(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JPOS endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]

                        elif(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                            if(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["JNEG endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(nazwaIteratora)

                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                            if(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                            if(arg2!=0):
                                temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            temp += ["JNEG endfor downto"+str(petla[2])]
                            ostatnioPrzypisane=[]
                            if(arg2==0):
                                ostatnioPrzypisane.append(nazwaIteratora)

                    else:
                        if(arg1==arg2):
                            if(arg1 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            elif(arg1 in parametry):
                                temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            else:
                                temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                            ostatnioPrzypisane=[]
                            ostatnioPrzypisane.append(nazwaIteratora)
                        else:
                            if(arg1 in ostatnioPrzypisane and arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                                ostatnioPrzypisane=[]
                                ostatnioPrzypisane.append(nazwaIteratora)

                            elif(arg1 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                                if(arg2 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JPOS endfor downto"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JPOS endfor downto"+str(petla[2])]
                                ostatnioPrzypisane=[]

                            elif(arg2 in ostatnioPrzypisane):
                                temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JNEG endfor downto"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JNEG endfor downto"+str(petla[2])]
                                ostatnioPrzypisane=[]

                            else:
                                if(arg1 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                                if(arg2 in parametry):
                                    temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JPOS endfor downto"+str(petla[2])]
                                else:
                                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"JPOS endfor downto"+str(petla[2])]
                                ostatnioPrzypisane=[]


                elif(str(arg1)!="ELTABLICY" and str(arg2)=="ELTABLICY"):
                    petla=stos.pop(-2)
                    nazwaIteratora = petla[1]

                    if(type(arg1)==int):
                        if(arg1 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg1))])]
                        temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                    else:
                        if(arg1 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                        elif(arg1 in parametry):
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg1)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]

                    temp += stos.pop()
                    temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                    if(type(arg1)!=int or (type(arg1)==int and arg1!=0)):
                        temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                    temp += ["JPOS endfor downto"+str(petla[2])]
                    ostatnioPrzypisane=[]

                elif(str(arg1)=="ELTABLICY" and str(arg2)!="ELTABLICY"):
                    petla=stos.pop(-2)
                    nazwaIteratora = petla[1]


                    if(type(arg2)==int):
                        if(arg2 not in ostatnioPrzypisane):
                            temp += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(arg2))])]
                        temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                    else:
                        if(arg2 in ostatnioPrzypisane):
                            temp += ["STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        elif(arg2 in parametry):
                            temp += ["LOADI "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        else:
                            temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+arg2)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                    temp += stos.pop()
                    temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)])]
                    if(type(arg2)!=int or (type(arg2)==int and arg2!=0)):
                        temp += ["SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                    temp += ["JNEG endfor downto"+str(petla[2])]
                    ostatnioPrzypisane=[]
                    if(type(arg2)==int and arg2==0):
                        ostatnioPrzypisane.append(nazwaIteratora)

                else:
                    petla=stos.pop(-3)
                    nazwaIteratora = petla[1]

                    if(stos[-1]==stos[-2]):
                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]
                        stos.pop()
                        ostatnioPrzypisane=[]
                        ostatnioPrzypisane.append(nazwaIteratora)
                    else:
                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")])]

                        temp += stos.pop()
                        temp += ["LOADI 0","STORE "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora)]),"SUB "+str(adresy[pamiec.index(procedura+"."+nazwaIteratora+".KONIEC")]),"JNEG endfor downto"+str(petla[2])]
                        ostatnioPrzypisane=[]
                temp.append("label: for downto"+str(petla[2]))
                instrukcje.extend(temp)
                ostatnieMnozenie=[]
                ostatnieDzielenie=[]
                for zm in slownik:
                    slownik[zm]=""


        elif(op[0]=="wywołanie"):
            temp = []
            nazwaWywolywanejProcedury: str = op[1]
            for p in deklaracjeProcedur:
                if(p.nazwa==nazwaWywolywanejProcedury): #na pewno jest, bo już sprawdzane wcześniej
                    proc = p

            ustawianeParametry = proc.listaParametrow
            ktoreUstawione: typing.List[bool] = []
            for par in ustawianeParametry:
                ktoreUstawione.append(False)

            while(False in ktoreUstawione):
                ind1: int = ktoreUstawione.index(False) # pierwszy indeks
                if(ustawianeParametry[ind1].typ=="tablica" or op[2+ind1] in parametry):     #bo pola wskaźnikowe tablic oraz parametry procedury wywołującej przechowują adresy
                    temp += ["LOAD "+str(adresy[pamiec.index(procedura+"."+op[2+ind1])])]
                else:
                    temp += ["SET "+str(adresy[pamiec.index(procedura+"."+op[2+ind1])])]

                for i in range(ind1,len(ustawianeParametry)):    #na pewno można, bo przy złej ilości argumentów wywołania procedury, program nie przeszedłby do etapu kompilacji
                    if(op[2+i]==op[2+ind1]):
                        nazwaParametru = ustawianeParametry[i].nazwa
                        temp += ["STORE "+str(adresy[pamiec.index(nazwaWywolywanejProcedury+"."+nazwaParametru)])]
                        ktoreUstawione[i]=True

            temp += ["SET returncall"+str(op[-1]), "STORE "+str(adresy[pamiec.index(nazwaWywolywanejProcedury+".RETURN")]), "JUMP "+nazwaWywolywanejProcedury, "label: returncall"+str(op[-1])]
            instrukcje.extend(temp)
            ostatnioPrzypisane=[]
            ostatnieMnozenie=[]
            ostatnieDzielenie=[]
            for i in range(2,len(op)):  # pozostałe nie zostały zmodyfikowane
                slownik[op[i]]=""


    if(procedura=="PROGRAM"):
        instrukcje.append("HALT")
    else:
        instrukcje.append("RTRN "+str(adresy[pamiec.index(procedura+".RETURN")]))

    return instrukcje



def fragmentKoncowy(deklaracjaPamieci: typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]], stale: typing.List[int]) -> typing.List[str]:
    global czyMnozenie, czyDzielenieLubModulo

    stale.sort()

    pamiec: typing.List[str] = deklaracjaPamieci[0]
    adresy: typing.List[int] = deklaracjaPamieci[1]
    wskaznikiTablicowe: typing.List[typing.Tuple[int,int]] = deklaracjaPamieci[2]
    indeksJedynki: int = stale.index(1)
    dodawaneStale: typing.List[typing.Tuple[int,bool]] = []
    for i in stale:
        if(i!=0 and i!=1):
            dodawaneStale.append([i,False])
        else:
            dodawaneStale.append([i,True])

    instrukcje: typing.List[str] = ["label: BLOKKONCOWY","SET 1", "STORE "+str(adresy[pamiec.index("STAŁA 1")])]
    liczba: int = 1
    while((liczba*2) in stale):
        ind: int = stale.index(liczba*2)
        instrukcje += ["ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba*2))])]
        dodawaneStale[ind][1]=True
        liczba *= 2
    if(liczba==1):
        instrukcje += ["HALF", "STORE "+str(adresy[pamiec.index("STAŁA 0")])]
    else:
        instrukcje += ["SUB 0", "STORE "+str(adresy[pamiec.index("STAŁA 0")])]

    for i in range(indeksJedynki+1,len(stale)):
        if(not dodawaneStale[i][1]):
            liczba = stale[i]
            instrukcje += ["SET "+str(liczba), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba))])]
            dodawaneStale[i][1]=True
            czyCosZmieniono: bool = True
            while(czyCosZmieniono):
                czyCosZmieniono=False
                czyMoznaDodacKtorasMniejsza: bool = False
                coMoznaDodac: int = 0
                for j in range(indeksJedynki,i):
                    if(liczba+stale[j] in stale and not dodawaneStale[stale.index(liczba+stale[j])][1]):
                        if(not czyMoznaDodacKtorasMniejsza):
                            czyMoznaDodacKtorasMniejsza=True
                            coMoznaDodac=stale[j]

                czyMoznaOdjacKtorasMniejsza: bool = False
                coMoznaOdjac: int = 0
                for j in range(indeksJedynki,i):
                    if(liczba-stale[j] in stale and not dodawaneStale[stale.index(liczba-stale[j])][1]):
                        if(not czyMoznaOdjacKtorasMniejsza):
                            czyMoznaOdjacKtorasMniejsza=True
                            coMoznaOdjac=stale[j]


                if(czyMoznaDodacKtorasMniejsza):
                    ind = stale.index(liczba+coMoznaDodac)
                    instrukcje += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(coMoznaDodac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba+coMoznaDodac))])]
                    dodawaneStale[ind][1]=True
                    liczba += coMoznaDodac
                elif(liczba*2 in stale and not dodawaneStale[stale.index(liczba*2)][1]):
                    ind = stale.index(liczba*2)
                    instrukcje += ["ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba*2))])]
                    dodawaneStale[ind][1]=True
                    liczba *= 2
                    czyCosZmieniono=True
                elif(liczba//2 in stale and not dodawaneStale[stale.index(liczba//2)][1]):
                    ind = stale.index(liczba//2)
                    instrukcje += ["HALF", "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba//2))])]
                    dodawaneStale[ind][1]=True
                    liczba = liczba//2
                    czyCosZmieniono=True
                elif(czyMoznaOdjacKtorasMniejsza):
                    ind = stale.index(liczba-coMoznaOdjac)
                    instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(coMoznaOdjac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba-coMoznaOdjac))])]
                    dodawaneStale[ind][1]=True
                    liczba -= coMoznaOdjac
                elif((liczba-1)*2 in stale and not dodawaneStale[stale.index((liczba-1)*2)][1]):
                    ind = stale.index((liczba-1)*2)
                    instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA 1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba-1)*2))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba-1)*2
                    czyCosZmieniono=True
                elif((liczba*2)-1 in stale and not dodawaneStale[stale.index((liczba*2)-1)][1]):
                    ind = stale.index((liczba*2)-1)
                    instrukcje += ["ADD 0", "SUB "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba*2)-1))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba*2)-1
                    czyCosZmieniono=True
                elif((liczba*2)+1 in stale and not dodawaneStale[stale.index((liczba*2)+1)][1]):
                    ind = stale.index((liczba*2)+1)
                    instrukcje += ["ADD 0", "ADD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba*2)+1))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba*2)+1
                    czyCosZmieniono=True
                elif((liczba+1)*2 in stale and not dodawaneStale[stale.index((liczba+1)*2)][1]):
                    ind = stale.index((liczba+1)*2)
                    instrukcje += ["ADD "+str(adresy[pamiec.index("STAŁA 1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba+1)*2))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba+1)*2
                    czyCosZmieniono=True


    for i in range(indeksJedynki-2,-1,-1):
        if(not dodawaneStale[i][1]):
            liczba = stale[i]
            if((-liczba) in stale):
                if(indeksJedynki+1<len(stale)):
                    instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")])]    # w przeciwnym razie, ostatnią instrukcją było przypisanie zera
                instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(-liczba))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba))])]
            else:
                instrukcje += ["SET "+str(liczba), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba))])]
            dodawaneStale[i][1]=True

            czyCosZmieniono: bool = True
            while(czyCosZmieniono):
                czyCosZmieniono=False
                czyMoznaDodacKtorasMniejsza: bool = False
                coMoznaDodac: int = 0
                for j in range(indeksJedynki-2,i,-1):
                    if(liczba+stale[j] in stale and not dodawaneStale[stale.index(liczba+stale[j])][1]):
                        if(not czyMoznaDodacKtorasMniejsza):
                            czyMoznaDodacKtorasMniejsza=True
                            coMoznaDodac=stale[j]

                if(not czyMoznaDodacKtorasMniejsza):
                    for j in range(indeksJedynki,len(stale)):
                        if(abs(liczba)>stale[j] and liczba+stale[j] in stale and not dodawaneStale[stale.index(liczba+stale[j])][1]):
                            if(not czyMoznaDodacKtorasMniejsza):
                                czyMoznaDodacKtorasMniejsza=True
                                coMoznaDodac=stale[j]

                czyMoznaOdjacKtorasMniejsza: bool = False
                coMoznaOdjac: int = 0
                for j in range(indeksJedynki-2,i,-1):
                    if(liczba-stale[j] in stale and not dodawaneStale[stale.index(liczba-stale[j])][1]):
                        if(not czyMoznaOdjacKtorasMniejsza):
                            czyMoznaOdjacKtorasMniejsza=True
                            coMoznaOdjac=stale[j]

                if(not czyMoznaOdjacKtorasMniejsza):
                    for j in range(indeksJedynki,len(stale)):
                        if(liczba-stale[j] in stale and not dodawaneStale[stale.index(liczba-stale[j])][1]):
                            if(not czyMoznaOdjacKtorasMniejsza):
                                czyMoznaOdjacKtorasMniejsza=True
                                coMoznaOdjac=stale[j]


                if(czyMoznaDodacKtorasMniejsza and coMoznaDodac<0):
                    ind = stale.index(liczba+coMoznaDodac)
                    instrukcje += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(coMoznaDodac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba+coMoznaDodac))])]
                    dodawaneStale[ind][1]=True
                    liczba += coMoznaDodac
                elif(czyMoznaOdjacKtorasMniejsza and coMoznaOdjac>0):
                    ind = stale.index(liczba-coMoznaOdjac)
                    instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(coMoznaOdjac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba-coMoznaOdjac))])]
                    dodawaneStale[ind][1]=True
                    liczba -= coMoznaOdjac
                elif(liczba*2 in stale and not dodawaneStale[stale.index(liczba*2)][1]):
                    ind = stale.index(liczba*2)
                    instrukcje += ["ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba*2))])]
                    dodawaneStale[ind][1]=True
                    liczba *= 2
                    czyCosZmieniono=True
                elif(liczba//2 in stale and not dodawaneStale[stale.index(liczba//2)][1]):
                    ind = stale.index(liczba//2)
                    instrukcje += ["HALF", "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba//2))])]
                    dodawaneStale[ind][1]=True
                    liczba = liczba//2
                    czyCosZmieniono=True
                elif(czyMoznaDodacKtorasMniejsza and coMoznaDodac>0):
                    ind = stale.index(liczba+coMoznaDodac)
                    instrukcje += ["ADD "+str(adresy[pamiec.index("STAŁA "+str(coMoznaDodac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba+coMoznaDodac))])]
                    dodawaneStale[ind][1]=True
                    liczba += coMoznaDodac
                elif(czyMoznaOdjacKtorasMniejsza and coMoznaOdjac<0):
                    ind = stale.index(liczba-coMoznaOdjac)
                    instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA "+str(coMoznaOdjac))]), "STORE "+str(adresy[pamiec.index("STAŁA "+str(liczba-coMoznaOdjac))])]
                    dodawaneStale[ind][1]=True
                    liczba -= coMoznaOdjac
                elif((liczba+1)*2 in stale and not dodawaneStale[stale.index((liczba+1)*2)][1]):
                    ind = stale.index((liczba+1)*2)
                    instrukcje += ["ADD "+str(adresy[pamiec.index("STAŁA 1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba+1)*2))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba+1)*2
                    czyCosZmieniono=True
                elif((liczba*2)+1 in stale and not dodawaneStale[stale.index((liczba*2)+1)][1]):
                    ind = stale.index((liczba*2)+1)
                    instrukcje += ["ADD 0", "ADD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba*2)+1))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba*2)+1
                    czyCosZmieniono=True
                elif((liczba*2)-1 in stale and not dodawaneStale[stale.index((liczba*2)-1)][1]):
                    ind = stale.index((liczba*2)-1)
                    instrukcje += ["ADD 0", "SUB "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba*2)-1))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba*2)-1
                    czyCosZmieniono=True
                elif((liczba-1)*2 in stale and not dodawaneStale[stale.index((liczba-1)*2)][1]):
                    ind = stale.index((liczba-1)*2)
                    instrukcje += ["SUB "+str(adresy[pamiec.index("STAŁA 1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("STAŁA "+str((liczba-1)*2))])]
                    dodawaneStale[ind][1]=True
                    liczba = (liczba-1)*2
                    czyCosZmieniono=True



    for wsk in wskaznikiTablicowe:
        adr: int = wsk[0]
        wart: int = wsk[1]

        if(wart in stale):
            instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(wart))]), "STORE "+str(adr)]
        elif(2*wart in stale):
            instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(2*wart))]), "HALF", "STORE "+str(adr)]
        elif(((2*wart+1) in stale) and (2*wart+1)//2==wart):
            instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA "+str(2*wart+1))]), "HALF", "STORE "+str(adr)]
        elif((-wart) in stale):
            instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("STAŁA "+str(-wart))]), "STORE "+str(adr)]
        else:
            instrukcje += ["SET "+str(wart), "STORE "+str(adr)]

    instrukcje.append("JUMP PROGRAM")

    if(czyMnozenie):
        instrukcje += ["label: MNOZENIE","LOAD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.KONTR")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE1")]), "JPOS 7", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("MNOZENIE1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE1")]), "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("MNOZENIE.KONTR")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.KONTR")])]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE2")]), "JPOS 7", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("MNOZENIE.KONTR")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.KONTR")]), "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("MNOZENIE2")]), "STORE "+str(adresy[pamiec.index("MNOZENIE2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE1")]), "JPOS 2", "JUMP 7", "LOAD "+str(adresy[pamiec.index("MNOZENIE1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")])]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE2")]), "STORE "+str(adresy[pamiec.index("MNOZENIE1")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE2")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "LOAD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE2")]), "JNEG 2", "JUMP 8"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "ADD 0", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "JUMP -9", "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE2")]), "JPOS 2", "JUMP 7", "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "HALF", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")])]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "HALF", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "STORE "+str(adresy[pamiec.index("MNOZENIE2")]), "JPOS 2", "JUMP 19", "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE2")]), "JPOS 2", "JUMP 8"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "HALF", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "HALF", "STORE "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "JUMP -9", "LOAD "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]), "ADD "+str(adresy[pamiec.index("MNOZENIE.TEMP_1")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("MNOZENIE2")]), "SUB "+str(adresy[pamiec.index("MNOZENIE.TEMP_2")]), "STORE "+str(adresy[pamiec.index("MNOZENIE2")]), "JUMP -19"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("MNOZENIE.KONTR")]), "JPOS 4", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]), "STORE "+str(adresy[pamiec.index("MNOZENIE.WYNIK")]), "RTRN "+str(adresy[pamiec.index("MNOZENIE.RETURN")])]

    if(czyDzielenieLubModulo):
        instrukcje += ["label: DZIELENIE","LOAD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.KONTR1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.KONTR2")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE1")]), "JNEG 2", "JUMP 5", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.KONTR1")]), "SUB "+str(adresy[pamiec.index("DZIELENIE1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE2")]), "JNEG 2", "JUMP 5"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.KONTR2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE2")]), "STORE "+str(adresy[pamiec.index("DZIELENIE2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE1")]), "JPOS 58", "LOAD "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE2")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE1")]), "JNEG 2", "JUMP 8"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "ADD 0", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "ADD 0", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "JUMP -9", "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE1")]), "JPOS 2", "JUMP 7", "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "HALF", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP2")])]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "HALF", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE1")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "STORE "+str(adresy[pamiec.index("DZIELENIE1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "JUMP 2", "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "JPOS 2", "JUMP 20", "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE1")]), "JPOS 2", "JUMP 9"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "JZERO 3", "HALF", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "HALF", "STORE "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "JUMP -10", "LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "ADD "+str(adresy[pamiec.index("DZIELENIE.TEMP1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE1")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "STORE "+str(adresy[pamiec.index("DZIELENIE1")]), "JUMP -21"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("DZIELENIE1")]), "ADD "+str(adresy[pamiec.index("DZIELENIE.TEMP2")]), "STORE "+str(adresy[pamiec.index("DZIELENIE1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "JUMP 5", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.KONTR1")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.KONTR2")]), "JZERO 12"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "JZERO 7", "LOAD "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "SUB "+str(adresy[pamiec.index("STAŁA 1")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.WYNIK")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE2")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "LOAD "+str(adresy[pamiec.index("DZIELENIE.KONTR2")]), "JPOS 6"]
        instrukcje += ["LOAD "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "JZERO 4", "LOAD "+str(adresy[pamiec.index("STAŁA 0")]), "SUB "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "STORE "+str(adresy[pamiec.index("DZIELENIE.MODULO")]), "RTRN "+str(adresy[pamiec.index("DZIELENIE.RETURN")])]

    return instrukcje



def kompilacja2(listaInstrukcji: typing.List[str], czyOstateczne: bool) -> typing.List[str]:
    nowaListaInstrukcji: typing.List[str] = []
    labele: typing.List[typing.Tuple[str,int]] = []
    indeksLabelaProgram: int = -1

    poz: int = 0

    for i in range (len(listaInstrukcji)):
        if("label:" in listaInstrukcji[i]):
            labele.append([listaInstrukcji[i][7:],poz])
            if(listaInstrukcji[i]=="label: PROGRAM"):
                if(not czyOstateczne):
                    indeksLabelaProgram = len(nowaListaInstrukcji) # na tym indeksie by był i będzie z powrotem wstawiony
        else:
            nowaListaInstrukcji.append(listaInstrukcji[i])
            poz += 1

    for lbl in labele:
        for i in range(len(nowaListaInstrukcji)):
            if(nowaListaInstrukcji[i]=="JUMP "+lbl[0]):
                nowaListaInstrukcji[i]="JUMP "+str(lbl[1]-i)
            elif(nowaListaInstrukcji[i]=="JZERO "+lbl[0]):
                nowaListaInstrukcji[i]="JZERO "+str(lbl[1]-i)
            elif(nowaListaInstrukcji[i]=="JPOS "+lbl[0]):
                nowaListaInstrukcji[i]="JPOS "+str(lbl[1]-i)
            elif(nowaListaInstrukcji[i]=="JNEG "+lbl[0]):
                nowaListaInstrukcji[i]="JNEG "+str(lbl[1]-i)
            elif(nowaListaInstrukcji[i]=="SET "+lbl[0]):
                nowaListaInstrukcji[i]="SET "+str(lbl[1])

    if(not czyOstateczne):
        nowaListaInstrukcji.insert(indeksLabelaProgram,"label: PROGRAM")

    return nowaListaInstrukcji


def dodajStale(pamiec: typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]], instrukcje: typing.List[str]) -> typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]]:
    global listaStalych

    for instr in instrukcje:
        if("SET " in instr):
            liczba = int(instr[4:])
            if(liczba not in listaStalych):
                listaStalych.append(liczba)
                pamiec[0].append("STAŁA "+str(liczba))
                pamiec[1].append(pamiec[1][-1]+1) # na pewno nie tablica, bo ostatnie są zawsze stałe (a zawsze są minimum stałe 0 i 1)

    return pamiec



def usunSET(listaInstrukcji: typing.List[str], strukturaPamieci: typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]]) -> typing.List[str]:
    pamiec = strukturaPamieci[0]
    adresy = strukturaPamieci[1]

    for i in range(len(listaInstrukcji)):
        if("SET " in listaInstrukcji[i]):
            liczba = listaInstrukcji[i][4:]
            listaInstrukcji[i] = "LOAD "+str(adresy[pamiec.index("STAŁA "+liczba)])

    return listaInstrukcji






####################### PARSER #######################




def p_plik(p):
    'plik : procedury main'


def p_procedury_koniec(p):
    'procedury :'


def p_procedury_bezZmiennych(p):
    'procedury : procedury PROCEDURA naglowekProcedury IS BEGIN komendy END'

    ogolnyBlokOperacyjny(p)


def p_procedury_zeZmiennymi(p):
    'procedury : procedury PROCEDURA naglowekProcedury IS deklaracje BEGIN komendy END'

    ogolnyBlokOperacyjny(p)


def p_main_bezZmiennych(p):
    'main : MAIN IS BEGIN komendy END'

    global nazwaF
    nazwaF = "PROGRAM"
    ogolnyBlokOperacyjny(p)


def p_main_zeZmiennymi(p):
    'main : MAIN IS deklaracje BEGIN komendy END'

    global nazwaF
    nazwaF = "PROGRAM"
    ogolnyBlokOperacyjny(p)


def p_komendy_1(p):
    'komendy : komenda'


def p_komendy_2(p):
    'komendy : komendy komenda'


def p_komenda_przypis(p):
    'komenda : identyfikator PRZYPISANIE wyrazenie SREDNIK'

    global tempOperacji, listaStalych
    tempOperacji.append([":=",p[1],p[3],p.lineno(4),p.lexpos(4)])   # gdyby wziąć lexpos(2), to operacja x:=x+y dla niezadeklarownego wcześniej x nie zwracałaby błędu
    if(type(p[3])==int):
        listaStalych.append(p[3])


def p_komenda_ifKrotki(p):
    'komenda : IF warunek THEN komendy ENDIF'

    global zakresyBlokow, brakOstrzezen
    if(type(p[2])==bool):
        brakOstrzezen = False
        if(p[2]):
                        # nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane
            print("Ostrzeżenie - warunek liczbowy w if w linii "+str(p.lineno(1))+". jest zawsze spełniony!\n")
            zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"always","if",p.lexpos(1)])

        else:
                        # blok wewnętrzny nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - warunek liczbowy w if w linii "+str(p.lineno(1))+". jest zawsze fałszywy!\n")
            zakresyBlokow.append([False,p.lexpos(3),p.lexpos(5),"if",p.lexpos(1)])

    else:
        zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"if",p.lexpos(1)])


def p_komenda_ifDlugi(p):
    'komenda : IF warunek THEN komendy ELSE komendy ENDIF'

    global zakresyBlokow, brakOstrzezen
    if(type(p[2])==bool):
        brakOstrzezen = False
        if(p[2]):
                        # blok wewnętrzny if nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane; blok wewnętrzny else nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - warunek liczbowy w if w linii "+str(p.lineno(1))+". jest zawsze spełniony!\n")
            zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"always","if",p.lexpos(1)])
            zakresyBlokow.append([False,p.lexpos(5),p.lexpos(7),"else",p.lexpos(1)])

        else:
                        # blok wewnętrzny else nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane; blok wewnętrzny if nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - warunek liczbowy w if w linii "+str(p.lineno(1))+". jest zawsze fałszywy!\n")
            zakresyBlokow.append([False,p.lexpos(3),p.lexpos(5),"if",p.lexpos(1)])
            zakresyBlokow.append([True,p.lexpos(5),p.lexpos(7),"always","else",p.lexpos(1)])

    else:
        zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"if",p.lexpos(1)])
        zakresyBlokow.append([True,p.lexpos(5),p.lexpos(7),"else",p.lexpos(1)])


def p_komenda_forWDol(p):
    'komenda : FOR IDENTYFIKATOR FROM wartosc DOWNTO wartosc DO komendy ENDFOR'

    global tempIteratorow, zakresyBlokow, tempOperacji, brakOstrzezen, listaStalych

    tempIteratorow.append([p[2],p.lexpos(7),p.lexpos(9),p.lineno(7)])

    if(type(p[4])==int and type(p[6])==int):
        if(p[4]<p[6]):     # w przeciwnym razie nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane

                        # blok wewnętrzny nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - w pętli for w linii "+str(p.lineno(1))+". podano zakres FROM "+str(p[4])+" DOWNTO "+str(p[6])+"!\n")
            zakresyBlokow.append([False,p.lexpos(7),p.lexpos(9),"for"])
            brakOstrzezen = False
        else:
            zakresyBlokow.append([True,p.lexpos(7),p.lexpos(9),"always","for downto",p[2],p.lexpos(3)])
            tempOperacji.append(["zakres fora downto",p[4],p[6],p.lineno(5),p.lexpos(5)])
            listaStalych.extend([p[4],p[6]])

    else:
        zakresyBlokow.append([True,p.lexpos(7),p.lexpos(9),"for downto",p[2],p.lexpos(3)])
        tempOperacji.append(["zakres fora downto",p[4],p[6],p.lineno(5),p.lexpos(5)])
        if(type(p[4])==int):
            listaStalych.append(p[4])
        if(type(p[6])==int):
            listaStalych.append(p[6])


def p_komenda_forWGore(p):
    'komenda : FOR IDENTYFIKATOR FROM wartosc TO wartosc DO komendy ENDFOR'

    global tempIteratorow, zakresyBlokow, tempOperacji, brakOstrzezen, listaStalych

    tempIteratorow.append([p[2],p.lexpos(7),p.lexpos(9),p.lineno(7)])

    if(type(p[4])==int and type(p[6])==int):
        if(p[4]>p[6]):     # w przeciwnym razie nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane

                        # blok wewnętrzny nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - w pętli for w linii "+str(p.lineno(1))+". podano zakres FROM "+str(p[4])+" TO "+str(p[6])+"!\n")
            zakresyBlokow.append([False,p.lexpos(7),p.lexpos(9),"for"])
            brakOstrzezen = False
        else:
            zakresyBlokow.append([True,p.lexpos(7),p.lexpos(9),"always", "for to",p[2],p.lexpos(3)])
            tempOperacji.append(["zakres fora to",p[4],p[6],p.lineno(5),p.lexpos(5)])
            listaStalych.extend([p[4],p[6]])

    else:
        zakresyBlokow.append([True,p.lexpos(7),p.lexpos(9),"for to",p[2],p.lexpos(3)])
        tempOperacji.append(["zakres fora to",p[4],p[6],p.lineno(5),p.lexpos(5)])
        if(type(p[4])==int):
            listaStalych.append(p[4])
        if(type(p[6])==int):
            listaStalych.append(p[6])


def p_komenda_while(p):
    'komenda : WHILE warunek DO komendy ENDWHILE'

    global zakresyBlokow, brakOstrzezen
    if(type(p[2])==bool):
        brakOstrzezen = False
        if(p[2]):
                        # nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane
            print("Ostrzeżenie - warunek liczbowy w pętli while w linii "+str(p.lineno(1))+". jest zawsze spełniony!\n")
            zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"always", "while",p.lexpos(1)])

        else:
                        # blok wewnętrzny nie zostanie wykonany - operacje z wnętrza ignorowane
            print("Ostrzeżenie - warunek liczbowy w pętli while w linii "+str(p.lineno(1))+". jest zawsze fałszywy!\n")
            zakresyBlokow.append([False,p.lexpos(3),p.lexpos(5),"while"])

    else:
        zakresyBlokow.append([True,p.lexpos(3),p.lexpos(5),"while",p.lexpos(1)])


def p_komenda_repUntil(p):
    'komenda : REPEAT komendy UNTIL warunek SREDNIK'

    global brakOstrzezen, zakresyBlokow
                                                        # nie ma wpływu na testy zadeklarowania zmiennych przed użyciem, ponieważ wnętrze na pewno zostanie wykonane minimum raz
    if(type(p[4])==bool):
        if(p[4]):
            print("Ostrzeżenie - warunek liczbowy w pętli repeat-until w linii "+str(p.lineno(3))+". jest zawsze spełniony!\n")
            brakOstrzezen = False
            zakresyBlokow.append([True,p.lexpos(1),p.lexpos(3),"never", "repeat-until",p.lexpos(5)])
        else:
            print("Ostrzeżenie - warunek liczbowy w pętli repeat-until w linii "+str(p.lineno(3))+". jest zawsze fałszywy!\n")
            brakOstrzezen = False
            zakresyBlokow.append([True,p.lexpos(1),p.lexpos(3),"always", "repeat-until",p.lexpos(5)])

    else:
        zakresyBlokow.append([True,p.lexpos(1),p.lexpos(3),"repeat-until",p.lexpos(5)])


def p_komenda_wywolanieProcedury(p):
    'komenda : wywolanieProcedury SREDNIK'


def p_komenda_read(p):
    'komenda : READ identyfikator SREDNIK'

    global tempOperacji
    tempOperacji.append(["read",p[2],p.lineno(3),p.lexpos(3)])


def p_komenda_write(p):
    'komenda : WRITE wartosc SREDNIK'

    global tempOperacji, listaStalych
    tempOperacji.append(["write",p[2],p.lineno(1),p.lexpos(1)])
    if(type(p[2])==int):
        listaStalych.append(p[2])


def p_naglowekProcedury(p):
    'naglowekProcedury : IDENTYFIKATOR LEWYNAWIAS deklaracjeParametrow PRAWYNAWIAS'

    global nazwaF
    nazwaF = p[1]


def p_wywolanieProcedury(p):
    'wywolanieProcedury : IDENTYFIKATOR LEWYNAWIAS argumenty PRAWYNAWIAS'

    global tempArgumentow, tempWywolanProcedur, tempOperacji

    czyZadeklarowana: bool = testIstnieniaProcedury(p[1],p.lineno(1))
    if(czyZadeklarowana):
        wywolanie = [p[1],[],p.lexpos(1)]
        operacjaWywolania = ["wywołanie",p[1]]

        for arg in tempArgumentow:
            wywolanie[1].append(arg)
            operacjaWywolania.append(arg)

        tempWywolanProcedur.append(wywolanie)
        tempOperacji.append(operacjaWywolania+[p.lineno(4),p.lexpos(4)])

    tempArgumentow = []


def p_deklaracje_1(p):
    'deklaracje : IDENTYFIKATOR LEWYNAWIASKWADRATOWY liczba DWUKROPEK liczba PRAWYNAWIASKWADRATOWY'

    global tempZmiennych
    tempZmiennych.append([p[1],"tablica",p[3],p[5]])


def p_deklaracje_2(p):
    'deklaracje : IDENTYFIKATOR'

    global tempZmiennych
    tempZmiennych.append([p[1],"liczba"])


def p_deklaracje_3(p):
    'deklaracje : deklaracje PRZECINEK IDENTYFIKATOR LEWYNAWIASKWADRATOWY liczba DWUKROPEK liczba PRAWYNAWIASKWADRATOWY'

    global tempZmiennych
    tempZmiennych.append([p[3],"tablica",p[5],p[7]])


def p_deklaracje_4(p):
    'deklaracje : deklaracje PRZECINEK IDENTYFIKATOR'

    global tempZmiennych
    tempZmiennych.append([p[3],"liczba"])


def p_deklaracjeParametrow_1(p):
    'deklaracjeParametrow : TABLICA IDENTYFIKATOR'

    global tempParametrow
    tempParametrow.append([p[2],"tablica"])


def p_deklaracjeParametrow_2(p):
    'deklaracjeParametrow : IDENTYFIKATOR'

    global tempParametrow
    tempParametrow.append([p[1],"liczba"])


def p_deklaracjeParametrow_3(p):
    'deklaracjeParametrow : deklaracjeParametrow PRZECINEK TABLICA IDENTYFIKATOR'

    global tempParametrow
    tempParametrow.append([p[4],"tablica"])

def p_deklaracjeParametrow_4(p):
    'deklaracjeParametrow : deklaracjeParametrow PRZECINEK IDENTYFIKATOR'

    global tempParametrow
    tempParametrow.append([p[3],"liczba"])


def p_argumenty_1(p):
    'argumenty : argumenty PRZECINEK IDENTYFIKATOR'

    global tempArgumentow
    tempArgumentow.append(p[3])


def p_argumenty_2(p):
    'argumenty : IDENTYFIKATOR'

    global tempArgumentow
    tempArgumentow.append(p[1])


def p_wyrazenie_zwykle(p):
    'wyrazenie : wartosc'

    p[0] = p[1]


def p_wyrazenie_minus(p):
    'wyrazenie : wartosc MINUS wartosc'

    global tempOperacji, listaStalych
    if((type(p[1])==int and type(p[3])==int) and p[1]-p[3]<=9223372036854775807 and p[1]-p[3]>=-9223372036854775808):
        p[0] = p[1]-p[3]
    else:
        tempOperacji.append(["-",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "DZIAŁ"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_wyrazenie_plus(p):
    'wyrazenie : wartosc PLUS wartosc'

    global tempOperacji, listaStalych
    if((type(p[1])==int and type(p[3])==int) and p[1]+p[3]<=9223372036854775807 and p[1]+p[3]>=-9223372036854775808):
        p[0] = p[1]+p[3]
    else:
        tempOperacji.append(["+",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "DZIAŁ"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_wyrazenie_modulo(p):
    'wyrazenie : wartosc MODULO wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        if(p[3]!=0):
            p[0] = p[1]%p[3]                        # charakterystyka modulo zgodna z pythonowskim %
        else:
            p[0] = 0
    else:
        tempOperacji.append(["%",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "DZIAŁ"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_wyrazenie_dzielenie(p):
    'wyrazenie : wartosc DZIELENIE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int and not (p[1]==-9223372036854775808 and p[3]==-1)):
        if(p[3]!=0):
            p[0] = p[1]//p[3]                       # charakterystyka dzielenia zgodna z pythonowskim //
        else:
            p[0] = 0
    else:
        tempOperacji.append(["/",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "DZIAŁ"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_wyrazenie_mnozenie(p):
    'wyrazenie : wartosc MNOZENIE wartosc'

    global tempOperacji, listaStalych
    if((type(p[1])==int and type(p[3])==int) and p[1]*p[3]<=9223372036854775807 and p[1]*p[3]>=-9223372036854775808):
        p[0] = p[1]*p[3]
    else:
        tempOperacji.append(["*",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "DZIAŁ"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_mniejszeRowne(p):
    'warunek : wartosc CZYMNIEJSZEROWNE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]<=p[3]
    else:
        tempOperacji.append(["<=",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_wiekszeRowne(p):
    'warunek : wartosc CZYWIEKSZEROWNE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]>=p[3]
    else:
        tempOperacji.append([">=",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_mniejsze(p):
    'warunek : wartosc CZYMNIEJSZE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]<p[3]
    else:
        tempOperacji.append(["<",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_wieksze(p):
    'warunek : wartosc CZYWIEKSZE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]>p[3]
    else:
        tempOperacji.append([">",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_rozne(p):
    'warunek : wartosc CZYROZNE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]!=p[3]
    else:
        tempOperacji.append(["!=",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_warunek_rowne(p):
    'warunek : wartosc CZYROWNE wartosc'

    global tempOperacji, listaStalych
    if(type(p[1])==int and type(p[3])==int):
        p[0] = p[1]==p[3]
    else:
        tempOperacji.append(["==",p[1],p[3],p.lineno(2),p.lexpos(2)])
        p[0] = "WAR"
        if(type(p[1])==int):
            listaStalych.append(p[1])
        if(type(p[3])==int):
            listaStalych.append(p[3])


def p_wartosc_liczba(p):
    'wartosc : liczba'

    p[0] = p[1]


def p_wartosc_identyfikator(p):
    'wartosc : identyfikator'

    p[0] = p[1]


def p_liczba_plus(p):
    'liczba : LICZBA'

    global brakBledow

    p[0] = p[1]

    if(p[1]>9223372036854775807):
        print("Błąd - podano zbyt dużą stałą "+str(p[1])+" w linii "+str(p.lineno(1))+".!\n")
        brakBledow = False


def p_liczba_minus(p):
    'liczba : MINUS LICZBA'

    global brakBledow

    p[0] = -p[2]

    if(p[2]>9223372036854775808):
        print("Błąd - podano zbyt małą stałą -"+str(p[2])+" w linii "+str(p.lineno(2))+".!\n")
        brakBledow = False


def p_identyfikator_zmienna(p):
    'identyfikator : IDENTYFIKATOR'

    p[0] = p[1]


def p_identyfikator_tablica(p):
    'identyfikator : identyfikatorTablicy'

    p[0] = "ELTABLICY"


def p_identyfikatorTablicy_1(p):
    'identyfikatorTablicy : IDENTYFIKATOR LEWYNAWIASKWADRATOWY IDENTYFIKATOR PRAWYNAWIASKWADRATOWY'

    global tempOperacji
    tempOperacji.append(["el. tablicy",p[1],p[3],p.lineno(1),p.lexpos(1)])


def p_identyfikatorTablicy_2(p):
    'identyfikatorTablicy : IDENTYFIKATOR LEWYNAWIASKWADRATOWY liczba PRAWYNAWIASKWADRATOWY'

    global tempOperacji,listaStalych
    tempOperacji.append(["el. tablicy",p[1],p[3],p.lineno(1),p.lexpos(1)])
    listaStalych.append(p[3])


def p_error(p):
    global brakBledow
    brakBledow = False
    try:
        print("Wystąpił błąd składni w linii "+str(p.lineno)+". w pobliżu wyrażenia "+str(p.value)+"!\n")
    except:
        print("Wystąpił krytyczny błąd składni!\n")


#stworzenie obiektu parsera
parser = ply.yacc.yacc()

#Uruchomienie sprawdzenia poprawności i kompilacja

if(len(sys.argv)==1):
    print("Nie podano nazwy pliku źródłowego i wynikowego!\n\n")
elif(len(sys.argv)==2):
    print("Nie podano nazwy pliku wynikowego!\n\n")
elif(len(sys.argv)>3):
    print("Podano zbyt wiele argumentów!\n\n")
elif(len(sys.argv[1])<4 or sys.argv[1][-4:]!=".imp"):
    print("Podany plik nie jest plikiem źródłowym z rozszerzeniem .imp!\n\n")
else:
    try:
        plik = open(sys.argv[1],'r')
        tresc = plik.read()

        #sprawdzenie składni
        try:
            parser.parse(tresc,lexer=lekser)
        except NameError:
            brakBledow=False

        if(not brakBledow):
            print("Nie udało się skompilować programu.\n\n\n")
        else:
            if(not brakOstrzezen):
                print("Podczas sprawdzania kodu wystąpiły potencjalnie niebezpieczne sytuacje. Przed uruchomieniem programu należy sprawdzić treść ostrzeżeń i upewnić się, że wykryte sytuacje nie uniemożliwiają poprawnego działania programu.\n\n\n")

            print("Kompilowanie programu...\n")
            usunNieuzywaneProcedury()

            #przydzielenie pamięci
            pamiec: typing.Tuple[typing.List[str], typing.List[int], typing.List[typing.Tuple[int,int]]] = przydzielPamiec(deklaracjeProcedur)
            if(brakBledow):

                #Kompilacja
                instr = ["JUMP BLOKKONCOWY"]
                for lista in listaOperacji:
                    instr.extend(kompilacja1(lista,pamiec))

                instr = kompilacja2(instr,False)
                pamiec = dodajStale(pamiec,instr)
                instr  = usunSET(instr, pamiec)

                instr.extend(fragmentKoncowy(pamiec,listaStalych))
                instr = kompilacja2(instr,True)

                plikWynikowy = open(sys.argv[2]+".mr",'w')

                for instrukcja in instr:
                    plikWynikowy.write(instrukcja+"\n")
                plikWynikowy.close()

                print("Zakończono.\n\n")

            else:
                print("Nie udało się skompilować programu.\n\n\n")

    except NameError:
        print("Podany plik źródłowy nie istnieje!\n\n")
