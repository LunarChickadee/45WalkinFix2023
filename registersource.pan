___ PROCEDURE .Initialize ______________________________________________________
permanent lasttime, lasttime2
global creditex, walkin, names, list,num, mtenter, fbenter, sdsenter,ordered,taxable,enter,
price,quant,tot,item,numb,display,mtordered, fbordered, sdsordered, usedisc,
mtnumb, mtdisplay, fbnumb, fbdisplay, sdsnumb, sdsdisplay, size, Moosetotal, OGStotal, Seedtotal, Treetotal, Seedlingtotal, shippin, shipto, waswindow, Bulbstotal,
ogsitem, mtitem, fbitem, sdsitem, itemlength, entryitem, mtentryitem, fbentryitem, sdsentryitem, recordsize, id_number, mtliveQuery, fbliveQuery, sdsliveQuery, getseedscat, gettreescat, getbulbscat,
walkinname,itemtrans,newitem, catheader
expressionstacksize 75000000
fileglobal liveQuery, queryResults, mad, cit, st, arbico, arbicoinv, invno, mtliveQuery, fbliveQuery, sdsliveQuery, mtqueryResults, fbqueryResults, sdsqueryResults, findinwalkin, findinmailinglist, walkingroup, walkinname, chargeit, giftamount, giftused, orderdisplay
getseedscat=1
gettreescat=1
getbulbscat=1
mtliveQuery=""
fbliveQuery=""
sdsliveQuery=""
liveQuery=""
ogsitem=""
mtenter=""
mtitem=""
fbenter=""
fbitem=""
sdsenter=""
sdsitem=""
enter=""
creditex=""
display=""
fbdisplay=""
sdsdisplay=""
mtdisplay=""
entryitem=""
fbentryitem=""
sdsentryitem=""
mtentryitem=""
itemlength=0
Moosetotal=0
Bulbstotal=0
Seedtotal=0
shippin=0
shipto=""
mad=""
cit=""
st=""
arbico=""
arbicoinv=""
invno=""
walkinname=""
itemtrans=0
chargeit=0
orderdisplay=""


walkin=info("windowname")
openfile "45ogscomments.linked"
arraybuild catheader,",", "45ogscomments.linked", headers
arraydeduplicate catheader, catheader, ","
arraysort catheader, catheader, ","

window walkin

;; Sarah, Stasha like this prompt, but no one else does, so leave it commented out by default in the master copy
if folderpath(dbinfo("folder","")) CONTAINS "sarah" or folderpath(dbinfo("folder","")) CONTAINS "stasha"
    yesno "Keep going?"
    If clipboard() contains "no"
        stop
    endif
endif
    
openfile "45WalkInReconciliation"
openfile "45 mailing list"
//openfile "45ogscomments.warehouse"
openfile "discounttable"
openfile "customer_history"
recordsize=info("records")
;;openfile "45mt prices"
;;select priceline notcontains "no"
;openfile "45MooseOrderingNifelheim"
openfile "45bulbs lookup"
openfile "45seeds prices"
window walkin
forcesynchronize
field Transaction
sortup
lastrecord
goform "sales"
superobject "Categories", "open", "FillList", "Close"
drawobjects
message "Ready"
;superobject "ogsinput", "Open"
___ ENDPROCEDURE .Initialize ___________________________________________________

___ PROCEDURE .NewRecord _______________________________________________________
if Paid=0 and Total>0 and PurchaseOrder="" and TransactionType notcontains "donation" and TransactionType notcontains "transfer" and TransactionType notcontains "owes"
message "Please complete this order"
stop
endif
if
Status="Com"
InsertBelow
else
call .finished
insertbelow
endif
___ ENDPROCEDURE .NewRecord ____________________________________________________

___ PROCEDURE .addtomailinglist ________________________________________________
openfile "45 mailing list"
;; first search really thoroughly (by address, by email)

goform "Add Walkin Customer"
insertrecord
inqcode=str(yearvalue(today()))[3,4]+"wi"
S=1
T=1
Bf=1
___ ENDPROCEDURE .addtomailinglist _____________________________________________

___ PROCEDURE .addtoordernew ___________________________________________________
If Status contains "com"
stop
endif
local qty, newitem
qty=""
gettext "How many?", qty
if extract(querySelection,¬,1) contains "-"
    newitem=extract(querySelection,"-",1)+ "+" + extract(extract(querySelection,¬,1),"-",2) + "+" + qty
else
    newitem=extract(querySelection,¬,1)+"+"+qty
endif

enter=enter+newitem+¶
qty=""
superobject "OGSEnter", "Open" 
activesuperobject "SetText", enter
activesuperobject "close"
;;superobject "ogsinput", "open"
;;ogsitem=""
ShowPage
stop
___ ENDPROCEDURE .addtoordernew ________________________________________________

___ PROCEDURE .arbico __________________________________________________________
;;debug

;;message arbicoinv
;;Notes="Arbico " + str(arbicoinv) + Notes

arrayfilter Order, arbico,¶, extract(extract(Order,¶,seq()),¬,1)+¬+extract(extract(Order,¶,seq()),¬,2)
+¬+extract(extract(Order,¶,seq()),¬,5)+" "+rep(chr(95),4)
+¬+extract(extract(Order,¶,seq()),¬,3)

printusingform "","arbicopacking"
printonerecord dialog
goform "arbicoinvoice"

;;pdfsetup "Arbico Invoice "+str(arbicoinv)+".pdf"

printonerecord ""
arbico=""

goform "sales"
___ ENDPROCEDURE .arbico _______________________________________________________

___ PROCEDURE .customerowes ____________________________________________________
local deadbeat
deadbeat=""
if info("trigger")="Button.Customer Owes"
TransactionType="Owes"
if Name=""
getscrap "Who owes?"
Name=clipboard()
deadbeat=Name+ " owes"
Notes=?(Notes="",deadbeat,Notes+¶+deadbeat)
else
deadbeat=Name+ " owes"
Notes=?(Notes="",deadbeat,Notes+¶+deadbeat)
endif
endif

___ ENDPROCEDURE .customerowes _________________________________________________

___ PROCEDURE .donation ________________________________________________________

TransactionType="Donation"
if
Group=""
getscrap "Donation to:"
Notes=?(Notes="","Donation to "+clipboard(),Notes+¶+"Donation to"+clipboard())
else
Notes=?(Notes="", "Donation to "+Group,Notes+¶+"Donation to "+Group)
endif

___ ENDPROCEDURE .donation _____________________________________________________

___ PROCEDURE .enterarbicocustomer _____________________________________________
fileglobal arbicoinv
arbicoinv=""

TaxExempt="Y"
Special="Y"
resale="arbico"
Group="Arbico"
call .entry

    local newWindowRect
    newWindowRect=rectanglecenter(
    info("screenrectangle"),
    rectanglesize(1,1,7*72,8*72))
    setwindowrectangle newWindowRect,
    "noHorzScroll noVertScroll noPallette"
    openform "arbicocustomer"

___ ENDPROCEDURE .enterarbicocustomer __________________________________________

___ PROCEDURE .entry ___________________________________________________________
waswindow=info("windowname")

if Date≠today()
    stop
endif
If Status="Com"
    stop
endif

sobulky:
Moosetotal=0
TaxTotal=0
Subtotal=0
OGStotal=0
Seedtotal=0
Treetotal=0
Bulbstotal=0
Seedlingtotal=0
«$Shipping»=0
numb=1
display=""
mtnumb=""
sdsnumb=""
fbnumb=""

loop
    stoploopif enter=""
    item=val(striptonum(extract(extract(enter,¶,numb),chr(43),1)))
    if item=4000
        quant=val(extract(extract(enter,¶,numb),chr(43),2))
        quant=?(quant=0,1, quant)
        id_number=40000
        price=val(extract(extract(enter,¶,numb),chr(43),3))
        tot=quant*price
        Treetotal=Treetotal+tot
        ordered=str(id_number) +¬+str(item)+¬+
            rep(chr(32),15)+"Trees"+¬+
            rep(chr(32),3)+"0#"+¬+
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶
            display=display+ordered        
    endif
    if item=5000
        quant=val(extract(extract(enter,¶,numb),chr(43),2))
        quant=?(quant=0,1, quant)
        id_number=50000
        price=val(extract(extract(enter,¶,numb),chr(43),3))
        tot=quant*price
        Seedlingtotal=Seedlingtotal+tot
        ordered=str(id_number) +¬+str(item)+¬+
            rep(chr(32),11)+"Seedlings"+¬+
            rep(chr(32),3)+"0#"+¬+
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶
            display=display+ordered    
    endif
    if item≥8000
        size=extract(extract(enter,¶,numb),chr(43),2)
        size=?(size="", "a",size)
        quant=val(extract(extract(enter,¶,numb),chr(43),3))
        quant=?(quant=0,1, quant)
        price=val(extract(extract(enter,¶,numb),chr(43),4))
        if price=0
            if item≥10000 and item≤30000   ;; look up in the warehouse file
                case Staff="Y"
                    price=lookup("45ogscomments.warehouse","Item",str(item) + "-" + upper(size),"Staff",0,0)
                case Special="Y"
                    price=lookup("45ogscomments.warehouse","Item",str(item) + "-" + upper(size),"NOFA",0,0)
                case Transfer="Y"
                    price=lookup("45ogscomments.warehouse","Item",str(item) + "-" + upper(size),"base",0,0)    
                defaultcase
                    price=lookup("45ogscomments.warehouse","Item",str(item) + "-" + upper(size),"Price",0,0)
                endcase
            else
                case Staff="Y"
                    price=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"Staff",0,0)
                case Special="Y"
                    price=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"NOFA",0,0)
                case Transfer="Y"
                    price=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"base",0,0)
                    
              case SpareText3="Y"
                    price=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"Price",0,0)
                    price=round(price+price*.055,1)
                defaultcase
                    price=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"Price",0,0)
                endcase
            endif
        endif
        if item≥10000 and item≤30000
            id_number=lookup("45ogscomments.warehouse","Item",str(item) + "-" + upper(size),"IDNumber",0,0)
        else
            id_number=lookup("45ogscomments.linked","Item",str(item) + "-" + upper(size),"IDNumber",0,0)
        endif
        
        if item>0 And price=0
            GetScrap "What is the price of "+str(item) + "-" + upper(size)+"?"
            price=val(clipboard())
        endif
        
        tot=quant*price
        
        
        ;; item number
        ordered=str(id_number) +¬+str(item)+"-"+upper(size)+¬+                                                                                     /*item number*/
        ?(item >=10000,""," ")+
         ?(item=8999,
            extract(extract(enter,¶,numb),chr(43),5)[1,19] +
            rep(chr(32),19-length(extract(extract(enter,¶,numb),chr(43),5)[1,19])),
            lookup(?(item≥10000 and item≤30000,"45ogscomments.warehouse","45ogscomments.linked"),"Item",str(item) + "-" + upper(size),"Description","Not OGS",0)[1,23])  +           /*adds description looked up from 45ogscomments.linked*/
           rep(chr(32),23-length(lookup(?(item≥10000 and item≤30000,"45ogscomments.warehouse","45ogscomments.linked"),"Item",str(item) + "-" + upper(size),"Description",
            "Not OGS",0)[1,23]))                                                                                    /*if item number is 8999 or 9999, put some padding and then the manually entered description. Otherwise, add padding. */
            +rep(chr(32),5-length(str(lookup(?(item≥10000 and item≤30000,"45ogscomments.warehouse","45ogscomments.linked"),"Item",str(item) + "-" + upper(size),«Sz.»,
            0,0))))+¬+str(lookup(?(item≥10000 and item≤30000,"45ogscomments.warehouse","45ogscomments.linked"),"Item",str(item) + "-" + upper(size),«Sz.»,
            0,0))+"#"+¬+                                                                                                /*adds spacing for size, and size, from 45ogscomments.linked (manual items get size 0#)*/
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶
        taxable=tot
        taxable=?(TaxExempt="Y",0,taxable)
        display=display+ordered    
        TaxTotal=TaxTotal+taxable
        OGStotal=OGStotal+tot
    endif
    if item≥7000 and item<8000
        item=val(extract(extract(enter,¶,numb),chr(43),1))
        size=extract(extract(enter,¶,numb),chr(43),2)
        size=?(size="", "a",size)
        quant=val(extract(extract(enter,¶,numb),chr(43),3))
        quant=?(quant=0,1, quant)
        id_number=79999
        price=val(extract(extract(enter,¶,numb),chr(43),4))
        mtnumb=val(extract(extract(enter,¶,numb),chr(43),6))
        if price=0
            case size="a" or size="A"
                price=lookup("45mt prices","Item",item,"priceA",0,0)
               mtnumb=lookup("45mt prices","Item",item,"szA",0,0)
            case size="b" or size="B"
                price=lookup("45mt prices","Item",item,"priceB",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szB",0,0)
            case size="c" or size="C"
                price=lookup("45mt prices","Item",item,"priceC",0,0)                
                mtnumb=lookup("45mt prices","Item",item,"szC",0,0)
            case size="d" or size="D"
                price=lookup("45mt prices","Item",item,"priceD",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szD",0,0)
            case size="e" or size="E"
                price=lookup("45mt prices","Item",item,"priceE",0,0)
               mtnumb=lookup("45mt prices","Item",item,"szE",0,0)
               endcase
        endif
        if Special="Y"
            case size="a" or size="A"
                price=lookup("45mt prices","Item",item,"bulkA",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szA",0,0)
            case size="b" or size="B"
                price=lookup("45mt prices","Item",item,"bulkB",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szB",0,0)
            case size="c" or size="C"
                price=lookup("45mt prices","Item",item,"bulkC",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szC",0,0)
            case size="d" or size="D"
                price=lookup("45mt prices","Item",item,"bulkD",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szD",0,0)
            case size="e" or size="E"
                price=lookup("45mt prices","Item",item,"bulkE",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szE",0,0)
            endcase
        endif
        if Staff="Y"
            case size="a" or size="A"
                price=lookup("45mt prices","Item",item,"staffA",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szA",0,0)
            case size="b" or size="B"
                price=lookup("45mt prices","Item",item,"staffB",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szB",0,0)
            case size="c" or size="C"
                price=lookup("45mt prices","Item",item,"staffC",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szC",0,0)
            case size="d" or size="D"
                price=lookup("45mt prices","Item",item,"staffD",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szD",0,0)
            case size="e" or size="E"
                price=lookup("45mt prices","Item",item,"staffE",0,0)
                mtnumb=lookup("45mt prices","Item",item,"szE",0,0)
            endcase
            
            ;;price=1
        endif
        
        if val(item)>0 And price=0
            GetScrap "What is the priceof "+str(item)+"?"
            price=val(clipboard())
        endif
        
        tot=val(quant)*price
        
        if val(item)=7000
            mtnumb=extract(extract(enter,¶,numb),chr(43),6)
            if size=""
                getscrap "what size is 7000?"
                mtnumb=clipboard()
            endif
        endif
        
       ordered=str(id_number) +¬+str(item)+"-"+upper(size)+¬+" "+
            ?(val(item)=7000,
            rep(chr(32),19-length(extract(extract(enter,¶,numb),"+",6)[1,19]))
            +extract(extract(enter,¶,numb),"+",6)[1,19],
            lookup("45mt prices","Item",item,"Variety","Special",0)[1,23])+¬+ rep(chr(32),23-length(lookup("45mt prices","Item",item,"Variety",
            "Special",0)[1,23]))+
            rep(chr(32),5-length(str(mtnumb)))+str(mtnumb)+"#"+¬+
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶

            display=display+ordered    
            Moosetotal=Moosetotal+tot
    endif
    
    
    
    
    
    if item≥6000 and item<7000
        item=val(extract(extract(enter,¶,numb),chr(43),1))
        size=extract(extract(enter,¶,numb),chr(43),2)
        size=?(size="", "a",size)
        quant=val(extract(extract(enter,¶,numb),chr(43),3))
        quant=?(quant=0,1, quant)
        id_number=69999
        price=val(extract(extract(enter,¶,numb),chr(43),4))
        fbnumb=val(extract(extract(enter,¶,numb),chr(43),6))
        if price=0
            case size="a" or size="A"
                price=lookup("45bulbs lookup","number",item,"price A",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size A",0,0)
            case size="b" or size="B"
                price=lookup("45bulbs lookup","number",item,"price B",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size B",0,0)
            case size="c" or size="C"
                price=lookup("45bulbs lookup","number",item,"price C",0,0)                
                fbnumb=lookup("45bulbs lookup","number",item,"size C",0,0)
            case size="d" or size="D"
                price=lookup("45bulbs lookup","number",item,"price D",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size D",0,0)
            endcase
        endif
        if Staff="Y"
            case size="a" or size="A"
                price=lookup("45bulbs lookup","number",item,"staff A",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size A",0,0)
            case size="b" or size="B"
                price=lookup("45bulbs lookup","number",item,"staff B",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size B",0,0)
            case size="c" or size="C"
                price=lookup("45bulbs lookup","number",item,"staff C",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size C",0,0)
            case size="d" or size="D"
                price=lookup("45bulbs lookup","number",item,"staff D",0,0)
                fbnumb=lookup("45bulbs lookup","number",item,"size D",0,0)
            endcase
        endif
        
        if val(item)>0 And price=0
            GetScrap "What is the price of "+str(item)+"?"
            price=val(clipboard())
        endif
        
        tot=val(quant)*price
        
        if val(item)=6000
            fbnumb=extract(extract(enter,¶,numb),chr(43),6)
            if size=""
                getscrap "what size is 6000?"
                fbnumb=clipboard()
            endif
        endif
        
       ordered=str(id_number) +¬+str(item)+"-"+upper(size)+¬+" "+
            ?(val(item)=6000,
            rep(chr(32),19-length(extract(extract(enter,¶,numb),"+",6)[1,19]))
            +extract(extract(enter,¶,numb),"+",6)[1,19],
            lookup("45bulbs lookup","number",item,"name","Special",0)[1,23])+¬+ rep(chr(32),23-length(lookup("45bulbs lookup","number",item,"name",
            "Special",0)[1,23]))+
            rep(chr(32),5-length(str(fbnumb)))+str(fbnumb)+¬+
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶

            display=display+ordered    
            Bulbstotal=Bulbstotal+tot
    endif    
    
    
    if item≥204 and item<5965
        item=val(extract(extract(enter,¶,numb),chr(43),1))
        size=extract(extract(enter,¶,numb),chr(43),2)
        size=?(size="", "a",size)
        quant=val(extract(extract(enter,¶,numb),chr(43),3))
        quant=?(quant=0,1, quant)
        id_number=19999
        price=val(extract(extract(enter,¶,numb),chr(43),4))
        sdsnumb=val(extract(extract(enter,¶,numb),chr(43),6))
        if price=0
            case size="a" or size="A"
                price=lookup("45seeds prices","Item",item,"priceA",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szA",0,0)," seeds","sds")
            case size="b" or size="B"
                price=lookup("45seeds prices","Item",item,"priceB",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szB",0,0)," seeds","sds")
            case size="c" or size="C"
                price=lookup("45seeds prices","Item",item,"priceC",0,0)                
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szC",0,0)," seeds","sds")
            case size="d" or size="D"
                price=lookup("45seeds prices","Item",item,"priceD",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szD",0,0)," seeds","sds")
            case size="e" or size="E"
                price=lookup("45seeds prices","Item",item,"priceE",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szE",0,0)," seeds","sds")
            case size="k" or size="K"
                price=lookup("45seeds prices","Item",item,"priceK",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szK",0,0)," seeds","sds")
            case size="l" or size="L"
                price=lookup("45seeds prices","Item",item,"priceL",0,0)
                sdsnumb=replace(lookup("45seeds prices","Item",item,"szL",0,0)," seeds","sds")
            endcase
        endif
        if Staff="Y"




        endif
        
        if val(item)>0 And price=0
            GetScrap "What is the price of "+str(item)+"?"
            price=val(clipboard())
        endif
        
        tot=val(quant)*price
        
        if val(item)=1000
            sdsnumb=extract(extract(enter,¶,numb),chr(43),6)
            if size=""
                getscrap "what size is 1000?"
                sdsnumb=clipboard()
            endif
        endif
        
       ordered=str(id_number) +¬+str(item)+"-"+upper(size)+¬+" "+
            ?(val(item)=1000,
            rep(chr(32),19-length(extract(extract(enter,¶,numb),"+",6)[1,19]))
            +extract(extract(enter,¶,numb),"+",6)[1,19],
            lookup("45seeds prices","Item",item,"Description","Special",0)[1,23])+¬+ rep(chr(32),23-length(lookup("45seeds prices","Item",item,"Description",
            "Special",0)[1,23]))+
            rep(chr(32),7-length(str(sdsnumb)))+str(sdsnumb)+¬+
            rep(chr(32), 6-length(str(quant)))+str(quant)+¬+
            rep(chr(32),7-length(pattern(price,"#.##")))+pattern(price,"$#.##")+¬+
            rep(chr(32),8-length(pattern(tot,"#.##")))+pattern(tot,"$#.##")+¶

            display=display+ordered    
           // debug
            Seedtotal=Seedtotal+tot
    endif    
    
    
    
    
    numb=numb+1
stoploopif numb>arraysize(strip(enter),¶)
while forever


ArraySort display,display,¶
Order=display
Order=arraystrip(Order,¶)
ArrayFilter Order, orderdisplay, ¶,rep(chr(32),3)+arraydelete(extract(Order,¶,seq()),1,1,¬)

Subtotal=OGStotal+Moosetotal+Bulbstotal+Seedtotal+Treetotal+Seedlingtotal
local quien,submarine
quien=0
submarine=0
;; if there is a customer number in the walk-in record, then search for it in the discount table grab the TotalPurchases value.
if «C#»>0
    quien=«C#»
    window "discounttable"
    find «C#»=quien
    if info("found")=0
        window waswindow
    else
        submarine=TotalPurchases
        window waswindow
    endif
endif

if (OGStotal≥2500 or OGStotal+submarine≥2500) and Special≠"Y" and TransactionType≠"Transfer" and Staff≠"Y" and SpareText3≠"Y"
    Special="Y"
    goto sobulky
endif

«%Discount»=?(OGStotal+submarine≥1200,.20,?(OGStotal+submarine≥600,.15,?(OGStotal+submarine≥300,.1,?(OGStotal+submarine≥100,.05,0))))
if Special="Y" or Staff="Y" or Transfer="Y"
    «%Discount» = 0
else
    if monthvalue(today()) = 11 or monthvalue(today()) = 12
        «%Discount»=«%Discount»+.05
        Notes=?(Notes notcontains "early bird", Notes+¶+"Early Bird Discount of 5% has been added to any other volume discounts", Notes)
    else
    
/***** This piece of code below is for our open house dates. An additional 5% is given to any walkin sales SB 1/12/23*****/

        if today() >= date("1/30/23") and today() <= date("2/10/23")
            «%Discount»=«%Discount»+.05

            Notes=?(Notes notcontains "customer appreciation", Notes+¶+"Customer Appreciation Discount of 5% has been 
            added to any other volume discounts", Notes)
        endif
/***** END *****/
    endif
endif
«OGSTallyDiscount»=?(Special="Y" or Staff="Y" or SpareText3="Y" or Transfer="Y",0, «OGSTallyDiscount»)

if MTDiscount = 1
    goto gotfreepotatoes
endif

MTDiscount=?(Moosetotal≥1200, .20, ?(Moosetotal≥600, .15,?(Moosetotal≥300, .10,?(Moosetotal≥100,.05,0))))
if MTDiscount < 1
    ;; for Tree Sale
    MTDiscount=0  
endif

if (Special="Y" or Staff="Y") and MTDiscount < 1
    MTDiscount=0
endif

;;


FBDiscount=?(Bulbstotal≥1200, .20, ?(Bulbstotal≥600, .15,?(Bulbstotal≥300, .10,?(Bulbstotal≥100,.05,0))))
;;give at least 15% discount to everyone
;;FBDiscount=?(Bulbstotal≥1200, .20, .15)
FBDiscount=0.3

if (Staff="Y")
    FBDiscount=0
endif

//debug

SDSDiscount=?(Seedtotal≥1200, .20, ?(Seedtotal≥600, .15,?(Seedtotal≥300, .10,?(Seedtotal≥100,.05,0))))
if (Staff="Y")
    SDSDiscount=0.5
endif    

    /***** This piece of code below is for our open house dates. An additional 5% is given to any walkin sales SB 1/12/23*****/
    
 if today() >= date("1/30/23") and today() <= date("2/10/23")
         
     «SDSDiscount»=«SDSDiscount»+.05

    Notes=?(Notes notcontains "customer appreciation", Notes+¶+"Customer Appreciation Discount of 5% has been 
    added to any other volume discounts", Notes)
endif
        
    /***** END *****/



gotfreepotatoes:

Discount=OGStotal*max(«%Discount»,OGSTallyDiscount)+Moosetotal*MTDiscount+Bulbstotal*FBDiscount+Seedtotal*SDSDiscount
MemberDiscount=?(Member="Y",float(Subtotal)*float(.01),0)
Adjtotal=Subtotal-Discount-MemberDiscount
SalesTax=round(float(Adjtotal)*float(.055)+.0001,.01)

if TaxExempt="Y"
    SalesTax=0
endif

Total=Adjtotal+SalesTax+BalanceDue+«$Shipping»
SeedSales=?(Member="Y",Seedtotal-Seedtotal*(SDSDiscount+.01),Seedtotal-Seedtotal*SDSDiscount)
TreeSales=Treetotal
SeedlingSales=Seedlingtotal
OGSSales=?(Member="Y",OGStotal-OGStotal*(«%Discount»+.01),OGStotal-OGStotal*«%Discount»)
MooseSales=?(Member="Y",Moosetotal-Moosetotal*(MTDiscount+.01),Moosetotal-Moosetotal*MTDiscount)
BulbsSales=?(Member="Y",Bulbstotal-Bulbstotal*(FBDiscount+.01),Bulbstotal-Bulbstotal*FBDiscount)

case Staff="Y"
    Notes=?(Notes notcontains "Staff Prices", "Staff Prices applied.",Notes)
case Special="Y"
    ;Notes=?(Notes notcontains "Bulk Prices", Notes+¶+specialname+" gets Bulk Prices",Notes)
    Notes=?(Notes notcontains "Bulk", Notes+¶+"Bulk price, no additional discount applies to OGS items.", Notes)
 case Transfer="Y"
    Notes=?(Notes notcontains "transfer", Notes+¶+"Transfer to "+Name, Notes)
endcase

save   				
drawobjects
if OGStotal>=100 and Name="" and Group=""
    message "Search for customer in discount table and mailing list"
endif
___ ENDPROCEDURE .entry ________________________________________________________

___ PROCEDURE .fbaddtoorder ____________________________________________________
local fbqty, fbsz, newitem
fbqty=""
fbsz=""
gettext "How many", fbsz
;gettext "How many?", fbqty
newitem=extract(fbquerySelection,¬,1)+"+"+extract(fbquerySelection,¬,2)+"+"+fbsz
enter=enter+newitem+¶
fbqty=""
fbsz=""
superobject "OGSEnter", "Open" 
activesuperobject "SetText", enter
activesuperobject "close"
;superobject "fbinput", "open"
ShowPage
___ ENDPROCEDURE .fbaddtoorder _________________________________________________

___ PROCEDURE .fbfind __________________________________________________________
noshow
if val(fbitem)≥6000 and val(fbitem) < 7000
    fborderitem=fbitem+¬
    fbitem=""
    call ".fbaddtoorder"
endif
waswindow=info("windowname")
window "45bulbs lookup:Secret"
select name contains fbitem
arrayselectedbuild fbentryitem, ¶, info("databasename"), str(number)+¬+name
selectall
window waswindow
showpage
fbitem=""
endnoshow

___ ENDPROCEDURE .fbfind _______________________________________________________

___ PROCEDURE .fbLiveQuery _____________________________________________________
case val(fbliveQuery) > 0
    liveclairvoyance fbliveQuery, fbqueryResults, ¶, "FB Query List", "45bulbs lookup", str(number), "", 
    arraysort(
    ?(«price A» > 0, str(number)+¬+"A"+¬+name+chr(45),"")
    +?(«price B» > 0, str(number)+¬+"B"+¬+name+chr(45),"")
    +?(«price C» > 0, str(number)+¬+"C"+¬+name+chr(45),"")
    +?(«price D» > 0, str(number)+¬+"D"+¬+name+chr(45),""),chr(45)), 
    10,0,"selected"
defaultcase
    liveclairvoyance fbliveQuery, fbqueryResults, ¶, "FB Query List", "45bulbs lookup", name, "", arraysort(
    ?(«price A» > 0, str(number)+¬+"A"+¬+name+chr(45),"")
    +?(«price B» > 0, str(number)+¬+"B"+¬+name+chr(45),"")
    +?(«price C» > 0, str(number)+¬+"C"+¬+name+chr(45),"")
    +?(«price D» > 0, str(number)+¬+"D"+¬+name+chr(45),""),chr(45)), 10,0,"selected"   
endcase

superobject "FB Query List", "FillList"
___ ENDPROCEDURE .fbLiveQuery __________________________________________________

___ PROCEDURE .finished ________________________________________________________
if Status="Com"
stop
endif
if Paid=0 and Total>0 and PurchaseOrder="" and TransactionType notcontains "Donation" and TransactionType notcontains "trans" and TransactionType notcontains "owes"
    Message "Please complete the sale"
    stop    
endif
if TransactionType=""
message "Set Transaction Type"
stop
endif
Status="Com"

if TransactionType="PurchaseOrder" or TransactionType="Owes" and email=""
;if «C#»>0, email=lookup("discounttable","C#",«C#»,"email","",0)
;else
    gettext "Email Invoice To:",email
;endif
endif

if TransactionType≠"PurchaseOrder" and (Notes contains "PO" or Notes contains "purchase order")
    Notes=replace(Notes,"PO","")
    Notes=replace(Notes,"purchase order","")
endif
if TransactionType≠"Donation" and Notes contains "donation"
    Notes=replace(Notes,"Donation","")
endif

if TransactionType="Transfer"
    Paid=Total
endif

if «C#»>0
    local quien,totalitarian
    quien=«C#»
    totalitarian=(OGSSales/(OGSSales+MooseSales+BulbsSales+SeedSales))*Subtotal
    window "discounttable"
    selectall
    select «C#»=quien
    if info("selected")<info("records")
        thisyearwalkinpurchases=thisyearwalkinpurchases+totalitarian
        field thisyearwalkinpurchases
        copycell
        pastecell
        call discountfill
        selectall
        find «C#»=quien
    else 
        message "Who?"
    endif
endif
window "Walkin Register45:sales"
___ ENDPROCEDURE .finished _____________________________________________________

___ PROCEDURE .giftcertificate _________________________________________________
TransactionType="Gift Certifcate"
giftamount=0
giftused= 0
getscrap "What is the value of the gift certifcate?"
giftamount=val(clipboard())
if giftamount≥Total
giftused=Total
else
giftused=giftamount
endif
giftamount=giftamount-giftused
if giftused=Total
Paid=giftused
«Gift_Certificate»=Paid
 getscrap "What's the certificate code?"
 Notes=Notes+¶+"Paid "+pattern(«Gift_Certificate»,"$#.##")+" with gift certifcate "+clipboard()+" amount left "+str(giftamount)
message "Adjust the certificate online"
applescript |||
		  tell application "Firefox"
			 activate 
    open location "https://fedcoseeds.com/manage_site/gift-certificates"
 end tell  |||
 else
 Paid=giftused
 «Gift_Certificate»=Paid
  getscrap "What's the certificate code?"
 Notes=Notes+¶+"Paid "+pattern(«Gift_Certificate»,"$#.##")+" with gift certifcate "+clipboard()+" amount left "+str(giftamount)
message "Additional owed "+str(Total-Paid)
 call ".mixedpayment"
 endif


___ ENDPROCEDURE .giftcertificate ______________________________________________

___ PROCEDURE .LiveQuery _______________________________________________________
fileglobal liveQuery, queryResults, queryResultsw
;message val(liveQuery)
case val(liveQuery)>0
liveclairvoyance liveQuery, queryResults, ¶, "Query List", "45ogscomments.linked", Item, "contains", str(Item)+¬+Description+¬+UnitNumber+¬+UnitName+¬+«unit note», 5,0,""
liveclairvoyance liveQuery, queryResultsw, ¶, "Query Listw", "45ogscomments.warehouse", Item, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 30,0,""
defaultcase
liveclairvoyance liveQuery, queryResults, ¶, "Query List", "45ogscomments.linked", Description, "contains", str(Item)+¬+Description+¬+UnitNumber+¬+UnitName+¬+«unit note», 30,0,""
liveclairvoyance liveQuery, queryResultsw, ¶, "Query Listw", "45ogscomments.warehouse", Description, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 30,0,""
endcase
superobject "Query List", "FillList"
superobject "Query Listw", "FillList"

/* 
OLD CODE DO NOT DELETE FOR NOW PLEASE 8/3/22 (SB)

fileglobal liveQuery, queryResults, queryResultsw
;message val(liveQuery)
case val(liveQuery)>0
liveclairvoyance liveQuery, queryResults, ¶, "Query List", "45ogscomments.linked", Item, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 5,0,""
liveclairvoyance liveQuery, queryResultsw, ¶, "Query Listw", "45ogscomments.warehouse", Item, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 30,0,""
defaultcase
liveclairvoyance liveQuery, queryResults, ¶, "Query List", "45ogscomments.linked", Description, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 30,0,""
liveclairvoyance liveQuery, queryResultsw, ¶, "Query Listw", "45ogscomments.warehouse", Description, "contains", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note», 30,0,""
endcase
superobject "Query List", "FillList"
superobject "Query Listw", "FillList"

*/
___ ENDPROCEDURE .LiveQuery ____________________________________________________

___ PROCEDURE .member __________________________________________________________
if info("activesuperobject")≠""
activesuperobject "close"
endif
call ".entry"
___ ENDPROCEDURE .member _______________________________________________________

___ PROCEDURE .mixedpayment ____________________________________________________
fileglobal tendered, change, addpay, paychoice
addpay=0
TransactionType="Mixed"
loop
tendered=0
popup "Cash"+¶+"Check"+¶+"Credit Card"+¶+"Gift Certificate", 175, 470, "Cash", paychoice
case paychoice="Cash"
gettext "Amount from Customer", tendered
Cash=Cash+val(tendered)
Paid=Paid+Cash
case paychoice="Check"
gettext "Amount of Check", tendered
Check=Check+val(tendered)
Paid=Paid+Check
case paychoice="Credit Card"
gettext "Amount of charge", tendered
CreditCard=CreditCard+val(tendered)
Paid=Paid+CreditCard
case paychoice="Gift Certificate"
giftamount=0
giftused=0
getscrap "What is the value of the gift certifcate?"
giftamount=val(clipboard())
gettext "Amount used", tendered
if val(tendered)>giftamount
message "that won't work"
stop
endif
Gift_Certificate=Gift_Certificate+val(tendered)
giftused=val(tendered)
giftamount=giftamount-giftused
Paid=Paid+Gift_Certificate
 getscrap "What's the certificate code?"
 Notes=Notes+¶+"Paid "+pattern(«Gift_Certificate»,"$#.##")+" with gift certifcate "+clipboard()+" amount left "+pattern(giftamount,"$#.##")
endcase
change=Paid-Total
if change>0
message "Change is "+pattern(change,"#.##")
Paid=Paid-change
Cash=Cash-change
endif

stoploopif Paid≥Total
message "Need "+pattern(abs(change),"$#.##")+" more"
repeatloopif change<0
while forever

case Cash+Check+CreditCard+«Gift_Certificate»≠Total
message "Math is off, please double-check"
endcase
case Check<0 or CreditCard<0 or «Gift_Certificate»<0
message "Something is weird, check the math"
endcase
case «Gift_Certificate»>0
message "Adjust the value of the gift certificate online"
applescript |||
		  tell application "Firefox"
			 activate 
    open location "https://fedcoseeds.com/manage_site/gift-certificates"
 end tell  |||
 endcase


___ ENDPROCEDURE .mixedpayment _________________________________________________

___ PROCEDURE .mtaddtoorder ____________________________________________________
local mtqty, mtsz, newitem
mtqty=""
mtsz=""
gettext "How many", mtsz
;gettext "How many?", mtqty
newitem=extract(mtquerySelection,¬,1)+"+"+extract(mtquerySelection,¬,2)+"+"+mtsz
enter=enter+newitem+¶
mtqty=""
mtsz=""
superobject "OGSEnter", "Open" 
activesuperobject "SetText", enter
activesuperobject "close"
;superobject "mtinput", "open"
ShowPage
___ ENDPROCEDURE .mtaddtoorder _________________________________________________

___ PROCEDURE .mtfind __________________________________________________________
noshow
if val(mtitem)≥7000
mtorderitem=mtitem+¬
mtitem=""
call ".mtaddtoorder"
endif
waswindow=info("windowname")
window "45mt prices:Secret"
select Variety contains mtitem
arrayselectedbuild mtentryitem, ¶, info("databasename"), str(Item)+¬+Variety
selectall
window waswindow
showpage
mtitem=""
endnoshow

___ ENDPROCEDURE .mtfind _______________________________________________________

___ PROCEDURE .mtLiveQuery _____________________________________________________
case val(mtliveQuery) > 0
    liveclairvoyance mtliveQuery, mtqueryResults, ¶, "MT Query List", "45mt prices", str(Item), "", 
    arraysort(
    ?(priceA > 0, str(Item)+¬+"A"+¬+Variety+chr(45),"")
    +?(priceB > 0, str(Item)+¬+"B"+¬+Variety+chr(45),"")
    +?(priceC > 0, str(Item)+¬+"C"+¬+Variety+chr(45),"")
    +?(priceD > 0, str(Item)+¬+"D"+¬+Variety+chr(45),"")
    +?(priceE > 0,str(Item)+¬+"E"+¬+Variety+chr(45),""),chr(45)), 
    10,0,"selected"
defaultcase
    liveclairvoyance mtliveQuery, mtqueryResults, ¶, "MT Query List", "45mt prices", Variety, "", arraysort(
    ?(priceA > 0, str(Item)+¬+"A"+¬+Variety+chr(45),"")
    +?(priceB > 0, str(Item)+¬+"B"+¬+Variety+chr(45),"")
    +?(priceC > 0, str(Item)+¬+"C"+¬+Variety+chr(45),"")
    +?(priceD > 0, str(Item)+¬+"D"+¬+Variety+chr(45),"")
    +?(priceE > 0,str(Item)+¬+"E"+¬+Variety+chr(45),""),chr(45)), 10,0,"selected"   
endcase

superobject "MT Query List", "FillList"
___ ENDPROCEDURE .mtLiveQuery __________________________________________________

___ PROCEDURE .newcharge _______________________________________________________
Paid=Total
TransactionType="CC"
message "Use the Square!"


  
 

		

___ ENDPROCEDURE .newcharge ____________________________________________________

___ PROCEDURE .newmtaddtoorder _________________________________________________
local mtqty, mtsz, newitem
mtqty=""
mtsz=""

gettext "Enter size and quantity separated by a space: d 4", mtqty
newitem=extract(mtorderitem,¬,1)+"+"+extract(mtqty," ",1)+"+"+extract(mtqty," ",2)
enter=enter+newitem+¶
mtqty=""
mtsz=""
superobject "OGSEnter", "Open" 
activesuperobject "SetText", enter
activesuperobject "close"
superobject "mtinput", "open"
ShowPage
___ ENDPROCEDURE .newmtaddtoorder ______________________________________________

___ PROCEDURE .newmtpoplist ____________________________________________________
global mtitemlist, mtorderitem, thiswindow
thiswindow=info("windowname")
mtorderitem=""
mtitemlist=""
window "45mt prices:secret"
select Variety["A-Z";1]=mtstringer
arrayselectedbuild mtitemlist, ¶, "45mt prices", Variety+¬+str(Item)
arraysort mtitemlist, mtitemlist, ¶
arrayfilter mtitemlist, mtitemlist, ¶, arrayreverse(extract(mtitemlist,¶, seq()),¬)
window thiswindow
popupclick mtitemlist,"", mtorderitem

if mtorderitem=""
    stop
endif

call ".newmtaddtoorder"

___ ENDPROCEDURE .newmtpoplist _________________________________________________

___ PROCEDURE .newpoplist ______________________________________________________
global itemlist, orderitem, thiswindow
thiswindow=info("windowname")
orderitem=""
itemlist=""


window "45ogscomments.linked:secret"
select headers=header
Selectwithin headers<>""
arrayselectedbuild itemlist, ¶, "45ogscomments.linked",«unit note»+¬+?(«Sz.»>0,str(«Sz.»)+"#","")+¬+Description+¬+str(Item)
arrayfilter itemlist, itemlist, ¶, arrayreverse(extract(itemlist,¶, seq()),¬)
arraysort itemlist, itemlist, ¶
window thiswindow
popupclick itemlist,"", orderitem
if orderitem=""
;Message "Nothing Selected"
stop
endif

querySelection = orderitem
call ".addtoordernew"

___ ENDPROCEDURE .newpoplist ___________________________________________________

___ PROCEDURE .newwalkinfind ___________________________________________________
local findinwalkin, findinmailinglist

case info("ActiveSuperObject")="WalkinName"
    liveclairvoyance walkinname, findinwalkin,¶,"specialpersonlist", "discounttable", Con, "contains", str(«C#»)+¬+Group+¬+Con,10,0,""
    liveclairvoyance walkinname, findinmailinglist,¶,"mailinglistlist", "45 mailing list", Con, "contains", str(«C#»)+¬+Group+¬+Con+"-"+City+" "+St,10,0,""
case info("ActiveSuperObject")="WalkinGroup"
    liveclairvoyance walkingroup, findinwalkin,¶,"specialpersonlist", "discounttable", Group, "contains", str(«C#»)+¬+Group+¬+Con,10,0,""
    liveclairvoyance walkingroup, findinmailinglist,¶,"mailinglistlist", "45 mailing list", Group, "contains", str(«C#»)+¬+Group+¬+Con+"-"+City+" "+St,10,0,""
endcase
___ ENDPROCEDURE .newwalkinfind ________________________________________________

___ PROCEDURE .ogsfind _________________________________________________________
local PopTop, PopLeft
noshow
if val(ogsitem)>8000
    orderitem=ogsitem+¬
    ogsitem=""
    call ".addtoorder"
endif

waswindow=info("windowname")

liveclairvoyance ogsitem, entryitem, ¶, "ogspopup", "45ogscomments.linked", Description, "", ?(headers="","",str(Item)+¬+Description+¬+str(«Sz.»)), 0,0,""
if arraysize(entryitem,¶)=recordsize
    YesNo "Try again?"
    If info("trigger")="Yes"
        superobject "ogslist", "open"
    endif
endif
window waswindow

superobject "ogspopup", "FIllList"
showpage
ogsitem=""
endnoshow
___ ENDPROCEDURE .ogsfind ______________________________________________________

___ PROCEDURE .openaddress _____________________________________________________
setwindowrectangle rectanglesize(704,1267,347,528),""
openform "CollectAddress"
___ ENDPROCEDURE .openaddress __________________________________________________

___ PROCEDURE .openremindme ____________________________________________________
setwindowrectangle rectanglesize(504,1067,280,600),""
openform "Remindme"
___ ENDPROCEDURE .openremindme _________________________________________________

___ PROCEDURE .paidcash ________________________________________________________
fileglobal tendered, change, addpay, paychoice
tendered=0
addpay=0
popup "Cash"+¶+"Check"+¶+"Money Order", 150, 500, "Cash", paychoice

TransactionType=paychoice


if paychoice="Cash"
gettext "Amount from Customer", tendered
change=val(tendered)-Total
if change<0
message "Need "+pattern(abs(change),"$#.##")+" more"
gettext "Additional payment", addpay
tendered=str(val(tendered)+val(addpay))
change=val(tendered)-Total
message "Change is "+pattern(change,"$#.##")
else
if change>0
message "Change is "+pattern(change,"$#.##")
endif
endif
endif
Paid=Total
stop
___ ENDPROCEDURE .paidcash _____________________________________________________

___ PROCEDURE .paidowed ________________________________________________________
Paid=MoneyTendered
«Date Paid»=today()
if Notes contains "owes"
Notes=replace(Notes, " owes", " paid "+datepattern(today(), "mm/dd/yy"))
endif
PurchaseOrder=replace(PurchaseOrder, "owes","")
if Notes contains "purchase order" 
Notes=Notes+¶+"paid "+datepattern(today(), "mm/dd/yy")
endif
TransactionType=""
message "Set Transacton Type"
___ ENDPROCEDURE .paidowed _____________________________________________________

___ PROCEDURE .poplist _________________________________________________________
global itemlist, orderitem, thiswindow
thiswindow=info("windowname")
orderitem=""
itemlist=""
window "45ogscomments.linked:secret"
select headers=header
arrayselectedbuild itemlist, ¶, "45ogscomments.linked", str(Item)+¬+Description+¬+str(«Sz.»)+"#"+¬+«unit note»
window thiswindow
popupclick itemlist,"", orderitem
if orderitem=""
stop
endif
call ".addtoordernew"

___ ENDPROCEDURE .poplist ______________________________________________________

___ PROCEDURE .purchaseorder ___________________________________________________
if info("trigger")="Button.Purchase Order"
TransactionType="PO"
if Group="" and Name=""
getscrap "Who is using this PO?"
Group=clipboard()
endif
endif
if PurchaseOrder="" 
gettext "What is the PO #?",PurchaseOrder
endif
Notes=?(Group≠"", Group, Name)+" PO "+str(PurchaseOrder)


___ ENDPROCEDURE .purchaseorder ________________________________________________

___ PROCEDURE .recordcustomer __________________________________________________
global waswindow
waswindow=info("windowname")

if ChosenOne=""
    message "oops!"
    stop
endif

if info("trigger") = "specialpersonlist"
    ;; customer is already in the discount table, so take their info from there
    
    window "discounttable"
    selectall
    find «C#»=val(extract(ChosenOne,¬,1))
    
    window waswindow
    OGSTallyDiscount=grabdata("discounttable",Discount)

    window "discounttable:secret"
    if Bulk=1
        window waswindow
        Special="Y"
        window "discounttable:secret"
    endif
    
else
    ;; customer is in the mailing list but not in the discount table, so:
    ;; copy mailing list record into discount table
    ;; gather any extra info (like if they're staff)
    ;; copy C# into walk-in record
    
    window "45 mailing list:secret"
    select «C#»=val(extract(ChosenOne,¬,1))
    window "discounttable"
    call "addrecord/7"
endif

window waswindow
«C#»=grabdata("discounttable",«C#»)
Name=grabdata("discounttable",Con)
Group=grabdata("discounttable",Group)
window "discounttable:secret"

if TaxExempt=1
    window waswindow
    TaxExempt="Y"
    resale=grabdata("discounttable",TaxID)
    if resale=""
        getscrap "What's Your Tax ID?"
        if clipboard()=""
            resale="9999"
        else
             resale=clipboard()
        endif
    endif
    window "discounttable:secret"
endif
    
if Mem=1
    window waswindow
    Member="Y"
    window "discounttable:secret"
endif
    
if Staff=1
    window waswindow
    Staff="Y"
endif

window waswindow

call ".entry"
___ ENDPROCEDURE .recordcustomer _______________________________________________

___ PROCEDURE .sdsaddtoorder ___________________________________________________
local sdsqty, sdssz, newitem
sdsqty=""
sdssz=""
gettext "How many", sdssz
;gettext "How many?", sdsqty
newitem=extract(sdsquerySelection,¬,1)+"+"+extract(sdsquerySelection,¬,2)+"+"+sdssz
enter=enter+newitem+¶
sdsqty=""
sdssz=""
superobject "OGSEnter", "Open" 
activesuperobject "SetText", enter
activesuperobject "close"
;superobject "sdsinput", "open"
ShowPage
___ ENDPROCEDURE .sdsaddtoorder ________________________________________________

___ PROCEDURE .sdsfind _________________________________________________________
noshow
if val(sdsitem)≥200 and val(sdsitem)<6000
sdsorderitem=sdsitem+¬
sdsitem=""
call ".sdsaddtoorder"
endif
waswindow=info("windowname")
window "45seeds prices:Secret"
select Description contains sdsitem
arrayselectedbuild sdsentryitem, ¶, info("databasename"), str(Item)+¬+Description
selectall
window waswindow
showpage
sdsitem=""
endnoshow

___ ENDPROCEDURE .sdsfind ______________________________________________________

___ PROCEDURE .sdsLiveQuery ____________________________________________________
case val(sdsliveQuery) > 0
    liveclairvoyance sdsliveQuery, sdsqueryResults, ¶, "SDS Query List", "45seeds prices", str(Item), "", 
    arraysort(
    ?(priceA > 0, str(Item)+¬+"A"+¬+Description+chr(45),"")
    +?(priceB > 0, str(Item)+¬+"B"+¬+Description+chr(45),"")
    +?(priceC > 0, str(Item)+¬+"C"+¬+Description+chr(45),"")
    +?(priceD > 0, str(Item)+¬+"D"+¬+Description+chr(45),"")
    +?(priceE > 0,str(Item)+¬+"E"+¬+Description+chr(45),"")
    +?(priceK > 0,str(Item)+¬+"K"+¬+Description+chr(45),"")
    +?(priceL > 0,str(Item)+¬+"L"+¬+Description+chr(45),""),¶), 
    10,0,"selected"
defaultcase
    liveclairvoyance sdsliveQuery, sdsqueryResults, ¶, "SDS Query List", "45seeds prices", Description, "", arraysort(
    ?(priceA > 0, str(Item)+¬+"A"+¬+Description+chr(45),"")
    +?(priceB > 0, str(Item)+¬+"B"+¬+Description+chr(45),"")
    +?(priceC > 0, str(Item)+¬+"C"+¬+Description+chr(45),"")
    +?(priceD > 0, str(Item)+¬+"D"+¬+Description+chr(45),"")
    +?(priceE > 0,str(Item)+¬+"E"+¬+Description+chr(45),"")
    +?(priceK > 0,str(Item)+¬+"K"+¬+Description+chr(45),"")
    +?(priceL > 0,str(Item)+¬+"L"+¬+Description+chr(45),""),¶), 10,0,"selected"   
endcase

superobject "SDS Query List", "FillList"
___ ENDPROCEDURE .sdsLiveQuery _________________________________________________

___ PROCEDURE .shipping ________________________________________________________
getscrap "What's the shipping?"
«$Shipping»=val(clipboard())
Total=Adjtotal+SalesTax+«$Shipping»+BalanceDue
___ ENDPROCEDURE .shipping _____________________________________________________

___ PROCEDURE .special _________________________________________________________
;global specialname

if info("activesuperobject")≠""
activesuperobject "close"
endif

if Special="Y"
    loop
        Getscrapok "Who is getting this deal?"
    repeatloopif clipboard()=""
    stoploopif clipboard()≠""
    while forever
endif

Name=clipboard()



call ".entry"
___ ENDPROCEDURE .special ______________________________________________________

___ PROCEDURE .staff ___________________________________________________________
;global staffname

if info("activesuperobject")≠""
activesuperobject "close"
endif

if Staff="Y"
    loop
        Getscrapok "Who is getting this deal?"
    repeatloopif clipboard()=""
    stoploopif clipboard()≠""
    while forever
endif

Name=clipboard()

call ".entry"
___ ENDPROCEDURE .staff ________________________________________________________

___ PROCEDURE .taxexempt _______________________________________________________
if info("activesuperobject")≠""
activesuperobject "close"
endif

if TaxExempt="Y" and resale=""
    getscrapok "enter resale number"
    TaxExNo=clipboard()
   ; if resale <>"" and «C#» <> ""
    ;    ;;update TaxEx and resale fields in the mailing list
    ;    post "update", zoyear + " mailing list","C#",val(«C#»),"resale",resale,"TaxEx","Y"
    ;endif
endif

call ".entry"
___ ENDPROCEDURE .taxexempt ____________________________________________________

___ PROCEDURE (macros) _________________________________________________________

___ ENDPROCEDURE (macros) ______________________________________________________

___ PROCEDURE next/1 ___________________________________________________________
fileglobal seller, oldrecord
seller=""
if Paid<Total and Notes notcontains "owes" and TransactionType notcontains "Donation" and TransactionType notcontains "Transfer" and TransactionType notcontains "PO"
   
            Message "Please complete the sale"
            stop

endif


Status="Com"

resynchronize
orderdisplay=""
save
field Transaction
sortup
lastrecord
oldrecord=Transaction
InsertBelow
Transaction=oldrecord+1
Time=timepattern(now(),"HH:MM AM/PM")
enter=""
mtenter=""
mtdisplay=""
fbenter=""
fbdisplay=""
sdsenter=""
sdsdisplay=""
list=""
names=""
Status=""
shippin=0
shipto=""
liveQuery=""
mtliveQuery=""
fbliveQuery=""
sdsliveQuery=""
popup "John Paul"+¶+"Scott"+¶+"Sara Roy"+¶+"Jake"+¶+"Sarah Oliver"+¶+"James"+¶+"Noah"+¶+"Renee"+¶+"Stasha"+¶+"Staff", 125, 1050, "John Paul", seller
Seller=seller
superobject "OGSEnter","Open", "SetText", "", "Close"
superobject "ogsinput", "Open"
drawobjects
___ ENDPROCEDURE next/1 ________________________________________________________

___ PROCEDURE synchronize/0 ____________________________________________________
Synchronize
field Transaction sortup
lastrecord
save
___ ENDPROCEDURE synchronize/0 _________________________________________________

___ PROCEDURE findrecord/4 _____________________________________________________
fileglobal transno
transno=""
gettext "Which transaction?",transno
selectall
find Transaction=val(transno)
___ ENDPROCEDURE findrecord/4 __________________________________________________

___ PROCEDURE spudsales/5 ______________________________________________________
global waswindow
waswindow=info("windowname")
openfile "MTwalkinsales"
deleteall
window waswindow
field Transaction
sortup
global raya, rayb, num
rayb=""
firstrecord
loop
num=1
loop
stoploopif num=arraysize(Order,¶)+1
raya=extract(extract(Order,¶,num),¬,1)[1,4]+¬+extract(extract(Order,¶,num),¬,1)[6,6]
    +¬+extract(extract(Order,¶,num),¬,1)[7,-2]+¬+extract(extract(Order,¶,num),¬,3)
rayb=rayb+¶+raya
rayb=arraystrip(rayb,¶)
num=num+1
while forever
window "MTwalksales:secret"
openfile "+@rayb"
window waswindow
rayb=""
downrecord
until info("stopped")
goform "sales"
selectall
lastrecord
save
window "MT tree sales"
field Item
select Item<8000 and Item>7000
removeunselected
stop
groupup
field Qty
total
RemoveDetail  "data"
lastrecord
deleterecord
save


___ ENDPROCEDURE spudsales/5 ___________________________________________________

___ PROCEDURE ogssales/6 _______________________________________________________
global waswindow
;Synchronize
lasttime=307665
;stop
waswindow=info("windowname")
openfile "45ogswalkinsales"
window waswindow
field Transaction
sortup
global raya, rayb, num
rayb=""
find Transaction≥lasttime
if info("found")=0
stop
endif
loop
num=1
loop
stoploopif num=arraysize(Order,¶)+1
raya=str(Transaction)+¬+extract(Order,¶,num)+¬+datepattern(Date,"mm/dd/yy")
rayb=rayb+¶+raya
rayb=arraystrip(rayb,¶)
num=num+1
while forever
window "45ogswalkinsales:secret"
openfile "+@rayb"
window waswindow
rayb=""
downrecord
stoploopif info("eof")
until info("stopped")
num=1
loop
stoploopif num=arraysize(Order,¶)+1
raya=str(Transaction)+¬+extract(Order,¶,num)+¬+datepattern(Date,"mm/dd/yy")
rayb=rayb+¶+raya
rayb=arraystrip(rayb,¶)
num=num+1
while forever
window "45ogswalkinsales:secret"
openfile "+@rayb"
window waswindow
rayb=""
;lasttime=Transaction
goform "sales"
selectall
lastrecord
save
window "45ogswalkinsales"
field Item
select val(Item[1,4])>8000
removeunselected
save
stop
call "walkin ordered"


___ ENDPROCEDURE ogssales/6 ____________________________________________________

___ PROCEDURE searchnotes/† ____________________________________________________
fileglobal searchtext
gettext "Search Notes for", searchtext
find Notes contains searchtext
___ ENDPROCEDURE searchnotes/† _________________________________________________

___ PROCEDURE (extras) _________________________________________________________

___ ENDPROCEDURE (extras) ______________________________________________________

___ PROCEDURE build_vertical_file ______________________________________________
global order, neworder,firstorder, newseq, order_len
local allwindows, numwindows, n, onewindowname

;; go to the datasheet. If the datasheet is not currently open, 
;; go to the active window and switch it to the datasheet
gosheet

;; create a list of all of the currently open windows in the tally
allwindows = listwindows("Walkin Register45")

;; Since we just ran "gosheet," the datasheet will be the active
;; window, and will be the first one in the list of windows. We're
;; going to close all of the windows in this list, so before we start
;; that, we want to delete the datasheet from the list.

if arraysize(allwindows,¶) > 1
    allwindows = arraydelete(allwindows,1,1,¶)
    numwindows = arraysize(allwindows,¶)

    ;; loop through the list of windows (except for the datasheet)
    ;; and close them all
    n = 0
    loop
        onewindowname = array(allwindows,n+1,¶)
        window onewindowname
        closewindow
        n = n+1
        numwindows = numwindows - 1
        stoploopif numwindows = 0
    while forever

endif

waswindow=info("windowname")
 
openfile "45walkin_vertical_ogs"
removesummaries "7"
selectall
field TransactionNo
Sortup
lastrecord
newseq=TransactionNo

window waswindow
select  Transaction>newseq and Transfer notcontains "Y"
if info("empty")
    message "No new orders"
    stop
endif
Field Transaction
Sortup

firstrecord
firstorder=Transaction
window "45walkin_vertical_ogs"
if error 
    openfile "45walkin_vertical_ogs"
endif
window waswindow
neworder=""
order=""
firstrecord
loop
    order=Order
    order_len = arraysize(extract(order,¶,1),¬)
    
    case order_len = 7
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+import()
    case order_len = 6
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+""+¬+import()
    case order_len = 5
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+""+¬+extract(import(),¬,1)+¬+extract(import(),¬,2)+¬+"0"+¬+extract(import(),¬,3)+¬+extract(import(),¬,4)+¬+extract(import(),¬,5)
    endcase
    
    window "45walkin_vertical_ogs:secret"
    openfile "+@neworder"
    window waswindow
    downrecord
    order=""
    neworder=""
until info("stopped")

window waswindow
selectall
window "45walkin_vertical_ogs"

call "get_IDs"

;;--------------------------------------------------------------------
;; ------ build transfers vertical file ------
;;--------------------------------------------------------------------

openfile "45transfers_vertical_ogs"
removesummaries "7"
selectall
field TransactionNo
Sortup
lastrecord
newseq=TransactionNo

window waswindow
select  Transaction>newseq and Transfer contains "Y"
Field Transaction
Sortup
if info("selected")=info("records")
    message "No new transfers"
    stop
endif

firstrecord
firstorder=Transaction
window "45transfers_vertical_ogs"
if error 
    openfile "45transfers_vertical_ogs"
endif
window waswindow
neworder=""
order=""
firstrecord
loop
    order=Order
    order_len = arraysize(extract(order,¶,1),¬)
    
    case order_len = 7
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+import()
    case order_len = 6
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+""+¬+import()
    case order_len = 5
        ArrayFilter order,neworder,¶,str(Transaction)+¬+str(Date)+¬+""+¬+extract(import(),¬,1)+¬+extract(import(),¬,2)+¬+"0"+¬+extract(import(),¬,3)+¬+extract(import(),¬,4)+¬+extract(import(),¬,5)
    endcase
    
    window "45transfers_vertical_ogs:secret"
    openfile "+@neworder"
    window waswindow
    downrecord
    order=""
    neworder=""
until info("stopped")

window waswindow
selectall
window "45transfers_vertical_ogs"

call "get_IDs"

message "Finished Walk-in build vertical file macro"
___ ENDPROCEDURE build_vertical_file ___________________________________________

___ PROCEDURE checkcashbox _____________________________________________________
local cash, dater
dater=""
cash=0
gosheet

gettext "What date?",dater
message dater
select Date>date(dater) and TransactionType="Cash"

if info("selected")=info("records")
goform "sales"
message "No Cash Today"
else
Field Paid
Total
lastrecord
cash=Paid
removesummaries 7
selectall
select Date>date(dater) and TransactionType="Mixed" and Cash>0

if info("selected")=info("records") 
goto donetime
else 
field Cash
Total
lastrecord
cash=cash+Cash
removesummaries 7
endif
endif
donetime:
selectall
lastrecord
goform "sales"
message "Cash Sales: "+pattern(cash,"$#,.##")

clipboard()=str(cash)
___ ENDPROCEDURE checkcashbox __________________________________________________

___ PROCEDURE receipt __________________________________________________________
global waswindow
waswindow=info("formname")
;superobject "OGSEnter","Open","Clear"
printusingform "","receipt"
printonerecord dialog 
«SpareText4»="Receipt Printed"

GoForm waswindow
if Paid≤0 and Total>0 and PurchaseOrder="" and Notes notcontains "owes" and TransactionType notcontains "don" and TransactionType notcontains "PO" and TransactionType notcontains "transfer"
    Message "Please complete the sale"
    stop
    endif

___ ENDPROCEDURE receipt _______________________________________________________

___ PROCEDURE checkchecks ______________________________________________________
local checkers, dater
dater=""
checkers=0
gosheet

gettext "What date?",dater
message dater
select Date>date(dater) and  TransactionType="Check"



if info("selected")=info("records")
goform "sales"
message "No Checks Today"
else
Field Paid
Total
lastrecord
checkers=Paid
removesummaries 7
selectall
select Date>date(dater) and TransactionType="Mixed" and Check>0

if info("selected")=info("records")
goto endtimes
else
field Check
Total
lastrecord
checkers=checkers+Check
removesummaries 7
endtimes:
selectall
lastrecord
goform "sales"
message "Check Sales: "+pattern(checkers,"$#,.##")
endif
endif
clipboard()=str(checkers)


___ ENDPROCEDURE checkchecks ___________________________________________________

___ PROCEDURE reconcile ________________________________________________________
local credit, dater
dater=""
credit=0
gosheet
gettext "What date?",dater
message dater
select Date>date(dater) and TransactionType="CC"

Field Paid
Total
lastrecord
credit=Paid
removesummaries 7
selectall
select Date>date(dater) and TransactionType="Mixed" and CreditCard>0



if info("selected")=info("records")
goto enditall
else
field CreditCard
Total
lastrecord
credit=credit+CreditCard
removesummaries 7
endif
enditall:
selectall
lastrecord
goform "sales"
message "Credit Card Sales: "+pattern(credit,"$#,.##")
clipboard()=str(credit)
___ ENDPROCEDURE reconcile _____________________________________________________

___ PROCEDURE .monthlytotal ____________________________________________________
synchronize
local totes,lastyear,diff
select monthvalue(Date)=monthvalue(today()) and TransactionType notcontains "don" and TransactionType notcontains "transfer"
field Adjtotal
Total
lastrecord
totes=Adjtotal
openfile "Walkin Register43.unlinked"
select monthvalue(Date)=monthvalue(today()-365) and Date≤today()-365 and TransactionType notcontains "don" and TransactionType notcontains "transfer"
field Adjtotal
Total
lastrecord
lastyear=Adjtotal
diff=divzero(totes,lastyear)-1
window "Walkin Register45:sales"
message "Sales So Far This Month "+pattern(totes,"$#,.##")+¶+"So Far This Month Last Year "+pattern(lastyear,"$#,.##")+¶+?(diff≥0,"Up ","Down ")+str(diff*100)+"%"
removeallsummaries
selectall
lastrecord
___ ENDPROCEDURE .monthlytotal _________________________________________________

___ PROCEDURE .ytdtotal ________________________________________________________
synchronize
local totes,lastyear,diff
select TransactionType notcontains "don" and TransactionType notcontains "transfer"
field Adjtotal
Total
lastrecord
totes=Adjtotal
openfile "Walkin Register43.unlinked"
select Date≤today()-365 and TransactionType notcontains "don" and TransactionType notcontains "transfer"
field Adjtotal
Total
lastrecord
lastyear=Adjtotal
removeallsummaries
selectall
diff=divzero(totes,lastyear)-1
window "Walkin Register45:sales"
message "Sales So Far This Year "+pattern(totes,"$#,.##")+¶+"So Far Last Year "+pattern(lastyear,"$#,.##")+¶+?(diff≥0,"Up ","Down ")+str(diff*100)+"%"
removeallsummaries
selectall
lastrecord
___ ENDPROCEDURE .ytdtotal _____________________________________________________

___ PROCEDURE .daily ___________________________________________________________
synchronize
local totes
select Date=today() and TransactionType notcontains "don" and TransactionType notcontains "transfer"
field Adjtotal
Total
lastrecord
totes=pattern(Adjtotal,"$#,.##")
message "Today's sales: "+totes
removesummaries 7
selectall
lastrecord
___ ENDPROCEDURE .daily ________________________________________________________

___ PROCEDURE .transfer ________________________________________________________

local vExpenseChoice,vExpenseArray

vExpenseChoice=""
vExpenseArray="Building Maintenance,Product Research,Testing Services,Other"

if Transfer="Y"
TransactionType="Transfer"
TaxExempt="Y"


loop
getscrapok "Division? Seeds, Trees, Bulbs, OGS?"
repeatloopif clipboard()="" or (clipboard() notcontains "seeds" and clipboard() notcontains "trees" and clipboard() notcontains "bulbs" and clipboard() notcontains "ogs")
stoploopif clipboard()≠""
while forever
Name=upperword(clipboard())
Notes="Transfer to "+Name

//Added by Lunar 6-2022
//This uses the array vExpense Array to fill in the user's numeric choice in account
//if it's an "other"
TransDivision=Name
loop
getscrapok "Acct# 1-BldgMaint,2-Research,3-Testing,4-Other"
vExpenseChoice=val(striptonum(clipboard()))
repeatloopif vExpenseChoice>4 or vExpenseChoice<1
stoploopif clipboard()≠""
while forever 
if vExpenseChoice<4
ExpenseAcct=array(vExpenseArray,vExpenseChoice,",")
else
getscrapok "Other Account is:"
ExpenseAcct=array(vExpenseArray,vExpenseChoice,",")+" "+"-"+" "+upperword(clipboard())
endif
Notes=Notes+" - "+ExpenseAcct
//end of Lunar's addition

call .entry
Else TransactionType=""
TaxExempt="N"
Notes=""
Name=""
endif

/*
Previous Code 6-2022
if Transfer="Y"
TransactionType="Transfer"
TaxExempt="Y"
loop
getscrapok "Which Division?"
repeatloopif clipboard()="" or (clipboard() notcontains "seeds" and clipboard() notcontains "trees" and clipboard() notcontains "bulbs")
stoploopif clipboard()≠""
while forever
Name=upperword(clipboard())
Notes="Transfer to "+Name
call .entry
Else TransactionType=""
TaxExempt="N"
Notes=""
Name=""
endif

*/

___ ENDPROCEDURE .transfer _____________________________________________________

___ PROCEDURE .deleterecord ____________________________________________________
yesno "Clear out this transaction?"
if clipboard() contains "yes"
    ;clear record leaving no ghost record
    Order=""
    TaxTotal=0
    Subtotal=0
    Adjtotal=0
    MemberDiscount=0
    Total=0
    Discount=0
    «$Shipping»=0
    SalesTax=0
    Paid=0
    Cash=0
    Check=0
    CreditCard=0
    Gift_Certificate=0
    TransactionType=""
    C#=0
    Group=""
    Name=""
    Notes="VOIDED TRANSACTION"
    SeedSales=0
    TreeSales=0
    OGSSales=0
    MooseSales=0
    BulbsSales=0
    BalanceDue=0
    enter=""
    deleterecord
else 
    stop
endif
___ ENDPROCEDURE .deleterecord _________________________________________________

___ PROCEDURE find customers without c# ________________________________________
select «C#» = 0 and Name <> "" and  (Staff = "Y" or Special = "Y" or TaxExempt = "Y" or «%Discount» > 0) and Group notcontains "Arbico"

___ ENDPROCEDURE find customers without c# _____________________________________

___ PROCEDURE SourceGet ________________________________________________________
local Dictionary, ProcedureList
//this saves your procedures into a variable
//step one
saveallprocedures "", Dictionary
clipboard()=Dictionary
//now you can paste those into a text editor and make your changes
STOP

//step 2
//this lets you load your changes back in from an editor and put them in
//now comment out from step one to step 2
//run the procedure one step at a time to load the list on your clipboard back in
Dictionary=clipboard()
loadallprocedures Dictionary,ProcedureList
message ProcedureList //messages which procedures got changed
___ ENDPROCEDURE SourceGet _____________________________________________________

___ PROCEDURE SourceGetGive ____________________________________________________
local Dictionary, ProcedureList

saveallprocedures "", Dictionary
clipboard()=Dictionary


/*


;exportallprocedures "", Dictionary
Dictionary=Clipboard()
importdictprocedures Dictionary, Dictionary
loadallprocedures Dictionary,ProcedureList
//this saves your procedures into a variable
//step one
saveallprocedures "", Dictionary
clipboard()=Dictionary
//now you can paste those into a text editor and make your changes
STOP

//step 2
//this lets you load your changes back in from an editor and put them in
//now comment out from step one to step 2
//run the procedure one step at a time to load the list on your clipboard back in
Dictionary=clipboard()
loadallprocedures Dictionary,ProcedureList
message ProcedureList //messages which procedures got changed

*/
___ ENDPROCEDURE SourceGetGive _________________________________________________

___ PROCEDURE .temp_lunar ______________________________________________________
local Dictionary1, ProcedureList
//this saves your procedures into a variable
exportallprocedures "", Dictionary1
clipboard()=Dictionary1

message "Macros are saved to your clipboard!"
___ ENDPROCEDURE .temp_lunar ___________________________________________________

___ PROCEDURE testtt ___________________________________________________________
    if today() >= date("1/30/23") and today() <= date("2/3/23")
        «%Discount»=«%Discount»+.05
        Notes=?(Notes notcontains "customer appreciation", Notes+¶+"Customer Appreciation Discount of 5% has been added to any other 
        volume discounts", Notes)
    endif
___ ENDPROCEDURE testtt ________________________________________________________
