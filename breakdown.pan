/* 
User types in a name, builds this doey
*/

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

// That goes into a variable called 
ChosenOne
//____

___ PROCEDURE .recordcustomer __________________________________________________
global waswindow, mailing_list_window
waswindow=info("windowname") //45 walkin

mailing_list_window="45 mailing list:secret" //added by lunar 2-6-23

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

    // If the customer doesn't have a number, but they're at the walkin
            //let's give them one 
    case «C#»<1 
        if info("windows") contains "Customer#"
            window "Customer#"
                call newnumber
            window mailing_list_window

        else 
            message "You need to have Customer# upen to do this. Procedure stopped."
            stop
        endif 
    defaultcase «C#»>1
        select «C#»=val(extract(ChosenOne,¬,1)) //this was all that was here before. Added cases an error handling -Lunar 2-6-23
    endcase

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

displaydata str(array(ChosenOne, 2,"-"))[1,4]

str(«C#»)+¬+Group+¬+Con+"-"+City+" "+St

/*
//ChosenOne = str(«C#»)+¬+Group+¬+Con+"-"+City+" "+St//
*/
MatchNoNum:
find «Con» matchexact str(tabarray(ChosenOne, 3))[1,"-"][1,-2] //do exact name
    AND City match (str(array(ChosenOne, 2,"-"))[1,4]+"*") //match first four of the city
    AND «C#» = val(ChosenOne[1,¬][1,-2]) //match the number, which should be 0

if (not info("found"))
    message "something is wrong, I was unable to find that customer. Contact tech-support."
    stop
endif 

yesno "Is: "+arrayrange(exportline(), 1,7,¬)+" correct?"
if clipboard()="Yes"
else
    goto MatchNoNum:
endif


    window "45 mailing list:secret"

    //__No Number In Mailing list?_______
    case «C#»<1 
        MatchNoNum:
            find «Con» matchexact str(tabarray(ChosenOne, 3))[1,"-"][1,-2] //do exact name
                AND City match (str(array(ChosenOne, 2,"-"))[1,4]+"*") //match first four of the city
                AND «C#» = val(ChosenOne[1,¬][1,-2]) //match the number, which should be 0

            if (not info("found"))
                message "something is wrong, I was unable to find that customer. Contact tech-support."
                stop
            endif 

            yesno "Is: "+arrayrange(exportline(), 1,7,¬)+" correct?"
            if clipboard()="Yes"
                goto GetNumNext
            else
                goto MatchNoNum:
            endif

        GetNumNext:
            if info("windows") contains "Customer#"
                window "Customer#"
                    call newnumber
                window mailing_list_window
                    «C#»=clipboard()
            else 
                message "You need to have Customer# upen to do this. Procedure stopped."
                stop
            endif 

    //___Normal process_____________
    defaultcase 
        select «C#»=val(extract(ChosenOne,¬,1)) //this was all that was here before. Added cases an error handling -Lunar 2-6-23
    endcase
    /*
    window "discounttable"
    call "addrecord/7"
    */
endif

stop